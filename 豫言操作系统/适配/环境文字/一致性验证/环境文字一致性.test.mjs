// 环境文字 0.2.0 一致性测试：真实 Wasm + Node 宿主。
// 需要宿主原语 豫言_云工_授权绑定存在（见交付报告中的宿主补丁）；缺少时本测试直接报错说明。
// 复跑：在私有暂存目录（含 dist/）里 `node --test <本文件>`；产物位置可用环境变量 产物根 指定（默认 <当前目录>/dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {pathToFileURL} from 'node:url';
import path from 'node:path';

const 产物 = path.resolve(process.env.产物根 ?? path.join(process.cwd(), 'dist'), '环境文字一致性');
const 宿主源码 = await readFile(path.join(产物, '宿主.mjs'), 'utf8');
if (!宿主源码.includes('豫言_云工_授权绑定存在')) throw new Error('宿主缺少原语 豫言_云工_授权绑定存在：请先按报告落实宿主补丁');
const {创建云工宿主} = await import(pathToFileURL(path.join(产物, '宿主.mjs')).href);
const 程序模块 = await WebAssembly.compile(await readFile(path.join(产物, '程序.wasm')));
const 值桥模块 = await WebAssembly.compile(await readFile(path.join(产物, '值桥.wasm')));
const 许可 = JSON.parse(await readFile(path.join(产物, '许可.json'), 'utf8'));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可});
const 上下文 = {waitUntil(承诺) { Promise.resolve(承诺).catch(() => {}); }};
class 资源桩 {}
const 环境 = {PRESENT: '值✓ 中文', MULTI: '甲\n乙\t丙', EMPTY: '', OBJ: new 资源桩(), NUM: 5, ZERO: 0, DB: new 资源桩(), SVC: new 资源桩(), BUCKET: new 资源桩(), OTHER: '不在许可中', UNLISTED: new 资源桩()};

async function 调(路径, 环境对象 = 环境) {
  const 回 = await 宿主.fetch(new Request('https://x.test' + 路径), 环境对象, 上下文);
  return {状态: 回.status, 文: await 回.text()};
}
const 文本 = 果 => { assert.equal(果.状态, 200, 果.文); return Buffer.from(果.文, 'hex').toString('utf8'); };
const 错 = 果 => { assert.equal(果.状态, 400, '应得到可捕获的失败：' + 果.文); assert.ok(果.文.startsWith('err|')); return Buffer.from(果.文.slice(4), 'hex').toString('utf8'); };

test('读取授权环境文字：已授权且存在返回原文，已授权但缺失或为空返回空串', async () => {
  assert.equal(文本(await 调('/env?name=PRESENT')), '值✓ 中文');
  assert.equal(文本(await 调('/env?name=MULTI')), '甲\n乙\t丙');
  assert.equal(文本(await 调('/env?name=EMPTY')), '');
  assert.equal(文本(await 调('/env?name=MISSING')), '', '已授权而 env 上不存在：返回空串而不是宿主异常');
  assert.equal(文本(await 调('/env?name=ZERO')), '', '假值视为缺失');
  assert.equal(文本(await 调('/env?name=PRESENT', {})), '', 'env 全空');
});

test('读取授权环境文字：未授权名与非文字值仍失败（部署错误，宿主异常）', async () => {
  await assert.rejects(调('/env?name=OTHER'), /未授权的ENV绑定：OTHER/);
  await assert.rejects(调('/env?name=NOT_LISTED'), /未授权的ENV绑定：NOT_LISTED/);
  await assert.rejects(调('/env?name=OBJ'), /环境变量不是文字/);
  await assert.rejects(调('/env?name=NUM'), /环境变量不是文字/);
});

test('授权绑定存在：已授权且真值为阳，已授权但缺失为阴，未授权抛错', async () => {
  const 问 = (种类, 名, 环境对象) => 调('/bind?kind=' + encodeURIComponent(种类) + '&name=' + encodeURIComponent(名), 环境对象);
  assert.equal((await 问('D1', 'DB')).文, '阳');
  assert.equal((await 问('D1', 'NODB')).文, '阴');
  assert.equal((await 问('SERVICE', 'SVC')).文, '阳');
  assert.equal((await 问('ASSETS', 'ASSETS')).文, '阴');
  assert.equal((await 问('ASSETS', 'ASSETS', {ASSETS: new 资源桩()})).文, '阳');
  assert.equal((await 问('R2', 'BUCKET')).文, '阳');
  assert.equal((await 问('ENV', 'PRESENT')).文, '阳');
  assert.equal((await 问('ENV', 'EMPTY')).文, '阴');
  assert.equal((await 问('ENV', 'MISSING')).文, '阴');
  assert.equal((await 问('ENV', 'OBJ')).文, '阳');
  assert.equal((await 问('D1', 'DB', {})).文, '阴');
  await assert.rejects(问('D1', 'UNLISTED'), /未授权的D1绑定：UNLISTED/);
  await assert.rejects(问('KV', 'DB'), /未授权的KV绑定：DB/);
  await assert.rejects(问('QUEUE', 'Q'), /未授权的QUEUE绑定：Q/);
  assert.match(错(await 问('', 'DB')), /绑定种类为空/);
  assert.match(错(await 问('D1', '')), /绑定名称为空/);
});
