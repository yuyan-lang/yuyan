// 汉语：同步载入共享工厂，验证完整文件链与显式目录授权。文言：同步载共工，验全文件之链与明示目录之授。
import test from 'node:test';
import assert from 'node:assert/strict';
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {createRequire} from 'node:module';
const {创建文件能力, 提取文件授权} = createRequire(import.meta.url)('./文件能力.mjs');
const 文 = 值 => new TextEncoder().encode(值);
test('共享文件工厂同步载入并沿用显式授权', () => {
  const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy文件能-'));
  文件系统.writeFileSync(路径.join(根, '文'), '甲乙');
  const 配 = 提取文件授权(['程序.wasm', '--授权只读目录', '读=' + 根, '--授权目录', '写=' + 根, '客参'], 根);
  assert.deepEqual(配.参数, ['程序.wasm', '客参']);
  assert.throws(() => 提取文件授权(['--授权目录'], 根), /缺少值/u);
  assert.throws(() => 提取文件授权(['--授权目录', '坏'], 根), /名=路径/u);
  const 文件能 = 创建文件能力({授权: 配.授权}), 实 = 文件能.实现;
  try {
    for (const 名 of ['取得目录', '打开', '开启写入', '读取', '定位读取', '关闭', '写入', '列目录', '查询信息', '创建目录', '删除']) assert.equal(typeof 实['诺节文件' + 名], 'function', 名);
    assert.equal(实.诺节文件取得目录(文('未授'))[0], 1);
    const 读 = 文(实.诺节文件取得目录(文('读'))[1]);
    assert.equal(实.诺节文件打开(读, 文('../越界'), false)[0], 7);
    assert.equal(实.诺节文件打开(读, 文('文'), true)[0], 1);
    const 柄 = 文(实.诺节文件打开(读, 文('文'), false)[1]);
    assert.deepEqual(Array.from(实.诺节文件读取(柄, 1n)[1]), [0xe7]);
    assert.equal(实.诺节文件定位读取(柄, -1n, 1n)[0], 7);
    assert.equal(实.诺节文件定位读取(柄, 0n, 0n)[1].length, 0);
    assert.equal(实.诺节文件关闭(柄)[0], 0);
    assert.equal(实.诺节文件读取(柄, 1n)[0], 3);
    const 写 = 文(实.诺节文件取得目录(文('写'))[1]);
    const 出 = 文(实.诺节文件开启写入(写, 文('新文'), false)[1]);
    assert.equal(实.诺节文件写入(出, 文('新'))[0], 0);
    assert.equal(实.诺节文件关闭(出)[0], 0);
    assert.equal(文件系统.readFileSync(路径.join(根, '新文'), 'utf8'), '新');
    assert.equal(实.诺节文件删除(写, 文('新文'))[0], 0);
  } finally { 文件能.清理(); 文件系统.rmSync(根, {recursive: true, force: true}); }
  const 未授 = 创建文件能力({授权: {目录: new Map()}});
  assert.equal(未授.实现.诺节文件取得目录(文('读'))[0], 1);
  未授.清理();
});
