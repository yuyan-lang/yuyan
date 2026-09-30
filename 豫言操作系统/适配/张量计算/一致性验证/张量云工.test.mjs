// 文言：以真 Wasm 验张量计算之云工适配：诺节载已构建之“张量探针云工”产物，以定时之事行张量探针，所书与期望逐行同。
// 汉语：用诺节加载已构建的“张量探针云工”产物（宿主.mjs、程序.wasm、值桥.wasm、中央张量内核.wasm），以定时事件（scheduled，不需响应）运行张量探针，核对输出与 应用/张量探针/期望输出.txt 逐行相同。
// 用法见同目录说明：在构建根执行 `node --test <本文件>`，产物根目录由环境变量 YY_DIST_ROOT 指定（默认 ./dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const 产物 = pathToFileURL(path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '张量探针云工') + '/');
const {创建云工宿主} = await import(new URL('宿主.mjs', 产物));
const 编 = async 名 => WebAssembly.compile(await readFile(new URL(名, 产物)));
const [程序模块, 值桥模块, 中央张量内核模块] = await Promise.all(['程序.wasm', '值桥.wasm', '中央张量内核.wasm'].map(编));
const 期望 = (await readFile(new URL('应用/张量探针/期望输出.txt', import.meta.url), 'utf8')).trimEnd().split('\n');

// 文言：行一定时之事，收其所书。汉语：以定时事件运行一次程序，返回它打印的全部行。
const 运行探针 = async 模块 => {
  let 文 = '';
  const 宿主 = 创建云工宿主({
    程序模块, 值桥模块, 许可: {}, 中央张量内核模块: 模块, 输出: 段 => { 文 += 段; },
    执行配置: {事件时限毫秒: {scheduled: 120000}}
  });
  await 宿主.scheduled({scheduledTime: 0, cron: ''}, {}, {waitUntil() {}});
  return 文.trimEnd().split('\n');
};

// 文言：「张量探针完成」之后之多线校验一行随内核之数值而变，不入期望，惟验其式并书之，以与浏览器所出相较。
// 汉语：“张量探针完成”之后的多线程校验一行随中央张量内核的数值实现而变，不写进期望输出：这里只核对格式并打印出来，供与浏览器两次运行的同一行比较（应逐字相同）。
test('张量探针在云工宿主上的输出与期望逐行相同', async () => {
  const 行们 = await 运行探针(中央张量内核模块);
  const 完 = 行们.indexOf('张量探针完成');
  assert.deepEqual(行们.slice(0, 完 + 1), 期望);
  assert.match(行们[完 + 1] ?? '', /^张量·多线程校验：\d+(,\d+){6}$/u);
  console.log(行们[完 + 1]);
});

test('构建产物没有中央张量内核模块时取得上下文返回资源暂不可用', async () => {
  const 行们 = await 运行探针(null);
  assert.deepEqual(行们, ['张量·取得上下文：资源暂不可用', '张量探针完成', '张量·多线程校验：资源暂不可用']);
});
