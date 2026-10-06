// 汉语：验证授权、空行、结束、单候者、关闭和晚到结果；实际Wasm适配另验。文言：验授、空行、尽、单候、闭及后至之果；实Wasm适配别验。
import test from 'node:test';
import assert from 'node:assert/strict';
import {创建控制台能力} from './控制台.mjs';

test('控制台区分空行与输入结束并原样输出', async () => {
  const 行们 = [[true, '\n'], [true, '中文\r\n'], [false, '']]; let 输出 = '';
  const 能 = 创建控制台能力(new Map([['主控制台', {读取行: () => 行们.shift(), 写文本: 文 => {输出 += 文;}}]]));
  assert.deepEqual(await 能.读取('主控制台'), [0, '', '']);
  assert.deepEqual(await 能.读取('主控制台'), [0, '中文', '']);
  assert.deepEqual(await 能.读取('主控制台'), [4, '', '']);
  assert.deepEqual(await 能.写入('主控制台', '甲\n乙'), [0, '', '']);
  assert.equal(输出, '甲\n乙'); 能.关闭();
});
test('未授权不触发回调，回调错误保留诊断', async () => {
  const 能 = 创建控制台能力(new Map([['失败', {读取行: () => {throw Error('读取坏');}, 写文本: () => {throw Error('输出坏');}}]]));
  assert.equal((await 能.读取('无'))[0], 1);
  assert.equal((await 能.写入('无', '文'))[0], 1);
  assert.deepEqual(await 能.读取('失败'), [8, '', '读取坏']);
  assert.deepEqual(await 能.写入('失败', '文'), [8, '', '输出坏']); 能.关闭();
});
test('只允许一个读取等待，关闭结束等待，晚到结果不再进入', async () => {
  let 送达, 闭数 = 0;
  const 能 = 创建控制台能力(new Map([['主控制台', {读取行: () => new Promise(成 => {送达 = 成;}), 关闭: () => {闭数++;}}]]));
  const 待读 = 能.读取('主控制台'); await Promise.resolve();
  assert.equal((await 能.读取('主控制台'))[0], 7);
  能.关闭(); 能.关闭();
  assert.deepEqual(await 待读, [4, '', '']);
  送达([true, '晚到']); await Promise.resolve();
  assert.deepEqual(await 能.读取('主控制台'), [3, '', '']); assert.equal(闭数, 1);
});
test('不同授权控制台的等待彼此独立', async () => {
  const 送达们 = [];
  const 能 = 创建控制台能力(new Map(['甲', '乙'].map(名 => [名, {读取行: () => new Promise(成 => 送达们.push(成))}])));
  const 甲 = 能.读取('甲'), 乙 = 能.读取('乙'); await Promise.resolve();
  送达们[1]([true, '乙行']); assert.equal((await 乙)[1], '乙行');
  送达们[0]([true, '甲行']); assert.equal((await 甲)[1], '甲行'); 能.关闭();
});
