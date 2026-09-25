// 文言：异源、改址、巨文与坏字皆拒，正径则得原文。
// 汉语：验证网页资源能力的同源、无重定向、大小和严格 UTF-8 边界。
import assert from 'node:assert/strict';
import test from 'node:test';
import {读取同源资源文字} from './宿主.mjs';

const 基址 = 'https://yuyan-lang.org/products/';

test('同源资源只发无重定向 GET 并返回 UTF-8 文字', async () => {
  let 调用 = 0;
  const 文 = await 读取同源资源文字('/products/目录.json', 基址, async (网址, 选项) => {
    调用++;
    assert.equal(网址, 'https://yuyan-lang.org/products/%E7%9B%AE%E5%BD%95.json');
    assert.deepEqual(选项, {method: 'GET', redirect: 'error', credentials: 'same-origin'});
    return new Response('豫言');
  });
  assert.equal(调用, 1);
  assert.equal(文, '豫言');
});

test('跨站形式及重定向均在读取前拒绝', async () => {
  let 调用 = 0;
  const 网络 = async () => { 调用++; return Response.redirect('https://elsewhere.example/'); };
  for (const 径 of ['//elsewhere.example/', '/\\elsewhere.example/', 'https://elsewhere.example/'])
    await assert.rejects(读取同源资源文字(径, 基址, 网络), /路径无效/u);
  assert.equal(调用, 0);
  await assert.rejects(读取同源资源文字('/redirect', 基址, 网络), /资源不可用/u);
  assert.equal(调用, 1);
});

test('超过二 MiB 或 UTF-8 无效时拒绝', async () => {
  await assert.rejects(读取同源资源文字('/large', 基址,
    async () => new Response(new Uint8Array(2 * 1024 * 1024 + 1))), /超过二 MiB/u);
  await assert.rejects(读取同源资源文字('/bad', 基址,
    async () => new Response(new Uint8Array([0xff]))), /encoded data|encoding|UTF-8/iu);
});
