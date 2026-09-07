import {test} from 'node:test';import assert from 'node:assert/strict';import {网页资源地址} from '../源码/网页路由.js';
import {readFileSync} from 'node:fs';
// 文言：借首页之文，不易版本之址。汉语：详情页映射静态 HTML 时不得触发托管平台重定向。
test('版本详情保留编号，静态托管关闭 HTML 地址改写',()=>{
  const url=new URL('https://packages.yuyan-lang.org/release/'+'a'.repeat(32));
  assert.equal(decodeURIComponent(网页资源地址(new Request(url)).pathname),'/首页.html');
  assert.match(readFileSync(new URL('../wrangler.jsonc',import.meta.url),'utf8'),/"html_handling":\s*"none"/);
});
// 古曰：增其门，不改旧径。今释：新页面不能截获既有 API 或包文件请求。
test('HTML 首页与静态资源可用，原 API 路径保持原样',()=>{const b='https://包管理.yuyan-lang.org';assert.equal(decodeURIComponent(网页资源地址(new Request(b+'/',{headers:{Accept:'text/html'}})).pathname),'/首页.html');for(const p of ['/','/health','/parser-core-2.0.zip','/upload/prepare/包-1.zip'])assert.equal(网页资源地址(new Request(b+p)),null);assert.equal(网页资源地址(new Request(b+'/',{method:'POST',headers:{Accept:'text/html'}})),null);assert.equal(网页资源地址(new Request(b+'/%ZZ')),null);assert.ok(网页资源地址(new Request(b+'/共用/主题.css')));});

test('市场与个人页分开，保留公开版本详情和邮件脚本',()=>{
 const b='https://packages.yuyan-lang.org';assert.equal(decodeURIComponent(网页资源地址(new Request(b+'/个人')).pathname),'/个人.html');assert.ok(网页资源地址(new Request(b+'/市场.mjs')));
 const market=readFileSync(new URL('../网页/首页.html',import.meta.url),'utf8'),personal=readFileSync(new URL('../网页/个人.html',import.meta.url),'utf8');assert.ok(!market.includes('id="账户表单"'));assert.ok(market.includes('id="市场列表"'));assert.ok(personal.includes('id="账户表单"'));assert.ok(!personal.includes('id="包查询"'));
});
