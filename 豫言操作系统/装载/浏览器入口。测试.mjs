// 文言：以真客器及文树验浏览器载器，坏簿不得先行客。
// 汉语：通过真实浏览器入口加载首页 Wasm，并验证错误清单在应用启动前被拒绝。
import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {dirname, resolve} from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';
import test from 'node:test';

const 根 = dirname(fileURLToPath(import.meta.url));
const 云 = resolve(根, '../../../yuyan-cloud');
const 产物 = resolve(云, 'dist/网站/资源/首页应用');
const 网站包 = resolve(云, '网站/package.json');
const 可运行 = existsSync(resolve(产物, '入口.mjs')) &&
  existsSync(resolve(云, '网站/node_modules/jsdom/package.json'));

test('浏览器通用入口先核接口再运行首页', {skip: !可运行}, async () => {
  const {JSDOM} = createRequire(网站包)('jsdom');
  const {启动豫言浏览器应用} = await import(pathToFileURL(resolve(产物, '入口.mjs')).href);
  const 页 = new JSDOM('<!doctype html><html lang="zh-CN"><body><button id="示例你好"></button><button id="示例函数"></button><button id="示例匹配"></button><pre id="首页示例"></pre><output id="示例输出"></output><p id="示例解释"></p><a id="运行示例"></a><button id="复制示例"></button></body></html>', {url: 'https://yuyan-lang.org/'});
  const 原取 = globalThis.fetch;
  const 路径 = pathToFileURL(产物 + '/');
  let 坏簿 = false;
  globalThis.fetch = async 网址 => {
    const 名 = fileURLToPath(网址);
    let 字节 = readFileSync(名);
    if (坏簿 && 名.endsWith('接口要求组.json')) {
      const 清单 = JSON.parse(字节.toString('utf8'));
      清单[0].函数[0].签名 = '→[「 有 」；「 整数 」]';
      字节 = Buffer.from(JSON.stringify(清单));
    }
    return new Response(字节, {status: 200});
  };
  try {
    const 宿主 = await 启动豫言浏览器应用({路径, 根: 页.window.document,
      储存: 页.window.localStorage, 全局: {document: 页.window.document,
        navigator: 页.window.navigator, encodeURIComponent}});
    try {
      页.window.document.getElementById('示例函数').click();
      for (let 次 = 0; 次 < 100 && !页.window.document.getElementById('首页示例').textContent.includes('「加一」'); 次++)
        await new Promise(完 => setTimeout(完, 10));
      assert.match(页.window.document.getElementById('首页示例').textContent, /「加一」/u);
    } finally {
      宿主.关闭();
      await 宿主.完成;
    }
    坏簿 = true;
    await assert.rejects(启动豫言浏览器应用({路径, 根: 页.window.document,
      储存: 页.window.localStorage, 全局: {document: 页.window.document,
        navigator: 页.window.navigator, encodeURIComponent}}), /接口规范或签名不一致/u);
  } finally {
    globalThis.fetch = 原取;
    页.window.close();
  }
});
