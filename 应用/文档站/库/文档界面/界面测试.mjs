// 文言：离线之文不系云府，客文不妄易。汉语：验证本地文档独立导航、禁用存储时切换语言，以及源码保持原文。
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
const { JSDOM } = await import(process.env.YY_DOM_MODULE || 'jsdom');
const 脚本 = await readFile(new URL('./界面.js', import.meta.url), 'utf8');
for (const 类型 of ['docs', 'source']) {
  test(`${类型} 离线阅读不依赖官网并保护原文`, () => {
    const 页 = new JSDOM(`<body><div data-yuyan-site="${类型}"></div><main><p data-han="现代说明" data-wen="古辞">现代说明</p><pre>公开名称</pre><p>公开名称</p></main></body>`, { url: 'https://example.invalid/index.html', runScripts: 'outside-only' });
    const 文 = 页.window.document;
    Object.defineProperty(文, 'cookie', { get() { throw Error('禁止存储'); }, set() { throw Error('禁止存储'); } });
    页.window.history.replaceState = () => { throw Error('禁止历史'); };
    assert.doesNotThrow(() => 页.window.eval(脚本));
    页.window.豫言界面.设置语言('wen');
    assert.equal(文.documentElement.lang, 'lzh');
    assert.equal(文.querySelector('[data-han]').textContent, '源码、类型签名与原注皆存其本，无独立文言译本者不妄改之。');
    assert.equal(文.querySelector('main [data-han]').textContent, '古辞');
    assert.equal(文.querySelector('pre').textContent, '公开名称');
    assert.equal(文.querySelector('main p:last-child').textContent, '公开之名');
    assert.ok([...文.querySelectorAll('a')].every(链 => new URL(链.href).hostname === 'example.invalid'));
    页.window.豫言界面.设置语言('han');
    assert.equal(文.querySelector('main p:last-child').textContent, '公开名称');
    页.window.close();
  });
}
