import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { webcrypto } from 'node:crypto';
import { JSDOM } from 'jsdom';

// 古曰：字当作字，客文居框；传败可补。今释：在 DOM 中验证一键提交、独立失败与安全文本渲染。
const id = 'a'.repeat(32), origin = 'https://packages.yuyan-lang.org';
const code = readFileSync(new URL('../网页/发布.mjs', import.meta.url), 'utf8');
const tick = () => new Promise(resolve => setTimeout(resolve, 20));
function 网页() {
  const dom = new JSDOM('<main id="正文"></main>', { url: origin, runScripts: 'outside-only' });
  const { window: w } = dom, calls = [], files = [];
  let fail = true, created = false;
  w.crypto.subtle = webcrypto.subtle;
  w.fetch = async (url, options = {}) => {
    calls.push({ url, options });
    if (url === '/api/releases' && options.method === 'POST') { created = true; return Response.json({ id, url: '/release/' + id }, { status: 201 }); }
    if (options.method === 'PUT') {
      assert.equal(created, true);
      if (url.includes('/build/') && fail) return Response.json({ error: '模拟上传失败' }, { status: 500 });
      const path = decodeURIComponent(url.split('/files/')[1]);
      if (!files.some(f => f.path === path)) files.push({ path, size: 3, url });
      return Response.json({ ok: true });
    }
    if (url.startsWith('/api/releases?')) return Response.json({ releases: [], nextOffset: null });
    return Response.json({ id, owner: '豫言', name: '<img src=x onerror=alert(1)>', version: '0.1.0',
      type: '可执行文件', description: '<script>alert(1)</script>', files,
      docsUrl: 'https://usercontent.yuyan-lang.org/' + id + '/index.html', cursor: null });
  };
  w.eval(code);
  return { dom, w, calls, files, succeed() { fail = false; } };
}
test('一个点击先公开版本，再逐文件上传；部分失败不阻断其他文件，重试使用同一版本', async () => {
  const { dom, w, calls, succeed } = 网页();
  const form = w.document.getElementById('即时表单');
  form.elements.name.value = '例包'; form.elements.description.value = '说明';
  for (const kind of ['source', 'build', 'docs']) {
    const file = { name: kind === 'docs' ? 'index.html' : kind + '.zip', size: 3,
      webkitRelativePath: '文档/index.html', async arrayBuffer() { return new TextEncoder().encode('abc').buffer; } };
    Object.defineProperty(form.elements[kind], 'files', { value: [file] });
  }
  form.dispatchEvent(new w.Event('submit', { cancelable: true })); await tick(); await tick();
  assert.equal(w.location.pathname, '/release/' + id);
  assert.match(w.document.getElementById('即时提示').textContent, /部分上传失败/);
  assert.equal(calls.filter(c => c.options.method === 'PUT').length, 3);
  assert.equal(w.document.querySelector('#版本标题 img'), null);
  assert.match(w.document.getElementById('版本标题').textContent, /<img/);
  const iframe = w.document.getElementById('包文档');
  assert.equal(iframe.getAttribute('sandbox'), 'allow-scripts');
  assert.equal(iframe.getAttribute('referrerpolicy'), 'no-referrer');
  assert.match(iframe.src, /^https:\/\/usercontent\.yuyan-lang\.org\//);
  assert.equal(iframe.hasAttribute('srcdoc'), false);
  succeed(); form.dispatchEvent(new w.Event('submit', { cancelable: true })); await tick(); await tick();
  assert.match(w.document.getElementById('即时提示').textContent, /上传完成/);
  assert.equal(calls.some(c => /prepare|validate|complete/.test(c.url)), false);
  dom.window.close();
});
