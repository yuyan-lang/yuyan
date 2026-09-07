// 古曰：一举立籍，诸物各传；败者可补，成者不易。今释：一个按钮完成元数据提交和逐文件上传，不等待服务端审查。
const 区 = document.createElement('section');
区.className = '即时发布';
区.innerHTML = `
  <h2>一键发布</h2><p>请先登录。提交后版本页面立即公开，文件逐个可用；上传失败可再次点击补传。服务器暂不审查材料内容。</p>
  <form id="即时表单">
    <label>包名 <input name="name" required maxlength="64"></label>
    <label>版本 <input name="version" required value="0.1.0" pattern="(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)"></label>
    <label>类型 <select name="type"><option>库</option><option>可执行文件</option></select></label>
    <label>简介 <input name="description" required maxlength="512"></label>
    <label>完整源码包 <input name="source" type="file"></label>
    <label>独立构建包 <input name="build" type="file"></label>
    <label>说明文件 <input name="readme" type="file" multiple accept=".md,.txt"></label>
    <label>生成文档目录（含 index.html）<input name="docs" type="file" webkitdirectory multiple></label>
    <label>平台运行包 <input name="runtime" type="file" multiple></label>
    <button type="submit">发布 / 重试缺失文件</button>
  </form>
  <p id="即时提示" role="status"></p><ul id="传输进度"></ul>
  <section id="版本详情" hidden><h2 id="版本标题"></h2><p id="版本说明"></p><p id="材料状态"></p>
    <button id="刷新版本" type="button">刷新文件状态</button><ul id="版本文件"></ul>
    <p>以下为上传者提供的未审查文档，运行在独立来源中。请勿在文档中输入密码或令牌。</p>
    <iframe id="包文档" title="上传者提供的包文档" sandbox="allow-scripts" referrerpolicy="no-referrer"></iframe>
  </section>
  <h2>最近发布的包</h2><ul id="版本列表"></ul><button id="更多版本" type="button" hidden>更多</button>`;
document.getElementById('正文').append(区);
const 元素 = id => document.getElementById(id), 表单 = 元素('即时表单');
let 当前版本 = location.pathname.match(/^\/release\/([a-f0-9]{32})$/)?.[1], 下页 = 0;
const 提示 = s => { 元素('即时提示').textContent = s; };
async function 求(url, options) {
  const r = await fetch(url, options), text = await r.text();
  let data; try { data = JSON.parse(text); } catch { data = { error: text }; }
  if (!r.ok) throw Error(data.error || '请求失败'); return data;
}
function 链接(text, url) { const a = document.createElement('a'); a.textContent = text; a.href = url; return a; }
async function 展示(id) {
  let data = await 求('/api/releases/' + id), files = [...data.files];
  while (data.cursor) { const page = await 求('/api/releases/' + id + '?cursor=' + encodeURIComponent(data.cursor)); files.push(...page.files); data.cursor = page.cursor; }
  当前版本 = id; 元素('版本详情').hidden = false;
  元素('版本标题').textContent = data.owner + ' / ' + data.name + ' / ' + data.version + ' · ' + data.type;
  元素('版本说明').textContent = data.description;
  const required = data.type === '可执行文件' ? ['source', 'build', 'readme', 'docs', 'runtime'] : ['source', 'build', 'readme', 'docs'];
  const missing = required.filter(k => k === 'docs' ? !files.some(f => f.path === 'docs/index.html') : !files.some(f => f.path.startsWith(k + '/')));
  元素('材料状态').textContent = missing.length ? '暂不可用：' + missing.join('、') : '各类材料已有上传（内容尚未验证，不代表材料完整或程序安全）。';
  元素('版本文件').replaceChildren();
  for (const f of files) { const li = document.createElement('li'); li.append(链接(f.path + ' · ' + f.size + ' 字节', f.url)); 元素('版本文件').append(li); }
  // 古曰：客页不入主文。今释：只使用服务端配置的独立文档 URL，绝不把上传内容插入门户 DOM。
  const docs = new URL(data.docsUrl);
  if (docs.protocol !== 'https:' || docs.hostname !== 'usercontent.yuyan-lang.org') throw Error('文档来源配置错误');
  元素('包文档').src = docs.href;
}
async function 列表() {
  const data = await 求('/api/releases?offset=' + 下页);
  for (const v of data.releases) { const li = document.createElement('li'); li.append(链接(v.owner + ' / ' + v.name + ' ' + v.version, '/release/' + v.id)); 元素('版本列表').append(li); }
  下页 = data.nextOffset; 元素('更多版本').hidden = 下页 === null;
}
表单.addEventListener('submit', async e => {
  e.preventDefault(); const controls = [...表单.elements]; controls.forEach(c => { c.disabled = true; }); 元素('传输进度').replaceChildren();
  try {
    const metadata = Object.fromEntries(['name', 'version', 'type', 'description'].map(k => [k, 表单.elements[k].value]));
    const version = await 求('/api/releases', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(metadata) });
    当前版本 = version.id; history.replaceState(null, '', version.url); 提示('版本已公开，正在上传所选文件。'); await 展示(version.id);
    let failures = 0;
    for (const kind of ['source', 'build', 'readme', 'docs', 'runtime']) {
      for (const file of 表单.elements[kind].files) {
        const path = kind === 'docs' ? file.webkitRelativePath.split('/').slice(1).join('/') : file.name;
        const item = document.createElement('li'); item.textContent = kind + '/' + path + '：上传中'; 元素('传输进度').append(item);
        try {
          if (file.size > 104857600) throw Error('单个文件不能超过 100 MiB');
          const hash = Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', await file.arrayBuffer())), n => n.toString(16).padStart(2, '0')).join('');
          await 求('/api/releases/' + version.id + '/files/' + kind + '/' + path.split('/').map(encodeURIComponent).join('/'), {
            method: 'PUT', headers: { 'X-Package-Size': String(file.size), 'X-Package-SHA256': hash }, body: file,
          });
          item.textContent = kind + '/' + path + '：已上传';
        } catch (error) { failures++; item.textContent = kind + '/' + path + '：' + error.message; }
      }
    }
    await 展示(version.id); 提示(failures ? '部分上传失败；保持元数据不变，重新点击即可补传，已上传文件不会覆盖。' : '上传完成。版本已公开，尚未经过内容审查。');
  } catch (error) { 提示(error.message); } finally { controls.forEach(c => { c.disabled = false; }); }
});
元素('刷新版本').addEventListener('click', () => 展示(当前版本).catch(e => 提示(e.message)));
元素('更多版本').addEventListener('click', () => 列表().catch(e => 提示(e.message)));
列表().catch(e => 提示(e.message));
if (当前版本) 展示(当前版本).catch(e => 提示(e.message));
