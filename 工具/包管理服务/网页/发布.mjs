// 文言：一档立籍，主名须合；败者同档补之。汉语：单 ZIP 上传并从包声明填充元数据，基础检查通过后立即公开。
const 区 = document.createElement('section');
区.className = '即时发布';
区.innerHTML = `
  <h2 data-han="所有者与发布" data-wen="定主而发布">所有者与发布</h2>
  <p id="所有者状态"></p><button id="设置所有者" type="button">设置所有者名称</button>
  <dialog id="所有者对话框"><form id="所有者表单">
    <h3>绑定所有者名称</h3><p>每个账户一个全站唯一名称，绑定后暂不可修改。这不是身份认证。</p>
    <label>所有者名称 <input name="owner" required maxlength="64" autocomplete="off"></label>
    <p>允许 1–32 个汉字、小写字母、数字、下划线或短横线，不能以“用户-”开头。</p>
    <button type="submit">确认绑定</button><button id="取消所有者" type="button">取消</button><p id="所有者提示" role="status"></p>
  </form></dialog>
  <p>验证邮箱并绑定名称后，上传一个完整 ZIP。包声明检查通过即公开，材料展开失败可重传同一 ZIP 补齐；不执行源码或构建脚本。</p>
  <p>ZIP 根目录：源码/（直接包含唯一的 。包。豫 和双语说明）、文档/index.html、构建/；可执行包还需要运行/。</p>
  <form id="即时表单">
    <label>完整发布 ZIP（最多 16 MiB）<input name="archive" type="file" required accept=".zip,application/zip"></label>
    <button type="submit">上传并发布 / 重试</button>
  </form>
  <p id="即时提示" role="status"></p><ul id="传输进度"></ul>
  <section id="版本详情" hidden><h2 id="版本标题"></h2><p id="版本说明"></p><p id="材料状态"></p>
    <button id="刷新版本" type="button">刷新文件状态</button><ul id="版本文件"></ul>
    <p>以下为上传者提供的未审查文档，运行在独立来源中。请勿在文档中输入密码或令牌。</p>
    <iframe id="包文档" title="上传者提供的包文档" sandbox="allow-scripts" referrerpolicy="no-referrer"></iframe>
  </section>
  <h2 data-han="我的包" data-wen="吾包">我的包</h2><ul id="版本列表"></ul><button id="更多版本" type="button" hidden>更多</button>`;
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
  const required = data.type === '可执行文件' ? ['source', 'build', 'docs', 'runtime', 'archive'] : ['source', 'build', 'docs', 'archive'];
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
  const data = await 求('/api/releases?mine=1&offset=' + 下页);
  for (const v of data.releases) { const li = document.createElement('li'); li.append(链接(v.owner + ' / ' + v.name + ' ' + v.version, '/release/' + v.id)); 元素('版本列表').append(li); }
  下页 = data.nextOffset; 元素('更多版本').hidden = 下页 === null;
}
表单.addEventListener('submit', async e => {
  e.preventDefault(); const controls = [...表单.elements]; controls.forEach(c => { c.disabled = true; }); 元素('传输进度').replaceChildren();
  try {
    const { user } = await 求('/api/account/session');
    if (!user) throw Error('请先登录');
    if (!user.emailVerified) throw Error('请先验证邮箱');
    if (!user.ownerBound) { 元素('所有者对话框').showModal(); throw Error('请先绑定所有者名称'); }
    const file = 表单.elements.archive.files[0];
    if (!file || file.size > 16777216) throw Error('请选择不超过 16 MiB 的 ZIP');
    提示('正在上传并检查包声明，随后展开材料…');
    const version = await 求('/api/releases/zip', { method: 'POST', headers: { 'Content-Type': 'application/zip' }, body: file });
    当前版本 = version.id; history.replaceState(null, '', '/个人'); await 展示(version.id);await 刷所有者();
    提示(version.incomplete ? '版本已公开，但部分材料暂不可获取；请重传同一 ZIP 补齐。' : '发布完成。包声明与所有者已核对，材料内容尚未审查。');
  } catch (error) { 提示(error.message); } finally { controls.forEach(c => { c.disabled = false; }); }
});
元素('刷新版本').addEventListener('click', () => 展示(当前版本).catch(e => 提示(e.message)));
元素('更多版本').addEventListener('click', () => 列表().catch(e => 提示(e.message)));

if (当前版本) 展示(当前版本).catch(e => 提示(e.message));

async function 刷所有者() {
  const {user}=await 求('/api/account/session');
  表单.hidden=!user;下页=0;元素('版本列表').replaceChildren();元素('更多版本').hidden=true;if(user)await 列表();
  元素('所有者状态').textContent=user ? (user.ownerBound ? '所有者：'+user.name : '尚未绑定所有者名称') : '请先登录账户。';
  元素('设置所有者').hidden=!user || !!user.ownerBound;
}
元素('设置所有者').addEventListener('click',()=>元素('所有者对话框').showModal());
元素('取消所有者').addEventListener('click',()=>元素('所有者对话框').close());
元素('所有者表单').addEventListener('submit',async e=>{
  e.preventDefault();const form=e.currentTarget,button=form.querySelector('button[type="submit"]');button.disabled=true;
  try {
    await 求('/api/account/owner',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({name:form.elements.owner.value})});
    元素('所有者对话框').close();await 刷所有者();
  } catch(error){元素('所有者提示').textContent=error.message;}finally{button.disabled=false;}
});
window.addEventListener('账户变化',()=>刷所有者().catch(e=>提示(e.message)));
window.addEventListener('focus',()=>刷所有者().catch(e=>提示(e.message)));
刷所有者().catch(e=>提示(e.message));
