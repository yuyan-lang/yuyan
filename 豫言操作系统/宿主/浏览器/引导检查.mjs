// 文言：豫言操作系统浏览器之引导检查，诸客器未启之先验之：浏览器具 WasmGC、JSPI 否；不具则蒙全页以示，页首之栏亦蒙焉，验时无所示。告末有“我明白”，按之则撤告而续览，功能不全；不记其按，每启一页皆复示。此篇不随诸客器抄录，官网布之于 /豫言操作系统/浏览器/引导检查.mjs，全站诸入口同域取之，改之惟布官网。
// 汉语：豫言操作系统浏览器版的引导检查，在启动任何豫言 Wasm 之前运行：检查浏览器是否支持 WasmGC 与 JSPI；检查期间不显示任何提示，不支持时显示覆盖整页（含页首导航栏）的“版本过旧”提示。提示末尾有“我明白”按钮：点击后关闭提示、继续访问（功能不全）；不记住关闭，每次打开或刷新页面都重新提示。本文件不随各应用复制，由官网发布在 /豫言操作系统/浏览器/引导检查.mjs，全站各应用的 入口.mjs 都从这个同域地址导入；改检查或提示只需发布官网。
const 检查层标识 = '豫言浏览器检查';
const 支持WasmGC = () => {
  try { return WebAssembly.validate(new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0, 1, 3, 1, 0x5f, 0])); } catch { return false; }
};
const 支持JSPI = () => typeof WebAssembly?.Suspending === 'function' && typeof WebAssembly?.promising === 'function';
const 显示检查层 = 文 => {
  if (typeof document === 'undefined' || !document.body) return null;
  let 层 = document.getElementById(检查层标识);
  if (!层) {
    层 = document.createElement('div');
    层.id = 检查层标识;
    层.setAttribute('role', 'alertdialog');
    层.setAttribute('aria-modal', 'true');
    层.style.cssText = 'position:fixed;top:0;right:0;bottom:0;left:0;z-index:2147483647;display:flex;align-items:center;justify-content:center;padding:16px;background:rgba(245,242,235,.97);color:#1d1b18;font:16px/1.7 system-ui,sans-serif;';
    document.body.appendChild(层);
  }
  层.innerHTML = `<div data-yy-style="max-width:32em;text-align:center">${文}</div>`;
  // 文言：页或以 CSP 禁行内之式，故写入之后以 CSSOM 施之。汉语：页面的 CSP 可能禁止 style 属性（如试写间、云工作台的 style-src 'self'），所以写入后再用 CSSOM 逐个设置样式。
  for (const 元 of 层.querySelectorAll('[data-yy-style]')) 元.style.cssText = 元.getAttribute('data-yy-style');
  return 层;
};

// 文言：示过旧之告；缺者之名须先转义。汉语：显示“版本过旧”提示；「缺」会写进 HTML，调用方须先转义。
export const 显示不支持 = 缺 => {
  const 层 = 显示检查层(
    '<h2 data-yy-style="margin:0 0 .5em;font-size:24px;font-weight:700;line-height:1.7;color:inherit;letter-spacing:normal;text-transform:none">您的浏览器版本过旧，无法运行本页面</h2>'
    + '<p data-yy-style="margin:0 0 .5em;font-size:16px;color:inherit">请将浏览器更新到 Safari 27、Chrome 137、Edge 137 或 Firefox 153 及以上版本后再访问。</p>'
    + `<p data-yy-style="margin:0 0 1em;font-size:13px;opacity:.7;color:inherit">缺少：${缺}</p>`
    + '<p data-yy-style="margin:0 0 .75em;font-size:16px;color:inherit">您也可以继续访问，但部分功能将无法使用。</p>'
    + '<button type="button" data-yy-style="font:inherit;padding:9px 24px;border:0;border-radius:3px;background:var(--豫朱,#a33c2c);color:#fff;cursor:pointer">我明白</button>');
  // 文言：钮依站之主钮（朱底白字）；不自移焦点，免站之焦点框环于钮外。汉语：按钮照站点主按钮样式（朱红底、白字）；不主动把焦点移到按钮上，免得站点的焦点框（朱红描边）套在按钮外面。
  层?.querySelector('button')?.addEventListener('click', () => 层.remove());
  return 层;
};

// 文言：还所缺之能之名；皆具则空。汉语：返回缺少的功能名列表；都支持时为空列表。
export function 检查浏览器支持() {
  const 缺 = [];
  if (typeof WebAssembly !== 'object') 缺.push('WebAssembly');
  else {
    if (!支持WasmGC()) 缺.push('WasmGC');
    if (!支持JSPI()) 缺.push('JSPI');
  }
  return 缺;
}
