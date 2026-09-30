// 文言：载二客器而启豫言，外壳不行应用之法。汉语：加载应用和值桥 Wasm，启动豫言程序。
import {创建浏览器宿主} from './宿主.mjs';
import {核对接口装载} from './接口核对.mjs';

// 文言：先验浏览器具客器所需之能（WasmGC、JSPI）；不具则蒙全页以示，页首之栏亦蒙焉。告末有“我明白”，按之则撤告而续览，功能不全；不记其按，每启一页皆复示。汉语：启动前检查浏览器是否支持所需 WebAssembly 功能（WasmGC、JSPI）；检查期间显示“正在检查浏览器”，不支持则显示覆盖整页（含页首导航栏）的提示。提示末尾有“我明白”按钮：点击后关闭提示、继续访问（功能不全）；不记住关闭，每次打开或刷新页面都重新提示。
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
const 显示不支持 = 缺 => {
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
export function 检查浏览器支持() {
  const 缺 = [];
  if (typeof WebAssembly !== 'object') 缺.push('WebAssembly');
  else {
    if (!支持WasmGC()) 缺.push('WasmGC');
    if (!支持JSPI()) 缺.push('JSPI');
  }
  return 缺;
}

export async function 启动豫言浏览器应用(选项 = {}) {
  const 缺 = 检查浏览器支持();
  if (缺.length) {
    显示不支持(缺.join('、'));
    throw Error('浏览器缺少豫言所需功能：' + 缺.join('、'));
  }
  // 文言：载编逾四百毫秒乃示“正在检查”，免常页一闪。汉语：下载与编译超过 400 毫秒才显示“正在检查浏览器”，避免正常页面闪烁。
  const 计时 = typeof document === 'undefined' ? null
    : setTimeout(() => 显示检查层('<p data-yy-style="margin:0;font-size:16px;color:inherit">正在检查浏览器…</p>'), 400);
  const 撤层 = () => { clearTimeout(计时); if (typeof document !== 'undefined') document.getElementById(检查层标识)?.remove(); };
  try {
    const 结果 = await 启动实际(选项);
    撤层();
    return 结果;
  } catch (错) {
    // 文言：客器不能编，亦示过旧之告。汉语：编译失败（CompileError）也视为浏览器过旧。
    clearTimeout(计时);
    if (错 instanceof WebAssembly.CompileError)
      显示不支持('WebAssembly 功能（' + String(错.message).slice(0, 120).replace(/[<>&"]/g, 字 => `&#${字.charCodeAt(0)};`) + '）');
    else 撤层();
    throw 错;
  }
}

async function 启动实际(选项) {
  const 路径 = 选项.路径 ?? new URL('.', import.meta.url);
  const [程序回应, 值桥回应, 要求回应, 提供回应] = await Promise.all([
    fetch(new URL('程序.wasm', 路径)),
    fetch(new URL('值桥.wasm', 路径)),
    fetch(new URL('接口要求组.json', 路径)),
    fetch(new URL('宿主提供组.json', 路径))
  ]);
  if (!程序回应.ok || !值桥回应.ok || !要求回应.ok || !提供回应.ok)
    throw Error('豫言浏览器程序或接口清单资源不可用');
  const [程序字节, 值桥字节, 应用要求, 宿主提供] = await Promise.all([
    程序回应.arrayBuffer(),
    值桥回应.arrayBuffer(),
    要求回应.json(),
    提供回应.json()
  ]);
  const [程序模块, 值桥模块] = await Promise.all([
    WebAssembly.compile(程序字节),
    WebAssembly.compile(值桥字节)
  ]);
  核对接口装载({程序模块, 程序字节, 应用要求, 宿主提供, 宿主: '浏览器'});
  return 创建浏览器宿主({程序模块, 值桥模块, ...选项, 路径});
}
