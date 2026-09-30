// 文言：载二客器而启豫言，外壳不行应用之法。汉语：加载应用和值桥 Wasm，启动豫言程序。
import {创建浏览器宿主} from './宿主.mjs';
import {核对接口装载} from './接口核对.mjs';

// 文言：先验浏览器具客器所需之能（WasmGC、JSPI）；不具则蒙全页以示，惟留页首之栏。告末有“我明白”，按之则撤告而强行续览，功能不全；同一标签之后诸页不复示。汉语：启动前检查浏览器是否支持所需 WebAssembly 功能（WasmGC、JSPI）；检查期间显示“正在检查浏览器”，不支持则显示全页提示，页首导航栏仍可点击。提示末尾有“我明白”按钮：点击后关闭提示、强行继续访问（功能不全），同一标签页之后的页面不再提示。
const 检查层标识 = '豫言浏览器检查';
const 知晓键 = '豫言浏览器检查已知晓';
const 已知晓 = () => { try { return sessionStorage.getItem(知晓键) === '1'; } catch { return false; } };
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
    // 待办事项：文言：顶距惟初建时一量，视口后变（如转屏）则不随。汉语：顶边只在创建时量一次，之后视口变化（如手机转屏）不会跟着调整。
    const 首 = document.querySelector('body > header');
    const 顶 = 首 ? Math.max(0, 首.getBoundingClientRect().bottom) : 0;
    层.style.cssText = `position:fixed;left:0;right:0;bottom:0;top:${顶}px;z-index:2147483647;display:flex;align-items:center;justify-content:center;padding:16px;background:rgba(245,242,235,.97);color:#1d1b18;font:16px/1.7 system-ui,sans-serif;`;
    document.body.appendChild(层);
  }
  层.innerHTML = `<div style="max-width:32em;text-align:center">${文}</div>`;
  return 层;
};
const 显示不支持 = 缺 => {
  // 文言：既知之，则不复示，并撤“正在检查”之层。汉语：已点过“我明白”就不再提示，同时撤掉可能还在的“正在检查浏览器”层。
  if (已知晓()) { if (typeof document !== 'undefined') document.getElementById(检查层标识)?.remove(); return null; }
  const 层 = 显示检查层(
    '<h2 style="margin:0 0 .5em">您的浏览器版本过旧，无法运行本页面</h2>'
    + '<p style="margin:0 0 .5em">请将浏览器更新到 Safari 27、Chrome 137、Edge 137 或 Firefox 153 及以上版本后再访问。</p>'
    + `<p style="margin:0 0 1em;font-size:13px;opacity:.7">缺少：${缺}</p>`
    + '<p style="margin:0 0 .75em">您也可以强行继续访问，但部分功能将无法使用。</p>'
    + '<button type="button" style="font:inherit;padding:.4em 2em;border:0;border-radius:6px;background:#1d1b18;color:#f5f2eb;cursor:pointer">我明白</button>');
  const 钮 = 层?.querySelector('button');
  钮?.addEventListener('click', () => {
    try { sessionStorage.setItem(知晓键, '1'); } catch { /* 文言：记之不成，则下页复示，无妨。汉语：存不下就在下一页再提示，不影响使用。 */ }
    层.remove();
  });
  钮?.focus();
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
    : setTimeout(() => 显示检查层('<p style="margin:0">正在检查浏览器…</p>'), 400);
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
