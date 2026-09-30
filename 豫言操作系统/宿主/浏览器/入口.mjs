// 文言：载二客器而启豫言，外壳不行应用之法。汉语：加载应用和值桥 Wasm，启动豫言程序。
import {创建浏览器宿主} from './宿主.mjs';
import {核对接口装载} from './接口核对.mjs';

// 文言：验浏览器之能、示过旧之告，皆归官网所布之引导检查（全站同域共一篇），改之惟布官网；取之不得（本地测试、离线、非页面）则不验不示，径启之。
// 汉语：浏览器能力检查与“版本过旧”提示由官网发布的引导检查完成（全站同域共用一份，见同目录 引导检查.mjs），改检查或提示只需发布官网；取不到时（本地测试、离线、非页面环境）不检查也不提示，直接启动。
const 引导检查址 = '/豫言操作系统/浏览器/引导检查.mjs';
const 取引导检查 = async () => {
  if (typeof document === 'undefined') return null;
  try { return await import(引导检查址); } catch { return null; }
};

export async function 启动豫言浏览器应用(选项 = {}) {
  const 引导 = await 取引导检查();
  const 缺 = 引导 ? 引导.检查浏览器支持() : [];
  if (缺.length) {
    引导.显示不支持(缺.join('、'));
    throw Error('浏览器缺少豫言所需功能：' + 缺.join('、'));
  }
  try {
    return await 启动实际(选项);
  } catch (错) {
    // 文言：客器不能编，亦示过旧之告。汉语：编译失败（CompileError）也视为浏览器过旧。
    if (引导 && 错 instanceof WebAssembly.CompileError)
      引导.显示不支持('WebAssembly 功能（' + String(错.message).slice(0, 120).replace(/[<>&"]/g, 字 => `&#${字.charCodeAt(0)};`) + '）');
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
