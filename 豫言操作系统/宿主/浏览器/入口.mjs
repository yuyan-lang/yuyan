// 文言：载二客器而启豫言，外壳不行应用之法。汉语：加载应用和值桥 Wasm，启动豫言程序。
import {创建浏览器宿主} from './宿主.mjs';
import {核对接口装载} from './接口核对.mjs';

export async function 启动豫言浏览器应用(选项 = {}) {
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
