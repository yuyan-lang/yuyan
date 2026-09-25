// 文言：先验清单与客器，后启旧浏览器宿主；汉语：接口预检通过后才调用现有浏览器启动入口。
import {启动豫言浏览器应用 as 启动原应用} from './原入口.mjs';
import {核对首批接口装载} from './装载核对.mjs';

export async function 启动豫言浏览器应用(选项 = {}) {
  const 路径 = 选项.路径 ?? new URL('.', import.meta.url);
  const [程序回应, 应用回应, 宿主回应, 入口回应, 投影回应] = await Promise.all([
    fetch(new URL('程序.wasm', 路径)),
    fetch(new URL('应用要求.json', 路径)),
    fetch(new URL('宿主提供.json', 路径)),
    fetch(new URL('启动接口.json', 路径)),
    fetch(new URL('投影.json', 路径))
  ]);
  if (![程序回应, 应用回应, 宿主回应, 入口回应, 投影回应].every(回应 => 回应.ok)) {
    throw Error('豫言操作系统装载失败：接口或程序资源不可用');
  }
  const [程序模块, 应用要求, 宿主提供, 应用入口, 投影] = await Promise.all([
    WebAssembly.compile(await 程序回应.arrayBuffer()),
    应用回应.json(), 宿主回应.json(), 入口回应.json(), 投影回应.json()
  ]);
  核对首批接口装载({程序模块, 应用要求, 宿主提供, 应用入口, 投影});
  return 启动原应用(选项);
}
