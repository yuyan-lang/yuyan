// 文言：工中载客器，以工全局为根；外壳不行客法。汉语：Dedicated Worker 自动加载本目录的豫言 Wasm，业务逻辑由豫言程序执行。
import {启动豫言浏览器应用} from './入口.mjs';

export const 宿主 = 启动豫言浏览器应用({根: globalThis, 全局: globalThis});
export const 就绪 = 宿主.then(实例 => 实例.就绪);
export const 完成 = 宿主.then(实例 => 实例.完成);
