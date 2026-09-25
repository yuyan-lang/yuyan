// 文言：页有指定标识始启客器；无标识则即启。汉语：仅当指定 DOM id 存在时启动豫言 Wasm；未指定 id 时直接启动。
import {启动豫言浏览器应用} from './入口.mjs';

const 标识 = new URL(import.meta.url).searchParams.get('标识');
export const 宿主 = !标识 || document.getElementById(标识)
  ? 启动豫言浏览器应用()
  : Promise.resolve(null);
export const 就绪 = 宿主.then(实例 => 实例?.就绪);
export const 完成 = 宿主.then(实例 => 实例?.完成);
