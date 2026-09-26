// 文言：页有指定标识始启客器；无标识则即启。汉语：仅当指定 DOM id 存在时启动豫言 Wasm；未指定 id 时直接启动。
// 文言：客器不能启，或中道而亡（缺 WasmGC、缺 JSPI 等），则于根元素记其因；静页依此以样式显“须新式浏览器”之示，不自书脚本。汉语：启动失败或运行中失败时，给 <html> 加属性 data-yuyan-app-error（值为原因，至多 300 字），并照常在控制台报错；页面用 CSS 属性选择器显示提示，不必自己写脚本。
import {启动豫言浏览器应用} from './入口.mjs';

const 标识 = new URL(import.meta.url).searchParams.get('标识');
const 记错 = 错 => {
  console.error(错);
  try { document.documentElement.setAttribute('data-yuyan-app-error', String(错?.message ?? 错).slice(0, 300)); } catch { /* 仅为提示，记不成亦无妨 */ }
};
export const 宿主 = !标识 || document.getElementById(标识)
  ? 启动豫言浏览器应用()
  : Promise.resolve(null);
export const 就绪 = 宿主.then(实例 => 实例?.就绪);
export const 完成 = 宿主.then(实例 => 实例?.完成);
// 文言：三承诺任一败，皆记其因；此为旁听，不改原承诺之败。汉语：旁听三个承诺的失败（catch 产生新承诺，不吞掉原承诺的拒绝，等待它们的调用方照常收到异常）。
宿主.catch(记错);
就绪.catch(记错);
完成.catch(记错);
