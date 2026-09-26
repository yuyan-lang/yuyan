// 文言：验收之具：造 JSDOM 页，以真实 Wasm 探针为客，逐术呼之；本具专验网页定时等八包。
// 汉语：前端接口一致性验收夹具：创建 JSDOM 页面，加载真实 Wasm 的“前端接口探针”应用，通过“豫言测试命令/回复”消息逐个调用接口函数。
//       与网页事件浏览器/一致性验证/测试/夹具.mjs 同构，只是探针产物与可注入的宿主选项（网络、编译客户端、导入模块）不同。
import {readFile} from 'node:fs/promises';
import {pathToFileURL, fileURLToPath} from 'node:url';

// 文言：路径皆可由环境变量易之；默认合于语言仓之布局。汉语：JSDOM 路径与探针产物目录都可用环境变量覆盖（变量名是中文，用 env 传）。
const 环境 = process.env;
const 本目录 = new URL('.', import.meta.url);
export const JSDOM路径 = 环境.JSDOM路径 ?? fileURLToPath(new URL('../../../../../../yuyan-cloud/网站/node_modules/jsdom/lib/api.js', 本目录));
export const 产物目录 = (环境.前端探针产物目录 ?? fileURLToPath(new URL('../产物/', 本目录))).replace(/\/?$/u, '/');
export const {JSDOM} = await import(pathToFileURL(JSDOM路径));
export const 宿主模块 = await import(pathToFileURL(产物目录 + '宿主.mjs'));
export const {创建浏览器宿主} = 宿主模块;
const 程序模块 = await WebAssembly.compile(await readFile(产物目录 + '程序.wasm'));
const 值桥模块 = await WebAssembly.compile(await readFile(产物目录 + '值桥.wasm'));

export const 页面壳 = '<!doctype html><html lang="zh-CN"><body><div id="根"></div></body></html>';
export const 等待 = async (判定, 说明 = '条件', 次数 = 300, 间隔 = 10) => {
  for (let 次 = 0; 次 < 次数; 次++) {
    const 果 = await 判定();
    if (果) return 果;
    await new Promise(完成 => setTimeout(完成, 间隔));
  }
  throw Error('等待超时：' + 说明);
};
export const 睡 = 毫秒 => new Promise(完成 => setTimeout(完成, 毫秒));

// 启动：返回 {窗, 文, 宿主, 调, 发, 关, 错误}
// 选项：html、url、网络、储存、队列上限、窗扩展(窗)、编译客户端、导入模块
export async function 启动探针({html = 页面壳, 网络 = async () => new Response('', {status: 404}), url = 'https://yuyan-lang.org/cloud/', 队列上限,
  窗扩展 = () => {}, 编译客户端, 导入模块, 储存, 页面应用超时, 全局包装 = 窗 => 窗} = {}) {
  const 窗 = new JSDOM(html, {url, pretendToBeVisual: true}).window;
  const 文 = 窗.document;
  窗扩展(窗);
  const 选项 = {程序模块, 值桥模块, 根: 文, 网络, 储存: 储存 ?? 窗.localStorage, 全局: 全局包装(窗)};
  if (队列上限) 选项.队列上限 = 队列上限;
  if (编译客户端 !== undefined) 选项.编译客户端 = 编译客户端;
  if (导入模块) 选项.导入模块 = 导入模块;
  if (页面应用超时) 选项.页面应用超时 = 页面应用超时;
  const 宿主 = 创建浏览器宿主(选项);
  let 错误;
  const 完成 = 宿主.完成.catch(错 => { 错误 = 错; });
  const 等回复 = new Map();
  文.addEventListener('豫言测试回复', 事 => {
    const 项 = 事.detail;
    const 待 = 等回复.get(项.号);
    if (待) { 等回复.delete(项.号); 待(项); }
  });
  let 下号 = 1;
  // 发命令但不等回复；返回 Promise（回复到达时解析）
  const 发 = (操作, ...参) => new Promise(解析 => {
    const 号 = 下号++;
    等回复.set(号, 解析);
    文.dispatchEvent(new 窗.CustomEvent('豫言测试命令', {detail: {号, 操作, 参}}));
  });
  // 调：等回复，成功返回字符串，失败抛错（错误文在 .message）
  const 调 = async (操作, ...参) => {
    const 回 = await 发(操作, ...参);
    if (!回.成) throw Error(回.果);
    return 回.果;
  };
  const 关 = async () => {
    宿主.关闭();
    await 完成;
    窗.close();
    if (错误) throw 错误;
  };
  await 宿主.就绪;
  return {窗, 文, 宿主, 调, 发, 关, 错误: () => 错误};
}
