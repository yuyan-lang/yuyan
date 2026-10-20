// 文言：网页行在（提案〇〇〇四三）：浏览器之标签即行在。展系统之快照于内存之文件系统（/源、/工具），以开机之壳（豫构交互）为首任务；任务各占一工作线程（同构建宿主之进程表），页惟为屏与键。
// 汉语：网页行在（提案 00043）：浏览器标签页就是行在。把系统快照（/源 的源码与 /工具 的豫构、值桥）展开到内存文件系统，起开机壳（豫构交互）为第一个任务；
//   每个任务占一个 Web Worker（沿用构建宿主的进程表与工作线程），页面只当屏幕与键盘。壳里每句豫言都在浏览器里编成网页汇编单元再装入运行，与原生同一份程序。
import {构建文件系统, 展开归档, 取压缩归档} from '../构建/文件系统.mjs';
import {进程表} from '../构建/进程.mjs';

// 文言：开机壳之环境：缓存根定于 /tmp，不起将作与行在（浏览器无 TCP）。汉语：开机壳的环境：共享缓存根定在 /tmp/豫构缓存，不起将作（构建服务）与行在（浏览器没有 TCP）。
export const 开机环境 = {豫言缓存目录: '/tmp/豫构缓存', 豫言将作: '关', 豫言行在: '关', HOME: '/数据', USER: '网页'};

// 文言：行在所需之能：跨源隔离（SharedArrayBuffer）、工作线程与解压之流。汉语：网页行在需要跨源隔离（SharedArrayBuffer）、Worker 与 DecompressionStream。
export function 行在缺能() {
  return [!globalThis.crossOriginIsolated && '跨源隔离（SharedArrayBuffer）', typeof Worker !== 'function' && 'Worker',
    typeof DecompressionStream !== 'function' && 'DecompressionStream'].filter(Boolean);
}

// 文言：起行在：取诸快照（源.tar.gz 为 /源，工具.tar.gz 为 /工具）、展之、立进程表、起开机壳而开其入。出(流, 文) 受壳之出，报(文) 受进度。
// 汉语：起网页行在：取各快照（源.tar.gz 展成 /源，工具.tar.gz 展成 /工具）并展开，建进程表，起开机壳并打开它的标准输入。出(流, 文字) 收壳的输出，报(文字) 收进度。
export async function 起行在({快照们, 出, 报 = () => {}}) {
  const 缺 = 行在缺能();
  if (缺.length) throw Error('此浏览器缺少网页行在所需的能力：' + 缺.join('、'));
  const 系统 = new 构建文件系统();
  let 文件数 = 0;
  for (const 网址 of 快照们) {
    const 名 = decodeURIComponent(String(网址).split('/').pop().split('?')[0]);
    报('正在取 ' + 名);
    const 归档 = await 取压缩归档(网址, {报传输: 传输 => 报('正在取 ' + 名 + '：' + 传输.连接状态)});
    文件数 += 展开归档(系统, 归档, {目录: '/'}).文件数;
  }
  for (const 目 of ['/数据', '/系统', '/软件', '/tmp']) 系统.建目录(目);
  报('系统快照已展开：' + 文件数 + ' 个文件');
  const 桥模块 = await WebAssembly.compile(系统.读('/工具/节点值桥接.wasm'));
  const 表 = new 进程表({文件系统: 系统, 桥模块, 工作线程网址: new URL('../构建/工作线程.mjs', import.meta.url)});
  const 解码们 = {1: new TextDecoder(), 2: new TextDecoder()};
  const 号 = await 表.启动('/工具/豫构.wasm', ['交互', '--直读', '--提示'], '/', 开机环境, 0, (流, 值) => 出(流, 解码们[流].decode(值, {stream: true})));
  if (号 < 0) throw Error('开机壳起不来，系统错误码：' + -号);
  表.开入(号);
  return {文件系统: 系统, 表, 壳号: 号, 送入: 行 => 表.送入(号, 行), 关入: () => 表.关入(号), 完成: 表.候完成(号)};
}
