// 文言：浏览器构建之客户端：取源码之快照与工具链，展于内存之文件系统，依次行诸令（豫构等），出随生随报，毕则尽弃。
// 汉语：浏览器构建客户端（网页编译接口 0.2 的“构建客户端”参考实现）：取源码快照与工具链（gzip 的 tar），展开到内存文件系统，
//   应用改动后依次运行各条命令（豫构等豫言 Wasm 程序，可再启动编译器等子进程，见 进程.mjs），输出随时报告；结束后丢弃全部文件。
//   页面须跨源隔离（COOP same-origin 与 COEP require-corp 或 credentialless），才有 SharedArrayBuffer。
//   作业：{快照:{网址, 剥离层数=1, 缓存=false}, 工具链:{网址, 缓存=false, 编译器='yy_bs_stable.wasm', 豫构='yy豫构_stable.wasm', 值桥='yy稳定节点宿主/yy节点值桥接.wasm'},
//          改动:{相对路径: 内容文字 或 null（删除）}, 命令:[[程序, 参数…]…], 环境:{名: 值}, 继续=false（某条失败后是否接着运行）, 处理器数}
//   报告：{type:"stage",phase,label}、{type:"command",index,argv}、{type:"output",stream,text}、{type:"command-done",index,exitCode,ms}。
//   结果：{ok, results:[{argv, exitCode, ms}], error, phase}。
import {构建文件系统, 展开归档, 取压缩归档, 规范路径} from './文件系统.mjs';
import {进程表} from './进程.mjs';

export const 仓库根 = '/仓库';
let 当前 = null;

export function 构建可用() {
  return typeof SharedArrayBuffer === 'function' && globalThis.crossOriginIsolated === true && typeof Worker === 'function' &&
    typeof DecompressionStream === 'function' && typeof WebAssembly.promising === 'function';
}

export function 停止构建() {
  if (!当前) return false;
  当前.停止();
  return true;
}

export async function 浏览器构建(作业, 报告 = () => {}) {
  if (当前) throw Error('已有构建在运行');
  const 控制器 = new AbortController();
  let 表 = null, 已停 = false, 阶段 = 'load';
  当前 = {停止: () => { 已停 = true; 控制器.abort(); 表?.停止(); }};
  const 开始 = performance.now();
  try {
    if (!构建可用()) {
      const 缺 = [!globalThis.crossOriginIsolated && '跨源隔离（SharedArrayBuffer）', typeof WebAssembly.promising !== 'function' && 'WebAssembly JSPI',
        typeof DecompressionStream !== 'function' && 'DecompressionStream'].filter(Boolean).join('、');
      return {ok: false, results: [], error: '此浏览器缺少构建所需的能力：' + 缺, phase: 阶段};
    }
    const 系统 = new 构建文件系统();
    报告({type: 'stage', phase: 'load', label: '正在取源码快照'});
    const 快照 = await 取压缩归档(作业.快照.网址, {缓存名: 作业.快照.缓存 ? '豫言构建快照' : '', 信号: 控制器.signal});
    const 展 = 展开归档(系统, 快照, {目录: 仓库根, 剥离层数: 作业.快照.剥离层数 ?? 1});
    报告({type: 'stage', phase: 'load', label: '源码快照已展开：' + 展.文件数 + ' 个文件' + (展.全局注释 ? '（' + 展.全局注释 + '）' : '')});
    for (const [路径, 内容] of Object.entries(作业.改动 ?? {})) {
      const 全 = 规范路径(路径, 仓库根);
      if (!全.startsWith(仓库根 + '/')) throw Error('改动路径越出仓库：' + 路径);
      if (内容 === null) 系统.删全部(全); else 系统.写(全, 内容);
    }
    报告({type: 'stage', phase: 'load', label: '正在取工具链'});
    const 工具链 = 作业.工具链;
    展开归档(系统, await 取压缩归档(工具链.网址, {缓存名: 工具链.缓存 ? '豫言构建工具链' : '', 信号: 控制器.signal}), {目录: '/工具链', 剥离层数: 工具链.剥离层数 ?? 0});
    系统.写(仓库根 + '/yy4_bs.wasm', 系统.读('/工具链/' + (工具链.编译器 ?? 'yy_bs_stable.wasm')));
    系统.写(仓库根 + '/yy豫构.wasm', 系统.读('/工具链/' + (工具链.豫构 ?? 'yy豫构_stable.wasm')));
    const 桥模块 = await WebAssembly.compile(系统.读('/工具链/' + (工具链.值桥 ?? 'yy稳定节点宿主/yy节点值桥接.wasm')));
    if (已停) return {ok: false, results: [], error: '已停止', phase: 阶段};
    表 = new 进程表({文件系统: 系统, 桥模块, 工作线程网址: new URL('./工作线程.mjs', import.meta.url), 处理器数: 作业.处理器数});
    阶段 = 'build';
    const 结果们 = [];
    let 成 = true;
    for (const [序, 命令] of (作业.命令 ?? []).entries()) {
      if (已停) break;
      报告({type: 'command', index: 序, argv: 命令});
      const 解码们 = {1: new TextDecoder(), 2: new TextDecoder()};
      const 起 = performance.now();
      const 果 = await 表.运行(命令[0], 命令.slice(1), {目录: 仓库根, 环境: 作业.环境 ?? {},
        观察者: (流, 值) => 报告({type: 'output', stream: 流 === 1 ? 'stdout' : 'stderr', text: 解码们[流].decode(值, {stream: true})})});
      const 毫秒 = Math.round(performance.now() - 起);
      结果们.push({argv: 命令, exitCode: 果.码, ms: 毫秒});
      报告({type: 'command-done', index: 序, exitCode: 果.码, ms: 毫秒});
      if (果.码 !== 0) { 成 = false; if (!作业.继续) break; }
    }
    if (已停) return {ok: false, results: 结果们, error: '已停止', phase: 阶段};
    return {ok: 成, results: 结果们, error: 成 ? '' : '命令失败', phase: 阶段, ms: Math.round(performance.now() - 开始)};
  } catch (错) {
    return {ok: false, results: [], error: 已停 ? '已停止' : (错?.message ?? String(错)), phase: 阶段};
  } finally {
    表?.停止();
    当前 = null;
  }
}
