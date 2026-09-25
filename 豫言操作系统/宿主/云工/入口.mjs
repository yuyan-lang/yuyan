// 文言：云工外壳唯载器、定授柄。汉语：构建产物中的 Worker 入口只加载 Wasm 与能力清单。
import 程序模块 from './程序.wasm';
import 值桥模块 from './值桥.wasm';
import 许可 from './许可.json';
import 应用要求 from './接口要求组.json';
import 宿主提供 from './宿主提供组.json';
import {模块源码, 值桥字节} from './动态资源.mjs';
import {创建云工宿主} from './宿主.mjs';
import {核对接口装载} from './接口核对.mjs';
import {DurableObject, WorkerEntrypoint, WorkflowEntrypoint} from 'cloudflare:workers';

核对接口装载({程序模块, 应用要求, 宿主提供, 宿主: '云工'});
export default 创建云工宿主({程序模块, 值桥模块, 许可, 动态资源: {模块源码, 值桥字节}});

// 文言：命名服务之壳唯转请于 service-fetch；公域 default 仍自辨 fetch。汉语：通用命名入口只把服务绑定请求作为 service-fetch 交给豫言，供构建器导出应用别名。
export class 豫言服务入口 extends WorkerEntrypoint {
  fetch(request) {
    return 创建云工宿主({程序模块, 值桥模块, 许可, 动态资源: {模块源码, 值桥字节}})
      .serviceFetch(request, this.env, this.ctx);
  }
}

// 文言：持久对象外壳唯分派事，所藏之态由豫言经 ctx.storage 管之。汉语：通用 Durable Object 类只把 fetch 与 alarm 交给豫言，持久状态由豫言使用原生存储维护。
export class 豫言持久对象 extends DurableObject {
  constructor(ctx, env) {
    super(ctx, env);
    this.ctx = ctx;
    this.env = env;
    this.宿主 = 创建云工宿主({程序模块, 值桥模块, 许可, 动态资源: {模块源码, 值桥字节}});
  }
  fetch(request) { return this.宿主.durableFetch(request, this.env, this.ctx); }
  alarm(info) { return this.宿主.durableAlarm(info, this.env, this.ctx); }
  webSocketMessage(ws, message) { return this.宿主.durableWebSocketMessage(ws, message, this.env, this.ctx); }
  webSocketClose(ws, code, reason, wasClean) { return this.宿主.durableWebSocketClose(ws, code, reason, wasClean, this.env, this.ctx); }
  webSocketError(ws, error) { return this.宿主.durableWebSocketError(ws, error, this.env, this.ctx); }
}

// 文言：工作流之宿唯依平台步骤调用豫言；步骤名、先后与值悉由客定。汉语：通用 Workflow 类把 run 和持久步骤交给豫言 Wasm，外壳不写业务流程。
export class 豫言工作流 extends WorkflowEntrypoint {
  run(event, step) {
    return 创建云工宿主({程序模块, 值桥模块, 许可, 动态资源: {模块源码, 值桥字节}})
      .workflow(event, step, this.env);
  }
}
