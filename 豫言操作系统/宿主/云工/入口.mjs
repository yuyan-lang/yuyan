// 文言：云工外壳唯载器、定授柄。汉语：构建产物中的 Worker 入口只加载 Wasm 与能力清单。
import 程序模块 from './程序.wasm';
import 值桥模块 from './值桥.wasm';
import 许可 from './许可.json';
import 应用要求 from './接口要求组.json';
import 宿主提供 from './宿主提供组.json';
import * as 构建资源 from './动态资源.mjs';
import {创建云工宿主} from './宿主.mjs';
import {核对接口装载} from './接口核对.mjs';
import {DurableObject, WorkerEntrypoint} from 'cloudflare:workers';

核对接口装载({程序模块, 应用要求, 宿主提供, 宿主: '云工'});
const {模块源码, 值桥字节} = 构建资源;
// 文言：用张量计算者，构建器书中央张量内核之模于 动态资源.mjs；无则无之。汉语：用了张量计算的应用，构建器在 动态资源.mjs 里导出中央张量内核模块（Workers 不能在运行时编译 Wasm 字节，须随产物导入）；没有时得到 undefined。
const 中央张量内核模块 = 构建资源.中央张量内核模块 ?? null;
const 造宿主 = () => 创建云工宿主({程序模块, 值桥模块, 许可, 动态资源: {模块源码, 值桥字节}, 中央张量内核模块});
// 文言：隔离体共一宿主，一程序一实例常存。汉语：整个隔离体共用一个宿主，程序实例随它常驻；持久对象各对象一个。
const 宿主 = 造宿主();
export default 宿主;

// 文言：命名服务之壳唯转请于 service-fetch；公域 default 仍自辨 fetch。汉语：通用命名入口只把服务绑定请求作为 service-fetch 交给豫言，供构建器导出应用别名。
export class 豫言服务入口 extends WorkerEntrypoint {
  fetch(request) {
    return 宿主.serviceFetch(request, this.env, this.ctx);
  }
}

// 文言：持久对象外壳唯分派事，所藏之态由豫言经 ctx.storage 管之。汉语：通用 Durable Object 类只把 fetch 与 alarm 交给豫言，持久状态由豫言经对象状态（ctx）使用原生存储维护；事务与独占区由豫言用回调调用。
export class 豫言持久对象 extends DurableObject {
  constructor(ctx, env) {
    super(ctx, env);
    this.ctx = ctx;
    this.env = env;
    this.宿主 = 造宿主();
  }
  fetch(request) { return this.宿主.durableFetch(request, this.env, this.ctx); }
  alarm(info) { return this.宿主.durableAlarm(info, this.env, this.ctx); }
}
