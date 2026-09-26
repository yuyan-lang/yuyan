// 文言：包装之工作者，转诸请于持久对象。汉语：把入站请求转给持久对象的包装 Worker：与构建产物的 入口.mjs 放在同一目录（它导出通用类 豫言持久对象）；可选请求头 x-do 选择不同的持久对象实例（缺省 main）。
export {豫言持久对象, 豫言服务入口, 豫言工作流} from './入口.mjs';
export default {
  fetch(request, env) { return env.DO.get(env.DO.idFromName(request.headers.get('x-do') ?? 'main')).fetch(request); }
};
