// 文言：包装之工作者，转诸请于持久对象。汉语：把入站请求转给持久对象的包装 Worker：与构建产物的 入口.mjs 放在同一目录（它导出通用类 豫言持久对象）；可选请求头 x-do 选择不同的持久对象实例（缺省 main）。
// 文言：有正文之请，先尽读其正文乃转；不然持久对象未读正文而先答者，转送之泵于答后复读请流，wrangler 视为致命之误而退。汉语：带正文的请求先读完正文再转发（请求信号照旧沿用）；否则持久对象没读正文就先作答时，转发的泵会在响应之后再读请求流，wrangler dev 把这当成致命错误而退出。
export {豫言持久对象, 豫言服务入口} from './入口.mjs';
export default {
  async fetch(request, env) {
    const 实例 = env.DO.get(env.DO.idFromName(request.headers.get('x-do') ?? 'main'));
    if (request.body === null) return 实例.fetch(request);
    return 实例.fetch(new Request(request, {body: await request.arrayBuffer()}));
  }
};
