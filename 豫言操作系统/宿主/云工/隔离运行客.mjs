// 文言：绝缘室之门，惟受程序与虚籍，运毕返其数据与响应。汉语：Dynamic Worker 子入口：用内存文件系统运行传入的文件式程序一次，只返回 /数据/ 与 /响应/ 下的文件；输入错误回 400，数据超限回 500。
import 程序 from './程序.wasm';
import 值桥 from './值桥.wasm';
import {内存文件系统, 执行模块} from './编译宿主.mjs';

const 答 = (文, 状态) => new Response(文, {status: 状态, headers: {'Content-Type': 'text/plain; charset=utf-8'}});
const 是字串对象 = 值 => !!值 && typeof 值 === 'object' && !Array.isArray(值) && Object.values(值).every(项 => typeof 项 === 'string');

export default {async fetch(请求) {
  let 入;
  try { 入 = JSON.parse(await 请求.text()); } catch { return 答('输入不是有效 JSON', 400); }
  const 参数 = 入?.args ?? [], 初始 = 入?.files ?? {};
  if (!Array.isArray(参数) || 参数.some(项 => typeof 项 !== 'string')) return 答('args 须为字符串数组', 400);
  if (!是字串对象(初始)) return 答('files 须为路径到文字的对象', 400);
  const 文件 = new 内存文件系统(初始);
  const 果 = await 执行模块(程序, 值桥, 文件, 参数);
  const 解 = new TextDecoder('utf-8', {fatal: true}), files = {};
  let 总 = 0;
  for (const [路径, 项] of 文件.文件) {
    if (!路径.startsWith('/数据/') && !路径.startsWith('/响应/')) continue;
    总 += 项.内容.length;
    if (总 > 1048576 || Object.keys(files).length >= 256) return 答('运行数据超过上限', 500);
    try { files[路径] = 解.decode(项.内容); } catch { return 答('输出文件不是有效 UTF-8：' + 路径, 500); }
  }
  return Response.json({...果, files});
}};
