// 文言：桥唯译值，业务在客；每求新器，无旧文相混。汉语：仅实现 WasmGC 值交换与参数、输出原语，每次请求创建独立实例。
import { 创建值桥, 小数表示 } from '../../浏览器/编译器/宿主.mjs';
import { 造边界导入, 造边界导出, 启动导出名 } from '../../网页汇编/边界.mjs';
const 解码 = new TextDecoder();
// 文言：客之退出以异常穿栈，宿主捕之而取其码。汉语：应用调用退出时抛出此异常穿过 Wasm 栈，由宿主捕获并取得退出码；零表示成功。
class 客体退出 extends Error {
  constructor(码) { super('客体退出'); this.退出码 = 码; }
}
export function 创建项目核心(模块, 桥模块) {
  const 单次请求 = function(请求) {
    const 桥 = 创建值桥(桥模块);
    const 文 = 值 => 值 instanceof Uint8Array ? 解码.decode(值) : String(值);
    const 输入 = JSON.stringify(请求);
    const 交换上限 = 2 * 1024 * 1024;
    if (new TextEncoder().encode(输入).length > 交换上限) throw Error('请求超过宿主交换上限');
    let 输出 = '', 错误输出 = '';
    const 截止 = performance.now() + 5000;
    // 文言：标准库之宿主服务，惟授九者，余皆为桩。汉语：标准库宿主服务（导入模块「标准库」）只给这九个，其余给桩（调用时报“接口函数未绑定”）；语义见 ../../标准库宿主.汉语.md。
    const 标准库 = {
      运行于Windows: () => false,
      运行于MacOS: () => false,
      运行于Linux: () => false,
      获取命令行参数: () => [输入],
      打印行: 值 => { 输出 += 文(值) + '\n'; },
      打印字符串: 值 => { 输出 += 文(值); },
      // 文言：误出收之，由调者断，不复借之抛错。汉语：标准错误写进错误流：这里先收集，运行失败时作为错误消息交给调用方，不再借它抛错。
      标准错误打印行: 值 => { 错误输出 += 文(值) + '\n'; },
      小数转字符串: 小数表示,
      退出进程: 码 => { throw new 客体退出(Number(码)); }
    };
    const 实例 = new WebAssembly.Instance(模块, {
      ...造边界导入(模块, 桥.原, { 标准库 }),
      'yuyan:browser/v1': {
        check() { if (performance.now() > 截止) throw Error('项目核心执行超时'); },
        fail(值) { throw Error(文(桥.解(值))); }
      }
    });
    try {
      try {
        实例.exports._start();
        // 文言：_start 毕，应用实现启动之术者，乃调其导出。汉语：_start 之后，应用若实现了「启动程序」（导出 豫言操作系统启动/启动程序）就调用它；旧产物没有这个导出。
        const 启动导出 = 造边界导出(实例, 模块, 桥.原)[启动导出名];
        if (启动导出) 启动导出();
      }
      catch (错) { if (!(错 instanceof 客体退出) || 错.退出码 !== 0) throw 错; }
      if (new TextEncoder().encode(输出).length > 交换上限) throw Error('输出超过宿主交换上限');
      return JSON.parse(输出);
    } catch (错) {
      // 文言：败则以所收之误出为言，无之则言其故。汉语：失败时以收集到的标准错误作错误消息（如标准库打印的“未捕捉的豫言异常：…”），没有就用退出码或异常本身的说明。
      const error = 错误输出.trim() || (错 instanceof 客体退出 ? '豫言程序退出：' + 错.退出码 : 错 instanceof WebAssembly.RuntimeError ? '请求含当前 Wasm 核心不支持的转义或数据' : 错.message);
      // 文言：客器陷止，亦还命之错误。汉语：统一 Wasm 陷阱的命令行错误外形，不在宿主解析命令。
      return { ok: false, error, ...(请求.操作 === '命令行' ? { stdout: '', stderr: error } : {}) };
    }
  };
  // 文言：长事志循豫核所定片长分求，逐片续号与总字；原子写入仍归调用者。汉语：宿主只将大文本按豫言给出的事件边界分批交换，累计结果后一次交给 DO 事务。
  return function 执行请求(请求) {
    if (请求?.操作 !== '项目裁决' || 请求?.action !== 'event-plan') return 单次请求(请求);
    const 配置 = 单次请求({操作:'项目裁决',action:'event-plan-config'});
    if (!配置.ok) return 配置;
    const 单元 = 配置.chunkChars;
    if (!Number.isSafeInteger(单元) || 单元 < 1 || 单元 > 32768) throw Error('事件分片配置无效');
    const 原文 = String(请求.text ?? '');
    const 总字节 = new TextEncoder().encode(原文).length;
    function* 传输片() {
      let 起 = 0, 位 = 0, 数 = 0;
      for (const 字 of 原文) {
        位 += 字.length;
        if (++数 === 单元 * 8) {yield 原文.slice(起,位);起=位;数=0;}
      }
      if (起 < 原文.length || 原文.length === 0) yield 原文.slice(起);
    }
    let 序 = 请求.eventSequence, 字节 = 请求.eventBytes;
    const 事件 = [];
    for (const 片 of 传输片()) {
      const 答 = 单次请求({...请求,text:片,fullTextBytes:总字节,eventSequence:序,eventBytes:字节});
      if (!答.ok) return 答;
      事件.push(...答.events);
      序 = 答.eventSequence;字节 = 答.eventBytes;
    }
    return {ok:true,events:事件,eventSequence:序,eventBytes:字节};
  };
}
