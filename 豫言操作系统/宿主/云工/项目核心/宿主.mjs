// 文言：桥唯译值，业务在客；每求新器，无旧文相混。汉语：仅实现 WasmGC 值交换与参数、输出原语，每次请求创建独立实例。
import { 创建值桥 } from '../../浏览器/编译器/宿主.mjs';
const 解码 = new TextDecoder();
export function 创建项目核心(模块, 桥模块) {
  const 单次请求 = function(请求) {
    const 桥 = 创建值桥(桥模块);
    const 文 = 值 => 值 instanceof Uint8Array ? 解码.decode(值) : String(值);
    const 输入 = JSON.stringify(请求);
    const 交换上限 = 2 * 1024 * 1024;
    if (new TextEncoder().encode(输入).length > 交换上限) throw Error('请求超过宿主交换上限');
    let 输出 = '';
    const 截止 = performance.now() + 5000;
    const 原语 = {
      豫言_运行于Windows: () => false,
      豫言_运行于MacOS: () => false,
      豫言_运行于Linux: () => false,
      豫言_获取命令行参数: () => [[输入], 1],
      豫言_打印行: 值 => { 输出 += 文(值) + '\n'; },
      豫言_打印字符串: 值 => { 输出 += 文(值); },
      豫言_标准错误打印行: 值 => { throw Error(文(值)); },
      豫言_字符串转整数: 值 => BigInt(文(值)),
      豫言_字节转字符串: 值 => Uint8Array.of(Number(值)),
      豫言_整数加: (甲, 乙) => BigInt.asIntN(64, 甲 + 乙),
      豫言_整数乘: (甲, 乙) => BigInt.asIntN(64, 甲 * 乙),
      豫言_整数除: (甲, 乙) => 甲 / 乙,
      豫言_整数转字符串: 值 => String(值),
      豫言_小数转字符串: 值 => String(值?.小数 ?? 值)
    };
    const 实例 = new WebAssembly.Instance(模块, {
      'yuyan:browser/v1': {
        check() { if (performance.now() > 截止) throw Error('项目核心执行超时'); },
        fail(值) { throw Error(文(桥.解(值))); }
      },
      'yuyan:gc-host/v1': { call(名, 参数) {
        const 名称 = 文(桥.解(名));
        if (!Object.hasOwn(原语, 名称)) throw Error('项目核心未授权宿主原语：' + 名称);
        const 结果 = 原语[名称](...桥.解(参数));
        if (输出.length > 交换上限) throw Error('输出超过宿主交换上限');
        return 桥.编(结果);
      } }
    });
    try { 实例.exports._start(); if (new TextEncoder().encode(输出).length > 交换上限) throw Error('输出超过宿主交换上限'); return JSON.parse(输出); }
    catch (错) {
      const error = 错 instanceof WebAssembly.RuntimeError ? '请求含当前 Wasm 核心不支持的转义或数据' : 错.message;
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
