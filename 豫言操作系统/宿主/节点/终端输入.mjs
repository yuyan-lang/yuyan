// 汉语：标准库既有终端原语；宿主只处理TTY模式和字节解码，壳编辑与拼音仍由豫言执行。文言：标准库旧终端之术；宿主惟治TTY之制与字节之解，壳编辑与拼音犹豫言行之。
import * as 文件系统 from 'node:fs';
import 终端 from 'node:tty';
import {StringDecoder as 字节解码器} from 'node:string_decoder';
export function 创建终端输入({是终端 = () => 终端.isatty(0), 造流 = () => new 终端.ReadStream(0), 读 = 字 => 文件系统.readSync(0,字,0,字.length,null)} = {}) {
  let 流 = null, 已入 = false, 旧制 = false;
  const 解码 = new 字节解码器('utf8');
  const 待键 = [];
  const 进入 = () => {
    if (已入) return true;
    if (!是终端()) return false;
    try { 流 ??= 造流(); 旧制 = !!流.isRaw; 流.setRawMode(true); 已入 = true; return true; } catch { return false; }
  };
  const 退出 = () => { if (已入) { 流.setRawMode(旧制); 已入 = false; } };
  // 汉语：逐字节读到一个完整字符，避免批量预读命令之后的子程序输入。文言：逐字节读至一全字符，毋批取命令后子程序之入。
  const 读字符 = () => {
    const 字 = Buffer.alloc(1);
    for (;;) {
      let 数;
      try { 数 = 读(字); } catch (错) { if (错.code === 'EAGAIN' || 错.code === 'EWOULDBLOCK') return null; throw 错; }
      if (!数) return null;
      const 文 = 解码.write(字.subarray(0,数));
      if (文) return 文;
    }
  };
  const 读取 = () => {
    if (!已入) return [false,''];
    if (待键.length) return [true,待键.shift()];
    const 首 = 读字符();
    if (首 === null) return [false,''];
    if (首 !== '\x1b') return [true, 首];
    const 次 = 读字符();
    if (次 === null) return [true, 首];
    if (次 !== '[' && 次 !== 'O') { 待键.push(次); return [true, 首]; }
    let 序 = 首 + 次;
    // 汉语：连续可读的终端序列保持整键；待办事项：延迟到达的序列尾部另补。文言：连可读之终端序存全键；待办事项：迟至之序尾后补。
    while (序.length < 65536) {
      const 尾 = 读字符();
      if (尾 === null) break;
      序 += 尾;
      if (/[A-Za-z~]/u.test(尾)) break;
    }
    return [true, 序];
  };
  // 汉语：继承输入期间恢复进入前模式，结束或失败后恢复父壳原始编辑模式。文言：继输入间复入前之制，终或败则复父壳原始编辑之制。
  const 暂停 = () => { const 需恢复 = 已入; 退出(); return () => { if (需恢复) 进入(); }; };
  return {进入,退出,读取,暂停};
}
