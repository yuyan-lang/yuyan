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
    try {
      流 ??= 造流(); 旧制 = !!流.isRaw; 流.setRawMode(true);
      // 汉语：ReadStream 将 TTY 句柄设为非阻塞；既有同步读键须等待首字节，恢复句柄阻塞后才交给 readSync。文言：ReadStream 置 TTY 柄为不阻；旧同步读键须候首字，复柄之阻而后授 readSync。
      流._handle?.setBlocking(true);
      已入 = true; return true;
    } catch { 流?.setRawMode(旧制); return false; }
  };
  const 退出 = () => { if (已入) { 流.setRawMode(旧制); 已入 = false; } };
  // 汉语：逐字节读到一个完整字符，避免批量预读命令之后的子程序输入。文言：逐字节读至一全字符，毋批取命令后子程序之入。
  const 读字符 = (等待 = true) => {
    const 字 = Buffer.alloc(1);
    if (!等待) 流._handle?.setBlocking(false);
    try {
    for (;;) {
      let 数;
      try { 数 = 读(字); } catch (错) { if (!等待 && (错.code === 'EAGAIN' || 错.code === 'EWOULDBLOCK')) return null; throw 错; }
      if (!数) return null;
      const 文 = 解码.write(字.subarray(0,数));
      if (文) return 文;
    }
    } finally { if (!等待) 流._handle?.setBlocking(true); }
  };
  // 汉语：取一键：一个 UTF-8 字符，或以 ESC 起的一整段转义序列；等待为阳时头一字阻塞等，为阴时不等待、没有字节答空。文言：取一键：一 UTF-8 之字，或以 ESC 起之一整段转义之序；等待为阳则首字阻候，为阴则不候、无字节答空。
  const 取一键 = (等待 = true) => {
    if (待键.length) return [true,待键.shift()];
    const 首 = 读字符(等待);
    if (首 === null) return null;
    if (首 !== '\x1b') return [true, 首];
    const 次 = 读字符(false);
    if (次 === null) return [true, 首];
    if (次 !== '[' && 次 !== 'O') { 待键.push(次); return [true, 首]; }
    let 序 = 首 + 次;
    // 汉语：连续可读的终端序列保持整键；待办事项：延迟到达的序列尾部另补。文言：连可读之终端序存全键；待办事项：迟至之序尾后补。
    while (序.length < 65536) {
      const 尾 = 读字符(false);
      if (尾 === null) break;
      序 += 尾;
      if (/[A-Za-z~]/u.test(尾)) break;
    }
    return [true, 序];
  };
  const 读取 = () => (已入 ? (取一键() ?? [false,'']) : [false,'']);
  // 汉语：不等待地试读一键（原语 248「瞥键」，2026-10-24，议题 571）：缓冲里有一键就取出来答阳，没有就答阴，不等；取出来的那一键读行编辑存在「预听键」里，下一次听键再用，不会丢。标准输入不是终端（管道、文件）时读不了非阻塞的字节，暂答无键（待办事项：原生执行器在这种情形用零时限 poll 也能试读，诺驿要补上不阻塞的管道读）。
  // 文言：瞥，过目也（《说文》），一视即去。不候而试读一键（原语二百四十八，十月二十四日，议五百七十一）：缓中有一键则取之而答阳，无则答阴，不候；所取之键读行编辑藏于预听键，次听键复用，不亡。标准入非终端（管、卷）则不能读不阻之字节，暂答无键（待办：原生执行器于此以零时 poll 亦能试读，诺驿当补不阻之管读）。
  const 瞥键 = () => (已入 ? (取一键(false) ?? [false,'']) : [false,'']);
  // 汉语：继承输入期间恢复进入前模式，结束或失败后恢复父壳原始编辑模式。文言：继输入间复入前之制，终或败则复父壳原始编辑之制。
  const 暂停 = () => { const 需恢复 = 已入; 退出(); return () => { if (需恢复) 进入(); }; };
  return {进入,退出,读取,瞥键,暂停};
}
