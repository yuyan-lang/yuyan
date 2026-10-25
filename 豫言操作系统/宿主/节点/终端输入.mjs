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
  // 汉语：标准输入不是终端（管道、FIFO、普通文件）时没有原始模式可进，读字节径走文件描述符零：接入 process.stdin 取 Node 为该描述符建的流句柄（管道、FIFO 是 Pipe，普通文件没有流句柄），用得着时切换阻塞。「听键」把句柄恢复阻塞后读，读到文件尾答无键；「瞥键」设成非阻塞后试读，暂不可读（EAGAIN）答无键，读完恢复阻塞。普通文件的读本就不阻塞，读到末尾同样返回零。文言：标准入非终端（管、先入先出之管、常卷），听键与瞥键读文件号零：接 process.stdin 取诺驿为之所建之流柄（管与先入先出为 Pipe，常卷无流柄），须时易其阻。听键复柄之阻而后读，至文尽答无键；瞥键设柄不阻而试读，暂不可读者（EAGAIN）答无键，毕复其阻。常卷之读本不阻，至末亦还零。
  let 管柄 = null, 管柄已取 = false;
  const 取管柄 = () => {
    if (管柄已取) return 管柄;
    管柄已取 = true;
    // 汉语：接入 process.stdin 本身会让 Node 把管道改成非阻塞，所以读之前总要按等待与否重设一次阻塞。文言：接 process.stdin 则诺驿自易其管为不阻，故读前必依候否复设其阻。
    try { 管柄 = process.stdin?._handle ?? null; } catch { 管柄 = null; }
    return 管柄;
  };
  // 汉语：逐字节读到一个完整字符，避免批量预读命令之后的子程序输入。文言：逐字节读至一全字符，毋批取命令后子程序之入。
  const 读字符 = (等待 = true) => {
    const 字 = Buffer.alloc(1);
    const 柄 = 已入 ? 流._handle : 取管柄();
    const 复阻 = 已入 ? !等待 : true;
    if (复阻) 柄?.setBlocking?.(已入 ? false : 等待);
    try {
    for (;;) {
      let 数;
      try { 数 = 读(字); } catch (错) { if (!等待 && (错.code === 'EAGAIN' || 错.code === 'EWOULDBLOCK')) return null; throw 错; }
      if (!数) return null;
      const 文 = 解码.write(字.subarray(0,数));
      if (文) return 文;
    }
    } finally { if (复阻) 柄?.setBlocking?.(true); }
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
  const 读取 = () => (已入 || !是终端() ? (取一键() ?? [false,'']) : [false,'']);
  // 汉语：不等待地试读一键（原语 248「瞥键」，2026-10-24，议题 571）：缓冲里有一键就取出来答阳，没有就答阴，不等；取出来的那一键读行编辑存在「预听键」里，下一次听键再用，不会丢。标准输入不是终端（管道、文件）时照样不等待地在文件描述符零上试读，见上面的读字节一支。文言：瞥，过目也（《说文》），一视即去。不候而试读一键（原语二百四十八，十月二十四日，议五百七十一）：缓中有一键则取之而答阳，无则答阴，不候；所取之键读行编辑藏于预听键，次听键复用，不亡。标准入非终端（管、卷）则亦不候而试读于文件号零，见上读字节之支。
  const 瞥键 = () => (已入 || !是终端() ? (取一键(false) ?? [false,'']) : [false,'']);
  // 汉语：继承输入期间恢复进入前模式，结束或失败后恢复父壳原始编辑模式。文言：继输入间复入前之制，终或败则复父壳原始编辑之制。
  const 暂停 = () => { const 需恢复 = 已入; 退出(); return () => { if (需恢复) 进入(); }; };
  return {进入,退出,读取,瞥键,暂停};
}
