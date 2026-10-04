// 汉语：标准库既有终端原语；宿主只处理TTY模式和字节解码，壳编辑与拼音仍由豫言执行。文言：标准库旧终端之术；宿主惟治TTY之制与字节之解，壳编辑与拼音犹豫言行之。
const 文件 = require('node:fs');
const 终端 = require('node:tty');
const {StringDecoder: 字节解码器} = require('node:string_decoder');
function 创建终端输入({是终端 = () => 终端.isatty(0), 造流 = () => new 终端.ReadStream(0), 读 = 字 => 文件.readSync(0,字,0,字.length,null)} = {}) {
  let 流 = null, 已入 = false, 旧制 = false;
  const 解码 = new 字节解码器('utf8');
  const 待键 = [];
  const 进入 = () => {
    if (已入) return true;
    if (!是终端()) return false;
    try { 流 ??= 造流(); 旧制 = !!流.isRaw; 流.setRawMode(true); 已入 = true; return true; } catch { return false; }
  };
  const 退出 = () => { if (已入) { 流.setRawMode(旧制); 已入 = false; } };
  const 读取 = () => {
    if (!已入) return [false,''];
    if (待键.length) return [true,待键.shift()];
    const 字 = Buffer.alloc(65536);
    let 数;
    try { 数 = 读(字); } catch (错) { if (错.code === 'EAGAIN' || 错.code === 'EWOULDBLOCK') return [false,'']; throw 错; }
    if (!数) return [false,''];
    const 文 = 解码.write(字.subarray(0,数));
    // 汉语：保留终端转义键序列；待办事项：跨读取块的转义序列与组合粘贴另补。文言：存终端转义键之序；待办事项：跨块之转义序与合粘后补。
    待键.push(...(文.match(/\x1b(?:\[[0-9;]*[A-Za-z~]|O[A-Za-z])|[\s\S]/gu) ?? []));
    return 待键.length ? [true,待键.shift()] : [false,''];
  };
  return {进入,退出,读取};
}
module.exports = {创建终端输入};
