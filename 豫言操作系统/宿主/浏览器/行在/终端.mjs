// 文言：页之终端（提案〇〇〇四三）：开机壳之出书于 <pre>，键入原样付壳之标准入，如真终端。惟识回车、换行、退格、制表与常用之 CSI 序（移标、擦行、清屏）；
//   壳入原始之制（组字）则每键径送；否则页自回显、自理退格，至回车乃送一行（犹终端之规范之制）。非 ASCII 之字占二格，同组字之「输入格宽」。
// 汉语：页面终端（提案 00043）：开机壳的输出写进 <pre>，按键原样送进开机壳的标准输入，像真终端一样。只认回车、换行、退格、制表与常用的 CSI 序列
//   （移光标、擦行、清屏），颜色等其余序列略过。壳进了原始模式（组字）时每个键直接送；否则页面自己回显、处理退格，按回车才送一行（同终端的规范模式）。
//   非 ASCII 的字占两格（同组字编辑器的「输入格宽」）：组字重画输入行时按显示宽度补空格擦旧字，两边算法一致才擦得干净。
//   待办事项：鼠标、备用屏与滚动区都没做。

const 上限行数 = 5000;

// 文言：键名转所送之序。汉语：特殊键转成送给壳的字节序列（同 xterm）。
const 键序 = {Enter: '\r', Backspace: '\x7f', Tab: '\t', Escape: '\x1b', ArrowUp: '\x1b[A', ArrowDown: '\x1b[B', ArrowRight: '\x1b[C', ArrowLeft: '\x1b[D',
  Home: '\x1b[H', End: '\x1b[F', Delete: '\x1b[3~', PageUp: '\x1b[5~', PageDown: '\x1b[6~'};

// 文言：造终端：屏为 <pre>，键为受键之 <textarea>（容输入法）；送(文) 付壳，原始() 问壳是否原始之制。返 {写}。
// 汉语：造页面终端：屏是显示用的 <pre>，键是接收按键与输入法的 <textarea>；送(文字) 把字送进壳，原始() 问壳是否在原始模式。返回 {写(文字), 聚焦()}。
export function 造终端({屏, 键, 送, 原始 = () => true}) {
  let 行们 = [[]], 行 = 0, 列 = 0, 转义 = '', 待绘 = false, 本行 = '';
  const 当行 = () => (行们[行] ??= []);
  const 下一行 = () => {
    行++;
    while (行们.length <= 行) 行们.push([]);
    if (行们.length > 上限行数) { const 去 = 行们.length - 上限行数; 行们 = 行们.slice(去); 行 -= 去; }
  };
  // 文言：置一字：宽字占二格，次格为空串；所覆之宽字残半则补空格。汉语：放一个字：宽字占两格，第二格记空串；盖住别的宽字的一半时，把剩下那一半换成空格。
  const 放 = 字 => {
    const 宽 = 字 > '\x7f' ? 2 : 1, 格们 = 当行();
    while (格们.length < 列 + 宽) 格们.push(' ');
    if (格们[列] === '' && 列 > 0) 格们[列 - 1] = ' ';
    if (格们[列 + 宽] === '') 格们[列 + 宽] = ' ';
    格们[列] = 字;
    if (宽 === 2) 格们[列 + 1] = '';
    列 += 宽;
  };
  // 文言：行 CSI 之序。汉语：执行一条 CSI 序列：参数是分号隔开的数，末字决定动作。
  const 办序 = 序 => {
    const 末 = 序.at(-1), 参们 = 序.slice(0, -1).replace(/^[?>=]/, '').split(';').map(项 => parseInt(项, 10)), 一 = 参们[0] || 0, 至少一 = Math.max(1, 一);
    switch (末) {
      case 'K': { const 格们 = 当行(); if (一 === 0) 格们.length = Math.min(格们.length, 列); else if (一 === 1) for (let 位 = 0; 位 <= 列 && 位 < 格们.length; 位++) 格们[位] = ' '; else 行们[行] = []; break; }
      case 'J': if (一 >= 2) { 行们 = [[]]; 行 = 0; 列 = 0; } else if (一 === 0) { 行们.length = 行 + 1; 当行().length = Math.min(当行().length, 列); } break;
      case 'A': 行 = Math.max(0, 行 - 至少一); break;
      case 'B': for (let 次 = 0; 次 < 至少一; 次++) 下一行(); break;
      case 'C': 列 += 至少一; break;
      case 'D': 列 = Math.max(0, 列 - 至少一); break;
      case 'G': 列 = Math.max(0, 一 - 1); break;
      case 'H': case 'f': { const 顶 = Math.max(0, 行们.length - 40); 行 = 顶 + Math.max(0, 一 - 1); while (行们.length <= 行) 行们.push([]); 列 = Math.max(0, (参们[1] || 1) - 1); break; }
      default: break;
    }
  };
  const 绘 = () => {
    待绘 = false;
    const 前 = 行们.slice(0, 行).map(格们 => 格们.join('')).join('\n'), 格们 = 当行(), 后 = 行们.slice(行 + 1).map(格们 => 格们.join('')).join('\n');
    const 标 = document.createElement('span');
    标.className = '光标';
    标.textContent = 格们[列] || ' ';
    屏.replaceChildren((前 ? 前 + '\n' : '') + 格们.slice(0, 列).join(''), 标, 格们.slice(列 + 1).join('') + (后 ? '\n' + 后 : ''));
    屏.scrollTop = 屏.scrollHeight;
  };
  const 写 = 文 => {
    for (const 字 of 文) {
      if (转义) {
        转义 += 字;
        if (转义 === '\x1b[' || 转义 === '\x1b]') continue;
        if (转义.startsWith('\x1b[')) { if (/[@-~]/.test(字)) { 办序(转义.slice(2)); 转义 = ''; } continue; }
        if (转义.startsWith('\x1b]')) { if (字 === '\x07' || 转义.endsWith('\x1b\\')) 转义 = ''; continue; }
        转义 = '';
        continue;
      }
      if (字 === '\x1b') 转义 = 字;
      else if (字 === '\r') 列 = 0;
      else if (字 === '\n') { 下一行(); 列 = 0; }
      else if (字 === '\b') 列 = Math.max(0, 列 - 1);
      else if (字 === '\t') do 放(' '); while (列 % 8);
      else if (字 >= ' ' && 字 !== '\x7f') 放(字);
    }
    if (!待绘) { 待绘 = true; requestAnimationFrame(绘); }
  };
  // 文言：规范之制：页自回显，退格去末字，回车送一行。汉语：规范模式（壳没进原始模式）：页面自己回显，退格删末字，回车把整行连换行送出。
  const 入 = 文 => {
    if (原始()) { 送(文); return; }
    for (const 字 of 文) {
      if (字 === '\r' || 字 === '\n') { 写('\r\n'); 送(本行 + '\n'); 本行 = ''; }
      else if (字 === '\x7f' || 字 === '\b') { if (本行) { const 末 = Array.from(本行).at(-1); 本行 = Array.from(本行).slice(0, -1).join(''); 写(末 > '\x7f' ? '\b\b  \b\b' : '\b \b'); } }
      else if (字 === '\x04') { if (!本行) 送(''); }
      else if (字 >= ' ') { 本行 += 字; 写(字); }
    }
  };
  键.addEventListener('keydown', 事 => {
    if (事.isComposing || 事.keyCode === 229) return;
    let 序 = 键序[事.key];
    if (!序 && 事.ctrlKey && !事.altKey && !事.metaKey && /^[a-z]$/i.test(事.key)) {
      if (事.key.toLowerCase() === 'c' && String(getSelection())) return;
      序 = String.fromCharCode(事.key.toUpperCase().charCodeAt(0) - 64);
    }
    if (!序) return;
    事.preventDefault();
    入(序);
  });
  键.addEventListener('input', 事 => {
    if (事.isComposing) return;
    const 文 = 键.value;
    键.value = '';
    if (文) 入(文.replace(/\r?\n/g, '\r'));
  });
  键.addEventListener('compositionend', 事 => {
    const 文 = 事.data || 键.value;
    键.value = '';
    if (文) 入(文);
  });
  屏.addEventListener('mouseup', () => { if (!String(getSelection())) 键.focus(); });
  return {写, 聚焦: () => 键.focus()};
}
