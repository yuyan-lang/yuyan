// 文言：显示面之宿主术（本在图形.mjs，拆出以瘦单应用开机之简包）：页中带 data-yy-显示面 之 canvas，以其属性之值为名而授之；又有前缀之授，以其属起头之名皆许，初取乃开同源之小窗。
//   显示面之表不系于文树：换以 SDL 之窗为面源亦可（见 豫言操作系统/宿主/节点/图形.mjs）。图形处理器、图形显示桥与字体仍在 图形.mjs。
// 汉语：显示面的宿主实现（原先在 图形.mjs 里，拆出来让单应用开机只带这一部分）：页面里带 data-yy-显示面="名称" 属性的 <canvas> 就以该名称授予本次运行；
//   带 data-yy-显示面前缀 属性的画布授权以该前缀起头的面名，第一次取时开一个同源小窗，里面放这块面。显示面表与文树无关：换成 SDL 窗口做面源也行
//   （见 豫言操作系统/宿主/节点/图形.mjs）。图形处理器、图形显示桥与字体留在 图形.mjs，按需载入。
//       显示面之表与图形之能不系于文树，节点之宿主亦内联此篇而共用之：换以 SDL 之窗为面源，以 Dawn 为图形处理器，别供其呈现之器。
// 汉语：显示、图形处理器、图形显示三个接口的浏览器宿主实现。显示面由页面授予：带 data-yy-显示面="名称" 属性的 <canvas> 即以该名称授予本次运行；
//       带 data-yy-显示面前缀 属性的画布授权以该前缀起头的面名，第一次取时开一个同源小窗，里面放这块面。图形处理器用 WebGPU 核心子集。两个工厂只依赖注入的 document 与全局对象，可以脱离 Wasm 单独测试；创建浏览器宿主把它们接到
//       豫言_浏览器_显示 与 豫言_浏览器_图形 两个原语上。
//       Node 宿主（路一）也内联本文件共用其中与 DOM 无关的部分：显示面表（创建显示面表）与图形能力（创建图形能力）；它换用 SDL 窗口作
//       显示面的“后端”，用 Dawn（@kmamal/gpu）作 GPU，并提供自己的“呈现器”，见 豫言操作系统/宿主/节点/图形.mjs。本文件因此不写 import。

// 文言：返客之码，适配依此转为公误。汉语：返回给适配的状态码：0 成功；1 未获授权、2 资源暂不可用、3 资源已失效、4 资源不存在、5 资源已存在、
//       6 资源配额已尽、7 输入无效、8 宿主操作失败（附消息）；9 图形校验错误（附诊断文字）；10 非法资源使用（适配据此终止本次运行）。
export const 状态码 = Object.freeze({成: 0, 未获授权: 1, 暂不可用: 2, 已失效: 3, 不存在: 4, 已存在: 5, 配额已尽: 6, 输入无效: 7, 宿主失败: 8, 校验错误: 9, 非法使用: 10});
export const 码 = 状态码;
// 文言：一值过桥不逾十六兆。汉语：值桥单值上限 16 MiB；图形处理器的“单次写入或读回的最大字节数”即此数。
export const 单次交换上限 = 16 * 1024 * 1024;
export const 显示面属性 = 'data-yy-显示面';
export const 显示面输入属性 = 'data-yy-显示面输入';
// 文言：前缀授权之属：带此属之 canvas 以其值为前缀，授一切以此起头之面名。汉语：前缀授权属性：带它的画布以其值为前缀，授予所有以此起头的显示面名。
export const 显示面前缀属性 = 'data-yy-显示面前缀';
// 文言：首帧既绘，宿主加此属于 canvas；面闭则撤之。页可据以撤“载入中”之示。
// 汉语：显示面画出第一帧时宿主给 canvas 加上这个属性，显示面关闭（归还）时去掉；页面据此撤下“正在载入”的提示。
export const 已画属性 = 'data-yy-已画';

const 解码器 = new TextDecoder('utf-8', {ignoreBOM: true});
export const 文字化 = 值 => (值 instanceof Uint8Array ? 解码器.decode(值) : String(值 ?? ''));
export const 消息 = 错 => String(错?.message ?? 错);
export const 空字节 = new Uint8Array();

// ---------------------------------------------------------------------------
// 一、显示：已授显示面、尺寸、像素提交、输入事件
// ---------------------------------------------------------------------------
// 文言：语义之键名：首版七者，零点二版增编辑与修饰之键十一，零点九版增功能一至功能十二与菜单；单一码点之键亦许，余皆不报。
// 汉语：规范的语义键名：0.1.0 的七个，0.2.0 增加 删除、起首、末尾、上翻页、下翻页、制表、退出 与修饰键 上档、控制、交替、命令；
//       0.9.0 增加 功能一至功能十二（F1–F12）与 菜单（ContextMenu 键）；event.key 是单个 Unicode 标量值时原样交付，其他键（CapsLock……）不交付。
const 键名表 = Object.freeze({ArrowLeft: '左', ArrowRight: '右', ArrowUp: '上', ArrowDown: '下', Enter: '回车', Backspace: '退格', ' ': '空格',
  Delete: '删除', Home: '起首', End: '末尾', PageUp: '上翻页', PageDown: '下翻页', Tab: '制表', Escape: '退出',
  F1: '功能一', F2: '功能二', F3: '功能三', F4: '功能四', F5: '功能五', F6: '功能六', F7: '功能七', F8: '功能八', F9: '功能九', F10: '功能十', F11: '功能十一', F12: '功能十二',
  ContextMenu: '菜单', Shift: '上档', Control: '控制', Alt: '交替', Meta: '命令'});
// 文言：修饰之键，面失焦时补其抬起。汉语：修饰键名；显示面失去焦点时宿主为仍按着的修饰键补发抬起。
export const 修饰键名 = Object.freeze(new Set(['上档', '控制', '交替', '命令']));
// 文言：滚一格当四十八逻辑像素。汉语：按行或按格给出的滚动量，一格折成 48 个逻辑像素（再乘像素比）。
export const 滚动每格逻辑像素 = 48;
export const 译键 = 键 => {
  if (typeof 键 !== 'string') return null;
  if (Object.hasOwn(键名表, 键)) return 键名表[键];
  return [...键].length === 1 ? 键 : null;
};
// 文言：事之种：一按下、二抬起、三文字、四移、五指按、六指抬、七面闭、八滚轮、九计时、十尺变、十一组字（零点七版增）。汉语：事件种类编号，与适配的转换一一对应；0.7.0 增 11 组字更新（文为预编辑文字，甲为光标码点位置）。
export const 事件种类 = Object.freeze({按键按下: 1, 按键抬起: 2, 文字输入: 3, 指针移动: 4, 指针按下: 5, 指针抬起: 6, 显示面已关闭: 7, 滚轮: 8, 显示计时: 9, 显示尺寸变化: 10, 组字更新: 11});
// 文言：每面之事列，限额从大。汉语：每个显示面未取走事件的上限；满时丢弃最旧事件。待办事项：丢弃计数暂不报告。
const 显示事件上限 = 65536;

// 文言：显示面之表，不系于宿主：取面之果、尺寸、提交之验、每面之事列与候者、面闭之序皆在此；面从何来、尺寸何量、素何以画、事何以入，由宿主之后端供之。
// 汉语：显示面表，与宿主无关的部分：取得显示面的结果、尺寸、像素提交的校验、每个显示面的事件队列与等候者、关闭流程。
//       显示面从哪里来、尺寸怎么量、像素怎么画、输入从哪里来，由宿主的“后端”对象提供（浏览器是 canvas，Node 是 SDL 窗口）：
//       找(名) → {值} 或 {码, 文}；同源(面, 值) → 是否同一来源；建(面, 值, {推, 关闭}) → 失败时 {码, 文}，成功时把清理函数放进 面.清理；
//       已断开(面)；尺寸值(面) → [宽, 高]（物理像素）；画(面, 宽, 高, 字节, 区) → 失败时 {码, 文}（区 为 0.8.0 区域提交的 [左, 上, 宽, 高]，可不理会而画整帧）；清理()；
//       0.2.0 另有可缺的 像素比值(面) → [物理, 逻辑] 与 设输入区域(面, 横, 纵, 宽, 高)。
export function 创建显示面表({后端, 已关闭 = () => false}) {
  const 面表 = new Map();
  let 下号 = 1;
  // 文言：返七项：码、四数、文，零点二版于末增一数（滚轮之纵量）。汉语：返回 [码, 甲, 乙, 丙, 丁, 文, 戊]；0.2.0 在末尾追加第五个整数 戊（滚轮事件的纵向滚动量），前六项的位置不变。
  const 结果 = (码值, 甲 = 0, 乙 = 0, 丙 = 0, 丁 = 0, 文 = '', 戊 = 0) => [码值, 甲, 乙, 丙, 丁, 文, 戊];
  const 错 = (码值, 文 = '') => 结果(码值, 0, 0, 0, 0, 文);
  const 事果 = 事 => 结果(码.成, 事.种, 事.甲 ?? 0, 事.乙 ?? 0, 事.丙 ?? 0, 事.文 ?? '', 事.丁 ?? 0);
  const 推 = (面, 事) => {
    if (面.已关闭) return;
    if (面.等待者) {
      const 完成 = 面.等待者;
      面.等待者 = null;
      完成(事果(事));
      return;
    }
    // 文言：指针之移相接者，以新代旧。汉语：队尾也是指针移动时就地替换，只留最新位置。
    const 尾 = 面.队列[面.队列.length - 1];
    if (事.种 === 事件种类.指针移动 && 尾?.种 === 事件种类.指针移动) { 面.队列[面.队列.length - 1] = 事; return; }
    // 汉语：尺寸变化只唤醒重读当前尺寸，相邻通知可合并。文言：尺变惟唤复读今尺，相邻之告可并。
    if (事.种 === 事件种类.显示尺寸变化 && 尾?.种 === 事件种类.显示尺寸变化) return;
    // 汉语：相邻的组字更新只留最新一次。文言：组字之更相接者，惟存其新。
    if (事.种 === 事件种类.组字更新 && 尾?.种 === 事件种类.组字更新) { 面.队列[面.队列.length - 1] = 事; return; }
    // 文言：滚轮之事相接者，并其量而取新位。汉语：队尾也是滚轮事件时合并：滚动量相加，指针位置取新的。
    if (事.种 === 事件种类.滚轮 && 尾?.种 === 事件种类.滚轮) {
      面.队列[面.队列.length - 1] = {...事, 丙: (尾.丙 ?? 0) + (事.丙 ?? 0), 丁: (尾.丁 ?? 0) + (事.丁 ?? 0)};
      return;
    }
    if (面.队列.length >= 显示事件上限) 面.队列.shift();
    面.队列.push(事);
  };
  const 关闭面 = 面 => {
    if (面.已关闭) return;
    面.已关闭 = true;
    for (const 清理 of 面.清理.splice(0)) { try { 清理(); } catch { /* 忽略 */ } }
    for (const 钩 of 面.关闭钩子.splice(0)) { try { 钩(); } catch { /* 忽略 */ } }
    if (面.等待者) {
      面.已报关闭 = true;
      const 完成 = 面.等待者;
      面.等待者 = null;
      完成(事果({种: 事件种类.显示面已关闭}));
    }
  };
  const 取得 = 名 => {
    if (已关闭()) return 错(码.暂不可用, '宿主已关闭');
    if (!名) return 错(码.未获授权, '显示面名称为空');
    const 找果 = 后端.找(名);
    if (找果.码) return 错(找果.码, 找果.文);
    for (const 面 of 面表.values()) if (!面.已关闭 && 后端.同源(面, 找果.值)) return 结果(码.成, 面.号);
    const 面 = {号: 下号++, 名, 队列: [], 等待者: null, 已关闭: false, 已报关闭: false, 模式: null, 清理: [], 关闭钩子: []};
    const 败 = 后端.建(面, 找果.值, {推: 事 => 推(面, 事), 关闭: () => 关闭面(面)});
    if (败) return 错(败.码, 败.文);
    面表.set(面.号, 面);
    return 结果(码.成, 面.号);
  };
  const 取开面 = 号 => {
    const 面 = 面表.get(号);
    if (!面) return null;
    if (!面.已关闭 && 后端.已断开(面)) 关闭面(面);
    return 面;
  };
  // 汉语：归还当前显示会话，复用关闭清理；授权仍由宿主持有，同名再取会得到新的面号。文言：归当显示之会，共用关闭之清；授权犹属宿主，同名复取则得新号。
  const 归还 = 号 => {
    const 面 = 取开面(号);
    if (!面 || 面.已关闭) return 错(码.已失效, '显示面已失效');
    关闭面(面);
    面.队列.length = 0;
    面.已报关闭 = true;
    面表.delete(号);
    return 结果(码.成);
  };
  const 尺寸 = 号 => {
    const 面 = 取开面(号);
    if (!面 || 面.已关闭) return 错(码.已失效, '显示面已失效');
    const [宽, 高] = 后端.尺寸值(面);
    if (!(宽 > 0 && 高 > 0)) return 错(码.暂不可用, '显示面暂时没有正尺寸');
    return 结果(码.成, 宽, 高);
  };
  // 文言：区者 [左, 上, 宽, 高]，缺则全帧；后端不识区则画全帧（零点八版）。汉语：区 为 [左, 上, 宽, 高]（0.8.0 区域提交），缺省为整帧；后端的 画 不认区域时照整帧画。
  const 提交 = (号, 宽, 高, 字节, 区) => {
    const 面 = 取开面(号);
    if (!面 || 面.已关闭) return 错(码.已失效, '显示面已失效');
    if (!Number.isSafeInteger(宽) || !Number.isSafeInteger(高) || 宽 <= 0 || 高 <= 0) return 错(码.输入无效, '画面宽高须为正整数');
    const 应长 = 宽 * 高 * 4;
    if (!Number.isSafeInteger(应长) || !(字节 instanceof Uint8Array) || 字节.length !== 应长) return 错(码.输入无效, '画面字节数须恰为宽×高×4');
    if (区 && !(区[0] >= 0 && 区[1] >= 0 && 区[2] > 0 && 区[3] > 0 && 区[0] + 区[2] <= 宽 && 区[1] + 区[3] <= 高)) return 错(码.输入无效, '区域须为正且在画布之内');
    if (面.模式 === '图形') return 错(码.暂不可用, '此显示面已用于图形处理器呈现');
    const [现宽, 现高] = 后端.尺寸值(面);
    if (现宽 !== 宽 || 现高 !== 高) return 错(码.暂不可用, '提交尺寸与显示面当前尺寸不同');
    const 败 = 后端.画(面, 宽, 高, 字节, 区);
    if (败) return 错(败.码, 败.文);
    面.模式 = '二维';
    return 结果(码.成);
  };
  // 文言：区之左上、宽高各合为一数：甲乘六万五千五百三十六加乙。汉语：（0.8.0）适配把区域的左上、宽高各拼成一个整数：甲×65536+乙。
  const 拆区 = (左上, 宽高) => (Number.isSafeInteger(左上) && Number.isSafeInteger(宽高) && 左上 >= 0 && 宽高 >= 0
    ? [Math.floor(左上 / 65536), 左上 % 65536, Math.floor(宽高 / 65536), 宽高 % 65536] : [-1, -1, 0, 0]);
  const 等待 = (号, 毫秒 = 0) => {
    const 面 = 取开面(号);
    if (!面 || 面.已报关闭) return 错(码.已失效, '显示面已关闭');
    if (面.队列.length) return 事果(面.队列.shift());
    if (面.已关闭) {
      面.已报关闭 = true;
      return 事果({种: 事件种类.显示面已关闭});
    }
    if (面.等待者) return 错(码.输入无效, '同一显示面已有等候者');
    // 汉语：计时与输入共用一个候者，任何一种完成都撤销计时，超时不吞下一输入。文言：计时与输入共一候者，一成则撤时，时尽不夺后事。
    return new Promise(完成 => {
      let 计时器;
      const 收尾 = 果 => {
        clearTimeout(计时器);
        if (面.等待者 === 收尾) 面.等待者 = null;
        完成(果);
      };
      面.等待者 = 收尾;
      if (毫秒 > 0) 计时器 = setTimeout(() => 收尾(事果({种: 事件种类.显示计时})), 毫秒);
    });
  };
  // 文言：诸面并候，事至则报其面之序（自零起），漏尽则序负一而事为计时；只一面则同单面之候。
  // 汉语：多面同时等待：事件到了就交回它所在面的序号（从零起）；期限到时序号为负一、事件为显示计时；只给一块面时同「待入事限时」。
  const 诸果 = (面序, 果) => [果[0], 面序, ...果.slice(1)];
  const 等待诸面 = (号们, 毫秒 = 0) => {
    const 限 = Math.max(0, Number(毫秒) || 0);
    const 列 = Array.from(号们 ?? [], 号 => Number(号));
    if (!列.length) return 错(码.输入无效, '等待诸面需要至少一块显示面');
    if (new Set(列).size !== 列.length) return 错(码.输入无效, '同一显示面不能在一组里出现两次');
    const 单 = 果 => (果[0] === 码.成 ? 诸果(0, 果) : 果);
    if (列.length === 1) {
      const 果 = 等待(列[0], 限);
      return 果 instanceof Promise ? 果.then(单) : 单(果);
    }
    const 面们 = [];
    for (const 号 of 列) {
      const 面 = 取开面(号);
      if (!面 || 面.已报关闭) return 错(码.已失效, '显示面已关闭');
      if (面.等待者) return 错(码.输入无效, '同一显示面已有等候者');
      面们.push(面);
    }
    const 即取 = () => {
      for (let 序 = 0; 序 < 面们.length; 序++) {
        const 面 = 面们[序];
        if (面.已报关闭) return 错(码.已失效, '显示面已关闭');
        if (面.队列.length) return 诸果(序, 事果(面.队列.shift()));
        if (面.已关闭) { 面.已报关闭 = true; return 诸果(序, 事果({种: 事件种类.显示面已关闭})); }
      }
      return null;
    };
    const 即 = 即取();
    if (即) return 即;
    return new Promise(完成 => {
      let 计时器, 已收 = false;
      const 钩们 = new Map();
      const 收尾 = 果 => {
        if (已收) return;
        已收 = true;
        clearTimeout(计时器);
        for (const 面 of 面们) if (面.等待者 === 钩们.get(面)) 面.等待者 = null;
        完成(果);
      };
      for (let 序 = 0; 序 < 面们.length; 序++) {
        const 面 = 面们[序], 钩 = 果 => 收尾(诸果(序, 果));
        钩们.set(面, 钩);
        面.等待者 = 钩;
      }
      if (限 > 0) 计时器 = setTimeout(() => 收尾(诸果(-1, 事果({种: 事件种类.显示计时}))), 限);
    });
  };
  // 文言：题面者，设宿主窗之题，惟为示，不能亦成。汉语：设置宿主窗口的标题，只是提示，宿主做不到也照常成功。
  const 题面 = (号, 题) => {
    const 面 = 取开面(号);
    if (!面 || 面.已关闭) return 错(码.已失效, '显示面已失效');
    const 败 = 后端.题?.(面, 题);
    if (败) return 错(败.码, 败.文);
    return 结果(码.成);
  };
  // 文言：像素比返二数：物理与逻辑，比即其商。汉语：像素比返回两个整数 [物理, 逻辑]，适配以其商为像素比；后端没有 像素比值 时为 1。
  const 像素比 = 号 => {
    const 面 = 取开面(号);
    if (!面 || 面.已关闭) return 错(码.已失效, '显示面已失效');
    const [物理, 逻辑] = 后端.像素比值?.(面) ?? [1, 1];
    if (!(物理 > 0 && 逻辑 > 0)) return 错(码.暂不可用, '显示面暂时没有像素比');
    return 结果(码.成, 物理, 逻辑);
  };
  // 文言：输入之区惟为提示，宿主不能则略之。汉语：文字输入区域只是提示：后端没有 设输入区域 时照常返回成功。
  const 输入区域 = (号, 横, 纵, 宽, 高) => {
    const 面 = 取开面(号);
    if (!面 || 面.已关闭) return 错(码.已失效, '显示面已失效');
    if (![横, 纵, 宽, 高].every(Number.isSafeInteger) || 宽 < 0 || 高 < 0) return 错(码.输入无效, '输入区域须为整数且宽高非负');
    后端.设输入区域?.(面, 横, 纵, 宽, 高);
    return 结果(码.成);
  };
  // 文言：指之形惟为提示：零常箭、一文字、二手形；宿主不能则略之。汉语：（0.7.0）指针形状只是提示：甲为 0 默认、1 文字、2 手形；后端没有 设指针形状 时照常返回成功。
  const 指针形状 = (号, 形) => {
    const 面 = 取开面(号);
    if (!面 || 面.已关闭) return 错(码.已失效, '显示面已失效');
    if (![0, 1, 2].includes(形)) return 错(码.输入无效, '指针形状须为 0、1、2');
    后端.设指针形状?.(面, 形);
    return 结果(码.成);
  };
  // 文言：剪贴之读写，惟后端能者行之（节点之 SDL）；浏览器另有剪贴之适配。汉语：剪贴板读写借显示原语交给后端（Node 的 SDL）；浏览器走自己的剪贴板适配，不用这两项。
  const 剪贴板读 = () => {
    if (typeof 后端.读剪贴板 !== 'function') return 错(码.暂不可用, '宿主没有剪贴板');
    const 果 = 后端.读剪贴板();
    return 果.码 ? 错(果.码, 果.文) : 结果(码.成, 0, 0, 0, 0, 果.文 ?? '');
  };
  const 剪贴板写 = 文 => {
    if (typeof 后端.写剪贴板 !== 'function') return 错(码.暂不可用, '宿主没有剪贴板');
    const 败 = 后端.写剪贴板(文);
    return 败 ? 错(败.码, 败.文) : 结果(码.成);
  };
  // 文言：一口而分诸术；异常皆归码，不越桥。零点二版于字节之后增二数。汉语：原语分派；同步异常都转成宿主操作失败，不让 JS 异常越过 Wasm。
  //       0.2.0 在字节参数之后追加两个整数 丙、丁（输入区域的宽高），原有参数的位置不变；0.8.0 的 提交区域 用 丙、丁 传拼起来的区域左上与宽高。
  const 调用 = (操作, 号, 甲, 乙, 字节, 丙 = 0, 丁 = 0) => {
    try {
      switch (操作) {
        case '取得': return 取得(文字化(字节));
        case '归还': return 归还(号);
        case '尺寸': return 尺寸(号);
        case '提交': return 提交(号, 甲, 乙, 字节);
        case '提交区域': return 提交(号, 甲, 乙, 字节, 拆区(丙, 丁));
        case '等待': return 等待(号);
        case '等待限时': return 等待(号, 甲);
        case '待诸面限时': return 等待诸面(甲, 乙);
        // 文言：题之操作亦作「题映面」，从接口之名，便于适配直呼。汉语：设置标题的操作也认接口名「题映面」，便于适配直接调用。
        case '题': case '题映面': return 题面(号, 文字化(字节));
        case '像素比': return 像素比(号);
        case '输入区域': return 输入区域(号, 甲, 乙, 丙, 丁);
        case '指针形状': return 指针形状(号, 甲);
        case '剪贴板读': return 剪贴板读();
        case '剪贴板写': return 剪贴板写(文字化(字节));
        default: return 错(码.宿主失败, '显示操作不受支持：' + String(操作).slice(0, 64));
      }
    } catch (错误) { return 错(码.宿主失败, 消息(错误)); }
  };
  const 清理 = () => {
    for (const 面 of 面表.values()) 关闭面(面);
    try { 后端.清理?.(); } catch { /* 忽略 */ }
  };
  const 状态 = () => ({
    显示面数: [...面表.values()].filter(面 => !面.已关闭).length,
    显示等待者数: [...面表.values()].filter(面 => 面.等待者).length
  });
  return {调用, 取面: 号 => 取开面(Number(号)), 尺寸值: 面 => 后端.尺寸值(面), 关闭面, 清理, 状态};
}

// 文言：浏览器之后端：面者页中之 canvas；旁置隐 input 以受键与文字；canvas 离文树则面闭。
// 汉语：浏览器的显示面后端：显示面是页面里带 data-yy-显示面 属性的 canvas；宿主在它后面插入隐形输入框接收按键与输入法文字；canvas 离开文档即关闭。
export function 创建画布后端({根, 全局}) {
  let 观察器 = null;
  const 活面 = new Map();
  const 小窗们 = new Map();
  // 文言：面之像素比取其窗之 devicePixelRatio；小窗之面从窗，主窗之面从全局。汉语：显示面的像素比取它所在窗口的 devicePixelRatio；小窗里的面用小窗的，主页面里的面用全局的。
  const 窗素 = 面 => 面?.窗 ?? 面?.画布?.ownerDocument?.defaultView ?? 全局;
  const 像素比 = 面 => {
    const 比 = Number(窗素(面)?.devicePixelRatio);
    return Number.isFinite(比) && 比 > 0 ? 比 : 1;
  };
  // 文言：尺以物理之素计：CSS 之宽高乘像素比而四舍五入。汉语：显示面尺寸按物理像素计：CSS 布局宽高乘 devicePixelRatio 后四舍五入。
  //       待办事项：canvas 带边框或内边距时布局矩形含它们，应改用 ResizeObserver 的 devicePixelContentBoxSize。
  const 尺寸值 = 面 => {
    const 矩 = 面.画布.getBoundingClientRect();
    const 比 = 像素比(面);
    return [Math.round(Number(矩.width) * 比), Math.round(Number(矩.height) * 比)];
  };
  // 文言：前缀之授权：页中带前缀属之 canvas，许一切以此起头之名。初取乃开同源之小窗，内置一画布与隐 input，照主画布之法；
  //       小窗被拦则面不可得，报资源暂不可用。待办事项：小窗之幅取定数，后版依原格子之大小开。
  // 汉语：前缀授权：页里带 data-yy-显示面前缀 属性的画布，授权所有以此起头的面名。第一次取时开一个同源小窗，里面放一块带
  //       data-yy-显示面 的画布与隐形输入框，照主画布的做法；小窗被浏览器拦下时这块面不可得，报资源暂不可用。
  const 小窗宽 = 960, 小窗高 = 640;
  const 开小窗 = 名 => {
    let 窗 = null;
    try { 窗 = 全局.open?.('', 名, 'popup,width=' + 小窗宽 + ',height=' + 小窗高); } catch { 窗 = null; }
    if (!窗) return null;
    try {
      const 文档 = 窗.document;
      if (!文档) { 窗.close?.(); return null; }
      文档.title = 名;
      const 体 = 文档.body ?? 文档.documentElement;
      if (体?.style) { 体.style.margin = '0'; 体.style.overflow = 'hidden'; 体.style.background = '#000'; }
      const 画布 = 文档.createElement('canvas');
      画布.setAttribute(显示面属性, 名);
      画布.setAttribute('style', 'display:block;width:100vw;height:100dvh;margin:0;padding:0;border:0;background:#000;touch-action:none;outline:none');
      画布.豫言小窗 = 窗;
      体.appendChild(画布);
      小窗们.set(名, {窗, 画布});
      return 画布;
    } catch {
      try { 窗.close?.(); } catch { /* 忽略 */ }
      return null;
    }
  };
  const 找画布 = 名 => {
    const 元们 = 根.querySelectorAll?.('canvas') ?? [];
    for (const 元 of 元们) if (元.getAttribute(显示面属性) === 名) return {值: 元};
    const 已开 = 小窗们.get(名);
    if (已开) {
      if (已开.画布.isConnected !== false) return {值: 已开.画布};
      小窗们.delete(名);
    }
    for (const 元 of 元们) {
      const 前缀 = 元.getAttribute?.(显示面前缀属性);
      if (前缀 && String(名).startsWith(前缀)) {
        const 画布 = 开小窗(名);
        return 画布 ? {值: 画布} : {码: 码.暂不可用, 文: '浏览器拦下了显示面「' + 名 + '」的小窗（需要用户手势或弹出窗许可）'};
      }
    }
    return null;
  };
  // 文言：canvas 之 CSS 尺若从其像素数而定，则改其像素数必自涨；首取时定其 CSS 尺以绝此环。
  // 汉语：页面没给 canvas 定 CSS 宽高时，它的布局尺寸跟随 width/height 属性；宿主按像素比改属性会使布局尺寸反复变大。
  //       首次取得时若布局尺寸恰等于属性尺寸且没有内联宽高，就把当时的布局尺寸写进内联样式固定下来。
  const 定样式尺寸 = 画布 => {
    try {
      if (画布.style?.width || 画布.style?.height) return;
      const 矩 = 画布.getBoundingClientRect();
      if (矩.width > 0 && 矩.width === 画布.width && 矩.height === 画布.height) {
        画布.style.width = 矩.width + 'px';
        画布.style.height = 矩.height + 'px';
      }
    } catch { /* 文言：定尺失败亦无妨。汉语：只是防止反馈，失败不影响功能。 */ }
  };
  // 文言：canvas 离文树则面闭。汉语：canvas 被移出文档即视为显示面关闭。
  const 查连接 = () => {
    for (const [面, 关闭] of 活面) if (!面.已关闭 && 面.画布.isConnected === false) 关闭();
  };
  const 建 = (面, 画布, {推, 关闭}) => {
    面.画布 = 画布;
    面.窗 = 画布.ownerDocument?.defaultView ?? 全局;
    面.小窗 = 画布.豫言小窗 ?? null;
    面.输入框 = null;
    面.二维 = null;
    面.组字中 = false;
    活面.set(面, 关闭);
    面.清理.push(() => 活面.delete(面));
    定样式尺寸(画布);
    const 听 = (目标, 名称, 处理, 选项) => {
      目标.addEventListener(名称, 处理, 选项);
      面.清理.push(() => 目标.removeEventListener(名称, 处理, 选项));
    };
    // 文言：canvas 不能受输入法之文，故旁置一隐 input 以受键与文字；点 canvas 则聚焦之。
    // 汉语：canvas 本身不能接收输入法文字，宿主在它后面插入一个透明的 <input>（带 data-yy-显示面输入="名称"）接收按键与文字；点击 canvas 时聚焦它。
    //       应用用 设置文字输入区域 告诉宿主光标所在的矩形，宿主把这个输入框移过去，输入法候选框随之定位（见 设输入区域）。
    const 文档 = 画布.ownerDocument ?? 根;
    const 输入框 = 文档.createElement('input');
    输入框.setAttribute('type', 'text');
    输入框.setAttribute(显示面输入属性, 面.名);
    输入框.setAttribute('autocomplete', 'off');
    输入框.setAttribute('aria-label', 面.名);
    输入框.setAttribute('style', 'position:fixed;left:0;top:0;width:1px;height:1px;opacity:0;border:0;padding:0;pointer-events:none');
    if (typeof 画布.after === 'function') 画布.after(输入框);
    else 画布.parentNode?.insertBefore(输入框, 画布.nextSibling ?? null);
    面.输入框 = 输入框;
    // 文言：取面之时，页无可编之物持焦，则聚焦输入框，入面即可打字。
    // 汉语：取得显示面时，页面上若没有可编辑的元素占着焦点，就聚焦输入框，进入显示面不必先点击就能打字。
    const 活 = 文档.activeElement;
    if (!(活 && 活 !== 文档.body && !活.disabled && (活.isContentEditable || 活.tagName === 'INPUT' || 活.tagName === 'TEXTAREA'))) {
      try { 输入框.focus({preventScroll: true}); } catch { /* 忽略 */ }
    }
    const 坐标 = 事件 => {
      const 矩 = 画布.getBoundingClientRect();
      const 比 = 像素比(面);
      return [Math.floor((Number(事件.clientX) - Number(矩.left)) * 比), Math.floor((Number(事件.clientY) - Number(矩.top)) * 比)];
    };
    听(画布, 'pointerdown', 事件 => {
      const [甲, 乙] = 坐标(事件);
      try { 输入框.focus({preventScroll: true}); } catch { /* 忽略 */ }
      try { 画布.setPointerCapture?.(事件.pointerId); } catch { /* 文言：合成之事无活指针。汉语：合成事件没有活动指针，捕获失败无妨。 */ }
      面.同步修饰?.(事件);
      推({种: 事件种类.指针按下, 甲, 乙, 丙: Number(事件.button) || 0});
    });
    // 文言：鼠按则止其默认而复聚焦输入框：点不可聚焦之 canvas，浏览器之默认或移焦于 body（WebKit 如此），pointerdown 所予之焦随之而失。
    // 汉语：鼠标按下时阻止默认动作并再次聚焦输入框：点击不可聚焦的 canvas，浏览器的默认处理可能把焦点移回 body（WebKit 如此），pointerdown 里给输入框的焦点就丢了。
    听(画布, 'mousedown', 事件 => {
      事件.preventDefault?.();
      try { 输入框.focus({preventScroll: true}); } catch { /* 忽略 */ }
    });
    听(画布, 'pointermove', 事件 => {
      const [甲, 乙] = 坐标(事件);
      推({种: 事件种类.指针移动, 甲, 乙});
    });
    听(画布, 'pointerup', 事件 => {
      const [甲, 乙] = 坐标(事件);
      推({种: 事件种类.指针抬起, 甲, 乙, 丙: Number(事件.button) || 0});
    });
    听(画布, 'contextmenu', 事件 => 事件.preventDefault?.());
    // 文言：滚轮之量折为物理之素：像素之式乘像素比，行之式每行四十八逻辑像素，页之式以画布之高；止页面之滚。
    // 汉语：滚轮：deltaMode 为像素时乘像素比，为行时每行折 48 个逻辑像素，为页时每页折画布高度；阻止页面跟着滚动（须非被动监听）。
    听(画布, 'wheel', 事件 => {
      事件.preventDefault?.();
      const [甲, 乙] = 坐标(事件);
      const 比 = 像素比(面);
      const 倍 = 事件.deltaMode === 1 ? 滚动每格逻辑像素 * 比 : 事件.deltaMode === 2 ? (Number(画布.getBoundingClientRect().height) || 0) * 比 : 比;
      推({种: 事件种类.滚轮, 甲, 乙, 丙: Math.round(Number(事件.deltaX || 0) * 倍) || 0, 丁: Math.round(Number(事件.deltaY || 0) * 倍) || 0});
    }, {passive: false});
    // 文言：修饰之键记其按住者，面失焦则补其抬起；制表之键止其默认，免焦点离面。
    // 汉语：记下按着的修饰键，输入框失去焦点时为它们补发按键抬起；制表键阻止默认动作，免得焦点离开显示面。
    面.按住修饰 = new Set();
    // 汉语：按事件自带的修饰标志补发修饰键的按下或抬起：自动化工具与某些输入法只带标志、不单独发修饰键事件（原生侧 1d70bdd9b 同理）。文言：依事所带修饰之标补发修饰键之按抬：自动之器与某些输入法惟带其标，不另发修饰键之事。
    const 修饰标志 = [['上档', 'shiftKey'], ['控制', 'ctrlKey'], ['交替', 'altKey'], ['命令', 'metaKey']];
    面.同步修饰 = 事件 => {
      for (const [名, 标] of 修饰标志) {
        if (typeof 事件[标] !== 'boolean') continue;
        const 按着 = 面.按住修饰.has(名);
        if (事件[标] && !按着) { 面.按住修饰.add(名); 推({种: 事件种类.按键按下, 文: 名}); }
        else if (!事件[标] && 按着) { 面.按住修饰.delete(名); 推({种: 事件种类.按键抬起, 文: 名}); }
      }
    };
    const 按键 = 种 => 事件 => {
      if (事件.isComposing || 事件.keyCode === 229 || 面.组字中) return;
      const 键 = 译键(事件.key);
      if (键 === null) return;
      if (键 === '制表') 事件.preventDefault?.();
      // 汉语：功能键与菜单键阻止浏览器的默认动作（F1 帮助、F3 查找、F5 刷新、F6 地址栏、F10 菜单栏）；F11 全屏与 F12 开发者工具照旧放行。
      // 文言：功能与菜单之键止浏览器之默认（F1 助、F3 寻、F5 刷新、F6 地址、F10 菜单）；F11 全屏、F12 开发者之器仍放行之。
      if (键 === '菜单' || (键.startsWith('功能') && 键 !== '功能十一' && 键 !== '功能十二')) 事件.preventDefault?.();
      // 汉语：（剪贴板 0.2）命令或控制加 c、x、v 由应用经剪贴板接口处理，阻止输入框的默认复制粘贴，免得粘贴的文字又作文字输入交付一次。文言：命令或控制加 c、x、v，客以剪贴之约处之，止输入框之默认，免所贴之文复以文字之入交之。
      if ((事件.metaKey || 事件.ctrlKey) && /^[cxvCXV]$/.test(String(事件.key))) 事件.preventDefault?.();
      if (修饰键名.has(键)) {
        if (种 === 事件种类.按键按下) 面.按住修饰.add(键);
        else 面.按住修饰.delete(键);
      } else 面.同步修饰(事件);
      推({种, 文: 键});
    };
    听(输入框, 'keydown', 按键(事件种类.按键按下));
    听(输入框, 'keyup', 按键(事件种类.按键抬起));
    听(输入框, 'blur', () => {
      for (const 键 of 面.按住修饰) 推({种: 事件种类.按键抬起, 文: 键});
      面.按住修饰.clear();
    });
    // 文言：成文则交：组字之中不交，组字毕乃交其全文。汉语：已提交的文字才交付：组字期间的 input 事件不交付，compositionend 时交付输入框里的全文。
    const 交文字 = 备用 => {
      const 文 = 输入框.value || 备用 || '';
      输入框.value = '';
      if (文) 推({种: 事件种类.文字输入, 文});
    };
    // 文言：组字之中，输入框之值即预编之文，光标之位折为码位，以组字之更交之；组字毕先告空文，后交成文。
    // 汉语：（0.7.0）组字期间输入框的值就是预编辑文字，把它和光标位置（UTF-16 下标折成码点数）作组字更新交付；组字结束先交空文字的组字更新，再交提交的文字。
    const 交组字 = (备用 = '') => {
      const 文 = 输入框.value || 备用 || '';
      const 位 = Number.isSafeInteger(输入框.selectionStart) ? Math.min(输入框.selectionStart, 文.length) : 文.length;
      推({种: 事件种类.组字更新, 文, 甲: [...文.slice(0, 位)].length});
    };
    听(输入框, 'compositionstart', () => { 面.组字中 = true; });
    听(输入框, 'compositionupdate', 事件 => { if (!输入框.value) 交组字(typeof 事件.data === 'string' ? 事件.data : ''); });
    听(输入框, 'compositionend', 事件 => {
      面.组字中 = false;
      推({种: 事件种类.组字更新, 文: '', 甲: 0});
      交文字(typeof 事件.data === 'string' ? 事件.data : '');
    });
    听(输入框, 'input', 事件 => { if (!面.组字中 && !事件.isComposing) 交文字(''); else 交组字(); });
    // 文言：先撤诸听，后去输入框。汉语：关闭时先移除监听，再移除输入框。
    面.清理.push(() => 输入框.remove());
    面.清理.push(() => 画布.removeAttribute?.(已画属性));
    // 汉语：布局及宿主视口变化唤醒同一豫言主循环，普通输入和计时语义保持。文言：布局与宿主视口易则唤同豫言主循环，输入与计时之义皆存。
    let 上尺 = 尺寸值(面), 上比 = 像素比(面);
    const 查尺 = () => {
      const 尺 = 尺寸值(面), 比 = 像素比(面);
      if (尺[0] === 上尺[0] && 尺[1] === 上尺[1] && 比 === 上比) return;
      上尺 = 尺; 上比 = 比; 推({种:事件种类.显示尺寸变化});
    };
    if (typeof 面.窗.ResizeObserver === 'function') {
      const 尺观察 = new 面.窗.ResizeObserver(查尺);
      尺观察.observe(画布); 面.清理.push(() => 尺观察.disconnect());
    }
    if (typeof 面.窗.addEventListener === 'function') 听(面.窗, 'resize', 查尺);
    if (!观察器 && typeof 全局.MutationObserver === 'function') {
      观察器 = new 全局.MutationObserver(查连接);
      观察器.observe(根.documentElement ?? 根, {childList: true, subtree: true});
    }
    // 文言：小窗阖则面闭；还面则闭其窗，主窗不闭。汉语：用户关掉小窗即这块面关闭；归还这块面时关掉小窗，主窗口不关。
    if (面.小窗) {
      if (typeof 面.小窗.addEventListener === 'function') for (const 名 of ['pagehide', 'unload']) 听(面.小窗, 名, () => 关闭());
      面.清理.push(() => { try { if (!面.小窗.closed) 面.小窗.close(); } catch { /* 忽略 */ } });
      面.清理.push(() => { for (const [面名, 开] of 小窗们) if (开.画布 === 画布) 小窗们.delete(面名); });
    }
    return null;
  };
  // 文言：像素比以二整数返之。汉语：像素比 = devicePixelRatio，化成两个整数（乘一百万与一百万）。
  const 像素比值 = 面 => [Math.round(像素比(面) * 1000000), 1000000];
  // 文言：移隐 input 至所示之矩，输入法之候选随之。汉语：把隐形输入框移到应用给的矩形（物理像素，相对显示面左上）处，候选框随之定位。
  const 设输入区域 = (面, 横, 纵, 宽, 高) => {
    const 框 = 面.输入框;
    if (!框?.style) return;
    const 矩 = 面.画布.getBoundingClientRect();
    const 比 = 像素比(面);
    框.style.left = (Number(矩.left) + 横 / 比) + 'px';
    框.style.top = (Number(矩.top) + 纵 / 比) + 'px';
    框.style.height = Math.max(1, 高 / 比) + 'px';
  };
  // 文言：指之形以 CSS 之 cursor 设之。汉语：（0.7.0）指针形状用 canvas 的 CSS cursor：默认 default、文字 text、手形 pointer。
  const 设指针形状 = (面, 形) => { if (面.画布?.style) 面.画布.style.cursor = ['default', 'text', 'pointer'][形] ?? 'default'; };
  // 文言：有区则惟绘其区；画布之像素数方改（改则清屏）则绘全帧。汉语：（0.8.0）有区域时 putImageData 只画这块；画布像素数刚改过（改了会清空画布）时画整帧。
  const 画 = (面, 宽, 高, 字节, 区) => {
    if (!面.二维) {
      const 境 = 面.画布.getContext('2d');
      if (!境) return {码: 码.暂不可用, 文: '显示面不能取得 2d 绘图上下文'};
      面.二维 = 境;
    }
    let 全帧 = !区;
    if (面.画布.width !== 宽) { 面.画布.width = 宽; 全帧 = true; }
    if (面.画布.height !== 高) { 面.画布.height = 高; 全帧 = true; }
    const 图类 = 窗素(面).ImageData ?? 全局.ImageData;
    const 图 = new 图类(new Uint8ClampedArray(字节.buffer, 字节.byteOffset, 字节.length), 宽, 高);
    if (全帧) 面.二维.putImageData(图, 0, 0);
    else 面.二维.putImageData(图, 0, 0, 区[0], 区[1], 区[2], 区[3]);
    if (面.画布.hasAttribute?.(已画属性) === false) 面.画布.setAttribute(已画属性, '');
    return null;
  };
  // 文言：题面者，设小窗之题；主画布所在之页不更，不能亦成。汉语：设置小窗的标题；主画布所在的页面不改，做不到也照常成功。
  const 题 = (面, 题文) => {
    const 窗 = 面.小窗;
    if (!窗) return null;
    try { if (窗.document) 窗.document.title = 题文; } catch { /* 忽略 */ }
    return null;
  };
  return {
    找: 名 => 找画布(名) ?? {码: 码.未获授权, 文: '页面没有授予名为「' + 名 + '」的显示面（需要带 ' + 显示面属性 + ' 属性的 canvas，或带 ' + 显示面前缀属性 + ' 属性的前缀授权）'},
    同源: (面, 画布) => 面.画布 === 画布,
    已断开: 面 => 面.画布.isConnected === false || 面.小窗?.closed === true,
    建,
    尺寸值,
    像素比值,
    设输入区域,
    设指针形状,
    画,
    题,
    清理: () => {
      观察器?.disconnect();
      观察器 = null;
      for (const {窗} of 小窗们.values()) { try { if (!窗.closed) 窗.close(); } catch { /* 忽略 */ } }
      小窗们.clear();
    }
  };
}
