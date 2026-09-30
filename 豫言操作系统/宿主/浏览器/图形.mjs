// 文言：显示面与图形处理器之宿主术。显示面者，页中带 data-yy-显示面 之 canvas，以其属性之值为名而授之；图形处理器取 WebGPU 之要。二者皆纯工厂，凭注入之根与全局而行，可离 Wasm 而测。
//       显示面之表与图形之能不系于文树，节点之宿主亦内联此篇而共用之：换以 SDL 之窗为面源，以 Dawn 为图形处理器，别供其呈现之器。
// 汉语：显示、图形处理器、图形显示三个接口的浏览器宿主实现。显示面由页面授予：带 data-yy-显示面="名称" 属性的 <canvas> 即以该名称授予本次运行；
//       图形处理器用 WebGPU 核心子集。两个工厂只依赖注入的 document 与全局对象，可以脱离 Wasm 单独测试；创建浏览器宿主把它们接到
//       豫言_浏览器_显示 与 豫言_浏览器_图形 两个原语上。
//       Node 宿主（路一）也内联本文件共用其中与 DOM 无关的部分：显示面表（创建显示面表）与图形能力（创建图形能力）；它换用 SDL 窗口作
//       显示面的“后端”，用 Dawn（@kmamal/gpu）作 GPU，并提供自己的“呈现器”，见 豫言操作系统/宿主/节点/图形.mjs。本文件因此不写 import。

// 文言：返客之码，适配依此转为公误。汉语：返回给适配的状态码：0 成功；1 未获授权、2 资源暂不可用、3 资源已失效、4 资源不存在、5 资源已存在、
//       6 资源配额已尽、7 输入无效、8 宿主操作失败（附消息）；9 图形校验错误（附诊断文字）；10 非法资源使用（适配据此终止本次运行）。
export const 状态码 = Object.freeze({成: 0, 未获授权: 1, 暂不可用: 2, 已失效: 3, 不存在: 4, 已存在: 5, 配额已尽: 6, 输入无效: 7, 宿主失败: 8, 校验错误: 9, 非法使用: 10});
const 码 = 状态码;
// 文言：一值过桥不逾十六兆。汉语：值桥单值上限 16 MiB；图形处理器的“单次写入或读回的最大字节数”即此数。
export const 单次交换上限 = 16 * 1024 * 1024;
export const 显示面属性 = 'data-yy-显示面';
export const 显示面输入属性 = 'data-yy-显示面输入';

const 解码器 = new TextDecoder('utf-8', {ignoreBOM: true});
const 文字化 = 值 => (值 instanceof Uint8Array ? 解码器.decode(值) : String(值 ?? ''));
const 消息 = 错 => String(错?.message ?? 错);
const 空字节 = new Uint8Array();

// ---------------------------------------------------------------------------
// 一、显示：已授显示面、尺寸、像素提交、输入事件
// ---------------------------------------------------------------------------
// 文言：语义之键名，首版七者；单一码点之键亦许，余皆不报。汉语：规范首版固定的七个语义键名；event.key 是单个 Unicode 标量值时原样交付，其他键（Shift、Tab、F1……）首版不交付。
const 键名表 = Object.freeze({ArrowLeft: '左', ArrowRight: '右', ArrowUp: '上', ArrowDown: '下', Enter: '回车', Backspace: '退格', ' ': '空格'});
export const 译键 = 键 => {
  if (typeof 键 !== 'string') return null;
  if (Object.hasOwn(键名表, 键)) return 键名表[键];
  return [...键].length === 1 ? 键 : null;
};
// 文言：事之种：一按下、二抬起、三文字、四移、五指按、六指抬、七面闭。汉语：事件种类编号，与适配的转换一一对应。
export const 事件种类 = Object.freeze({按键按下: 1, 按键抬起: 2, 文字输入: 3, 指针移动: 4, 指针按下: 5, 指针抬起: 6, 显示面已关闭: 7});
// 文言：每面之事列，限额从大。汉语：每个显示面未取走事件的上限；满时丢弃最旧事件。待办事项：丢弃计数暂不报告。
const 显示事件上限 = 65536;

// 文言：显示面之表，不系于宿主：取面之果、尺寸、提交之验、每面之事列与候者、面闭之序皆在此；面从何来、尺寸何量、素何以画、事何以入，由宿主之后端供之。
// 汉语：显示面表，与宿主无关的部分：取得显示面的结果、尺寸、像素提交的校验、每个显示面的事件队列与等候者、关闭流程。
//       显示面从哪里来、尺寸怎么量、像素怎么画、输入从哪里来，由宿主的“后端”对象提供（浏览器是 canvas，Node 是 SDL 窗口）：
//       找(名) → {值} 或 {码, 文}；同源(面, 值) → 是否同一来源；建(面, 值, {推, 关闭}) → 失败时 {码, 文}，成功时把清理函数放进 面.清理；
//       已断开(面)；尺寸值(面) → [宽, 高]（物理像素）；画(面, 宽, 高, 字节) → 失败时 {码, 文}；清理()。
export function 创建显示面表({后端, 已关闭 = () => false}) {
  const 面表 = new Map();
  let 下号 = 1;
  const 结果 = (码值, 甲 = 0, 乙 = 0, 丙 = 0, 丁 = 0, 文 = '') => [码值, 甲, 乙, 丙, 丁, 文];
  const 错 = (码值, 文 = '') => 结果(码值, 0, 0, 0, 0, 文);
  const 事果 = 事 => 结果(码.成, 事.种, 事.甲 ?? 0, 事.乙 ?? 0, 事.丙 ?? 0, 事.文 ?? '');
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
  const 尺寸 = 号 => {
    const 面 = 取开面(号);
    if (!面 || 面.已关闭) return 错(码.已失效, '显示面已失效');
    const [宽, 高] = 后端.尺寸值(面);
    if (!(宽 > 0 && 高 > 0)) return 错(码.暂不可用, '显示面暂时没有正尺寸');
    return 结果(码.成, 宽, 高);
  };
  const 提交 = (号, 宽, 高, 字节) => {
    const 面 = 取开面(号);
    if (!面 || 面.已关闭) return 错(码.已失效, '显示面已失效');
    if (!Number.isSafeInteger(宽) || !Number.isSafeInteger(高) || 宽 <= 0 || 高 <= 0) return 错(码.输入无效, '画面宽高须为正整数');
    const 应长 = 宽 * 高 * 4;
    if (!Number.isSafeInteger(应长) || !(字节 instanceof Uint8Array) || 字节.length !== 应长) return 错(码.输入无效, '画面字节数须恰为宽×高×4');
    if (面.模式 === '图形') return 错(码.暂不可用, '此显示面已用于图形处理器呈现');
    const [现宽, 现高] = 后端.尺寸值(面);
    if (现宽 !== 宽 || 现高 !== 高) return 错(码.暂不可用, '提交尺寸与显示面当前尺寸不同');
    const 败 = 后端.画(面, 宽, 高, 字节);
    if (败) return 错(败.码, 败.文);
    面.模式 = '二维';
    return 结果(码.成);
  };
  const 等待 = 号 => {
    const 面 = 取开面(号);
    if (!面 || 面.已报关闭) return 错(码.已失效, '显示面已关闭');
    if (面.队列.length) return 事果(面.队列.shift());
    if (面.已关闭) {
      面.已报关闭 = true;
      return 事果({种: 事件种类.显示面已关闭});
    }
    if (面.等待者) return 错(码.输入无效, '同一显示面已有等候者');
    return new Promise(完成 => { 面.等待者 = 完成; });
  };
  // 文言：一口而分诸术；异常皆归码，不越桥。汉语：原语分派；同步异常都转成宿主操作失败，不让 JS 异常越过 Wasm。
  const 调用 = (操作, 号, 甲, 乙, 字节) => {
    try {
      switch (操作) {
        case '取得': return 取得(文字化(字节));
        case '尺寸': return 尺寸(号);
        case '提交': return 提交(号, 甲, 乙, 字节);
        case '等待': return 等待(号);
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
function 创建画布后端({根, 全局}) {
  let 观察器 = null;
  const 活面 = new Map();
  const 像素比 = () => {
    const 比 = Number(全局.devicePixelRatio);
    return Number.isFinite(比) && 比 > 0 ? 比 : 1;
  };
  // 文言：尺以物理之素计：CSS 之宽高乘像素比而四舍五入。汉语：显示面尺寸按物理像素计：CSS 布局宽高乘 devicePixelRatio 后四舍五入。
  //       待办事项：canvas 带边框或内边距时布局矩形含它们，应改用 ResizeObserver 的 devicePixelContentBoxSize。
  const 尺寸值 = 面 => {
    const 矩 = 面.画布.getBoundingClientRect();
    const 比 = 像素比();
    return [Math.round(Number(矩.width) * 比), Math.round(Number(矩.height) * 比)];
  };
  const 找画布 = 名 => {
    for (const 元 of 根.querySelectorAll?.('canvas') ?? []) if (元.getAttribute(显示面属性) === 名) return 元;
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
    面.输入框 = null;
    面.二维 = null;
    面.组字中 = false;
    活面.set(面, 关闭);
    面.清理.push(() => 活面.delete(面));
    定样式尺寸(画布);
    const 听 = (目标, 名称, 处理) => {
      目标.addEventListener(名称, 处理);
      面.清理.push(() => 目标.removeEventListener(名称, 处理));
    };
    // 文言：canvas 不能受输入法之文，故旁置一隐 input 以受键与文字；点 canvas 则聚焦之。
    // 汉语：canvas 本身不能接收输入法文字，宿主在它后面插入一个透明的 <input>（带 data-yy-显示面输入="名称"）接收按键与文字；点击 canvas 时聚焦它。
    //       待办事项：输入法候选框位置固定在视口左上角，应跟随应用的光标位置。
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
    const 坐标 = 事件 => {
      const 矩 = 画布.getBoundingClientRect();
      const 比 = 像素比();
      return [Math.floor((Number(事件.clientX) - Number(矩.left)) * 比), Math.floor((Number(事件.clientY) - Number(矩.top)) * 比)];
    };
    听(画布, 'pointerdown', 事件 => {
      const [甲, 乙] = 坐标(事件);
      try { 输入框.focus({preventScroll: true}); } catch { /* 忽略 */ }
      try { 画布.setPointerCapture?.(事件.pointerId); } catch { /* 文言：合成之事无活指针。汉语：合成事件没有活动指针，捕获失败无妨。 */ }
      推({种: 事件种类.指针按下, 甲, 乙, 丙: Number(事件.button) || 0});
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
    const 按键 = 种 => 事件 => {
      if (事件.isComposing || 事件.keyCode === 229 || 面.组字中) return;
      const 键 = 译键(事件.key);
      if (键 !== null) 推({种, 文: 键});
    };
    听(输入框, 'keydown', 按键(事件种类.按键按下));
    听(输入框, 'keyup', 按键(事件种类.按键抬起));
    // 文言：成文则交：组字之中不交，组字毕乃交其全文。汉语：已提交的文字才交付：组字期间的 input 事件不交付，compositionend 时交付输入框里的全文。
    const 交文字 = 备用 => {
      const 文 = 输入框.value || 备用 || '';
      输入框.value = '';
      if (文) 推({种: 事件种类.文字输入, 文});
    };
    听(输入框, 'compositionstart', () => { 面.组字中 = true; });
    听(输入框, 'compositionend', 事件 => { 面.组字中 = false; 交文字(typeof 事件.data === 'string' ? 事件.data : ''); });
    听(输入框, 'input', 事件 => { if (!面.组字中 && !事件.isComposing) 交文字(''); });
    // 文言：先撤诸听，后去输入框。汉语：关闭时先移除监听，再移除输入框。
    面.清理.push(() => 输入框.remove());
    if (!观察器 && typeof 全局.MutationObserver === 'function') {
      观察器 = new 全局.MutationObserver(查连接);
      观察器.observe(根.documentElement ?? 根, {childList: true, subtree: true});
    }
    return null;
  };
  const 画 = (面, 宽, 高, 字节) => {
    if (!面.二维) {
      const 境 = 面.画布.getContext('2d');
      if (!境) return {码: 码.暂不可用, 文: '显示面不能取得 2d 绘图上下文'};
      面.二维 = 境;
    }
    if (面.画布.width !== 宽) 面.画布.width = 宽;
    if (面.画布.height !== 高) 面.画布.height = 高;
    面.二维.putImageData(new 全局.ImageData(new Uint8ClampedArray(字节.buffer, 字节.byteOffset, 字节.length), 宽, 高), 0, 0);
    return null;
  };
  return {
    找: 名 => {
      const 画布 = 找画布(名);
      return 画布 ? {值: 画布} : {码: 码.未获授权, 文: '页面没有授予名为「' + 名 + '」的显示面（需要带 ' + 显示面属性 + ' 属性的 canvas）'};
    },
    同源: (面, 画布) => 面.画布 === 画布,
    已断开: 面 => 面.画布.isConnected === false,
    建,
    尺寸值,
    画,
    清理: () => {
      观察器?.disconnect();
      观察器 = null;
    }
  };
}

export function 创建显示能力({根, 全局, 已关闭 = () => false}) {
  return 创建显示面表({后端: 创建画布后端({根, 全局}), 已关闭});
}

// ---------------------------------------------------------------------------
// 二、图形处理器（WebGPU 核心子集）与图形显示桥
// ---------------------------------------------------------------------------
// 文言：用途与映射之位，以数书之，免依全局常量。汉语：WebGPU 用途位与映射位直接写数值，测试用的伪 GPU 也能用。
const 缓冲位 = Object.freeze({映读: 1, 复源: 4, 复的: 8, 索引: 16, 顶点: 32, 均匀: 64, 存储: 128});
const 纹理位 = Object.freeze({复源: 1, 复的: 2, 采样: 4, 存储: 8, 渲染: 16});
const 映读 = 1;
const 缓冲用途表 = Object.freeze({存储: 缓冲位.存储, 均匀: 缓冲位.均匀, 顶点: 缓冲位.顶点, 索引: 缓冲位.索引});
const 纹理用途表 = Object.freeze({采样: 纹理位.采样, 存储: 纹理位.存储, 渲染: 纹理位.渲染});
export const 纹理每像素 = Object.freeze({'rgba8unorm': 4, 'rgba8unorm-srgb': 4, 'bgra8unorm': 4, 'bgra8unorm-srgb': 4, 'rgba16float': 8, 'r32float': 4});
const 存储纹理格式 = new Set(['rgba8unorm', 'rgba16float', 'r32float']);
const 顶点格式集 = new Set(['float32', 'float32x2', 'float32x3', 'float32x4', 'unorm8x4']);
const 拓扑集 = new Set(['triangle-list', 'triangle-strip', 'line-list', 'point-list']);
const 过滤集 = new Set(['nearest', 'linear']);
const 寻址集 = new Set(['clamp-to-edge', 'repeat', 'mirror-repeat']);
// 文言：混合三式，依规范之式。汉语：三种混合方式；透明混合的透明度为 源 + 目标 × (1 − 源透明度)，相加混合颜色与透明度都是 源 + 目标。
const 混合表 = Object.freeze({
  不混合: undefined,
  透明: {color: {srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add'}, alpha: {srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add'}},
  相加: {color: {srcFactor: 'one', dstFactor: 'one', operation: 'add'}, alpha: {srcFactor: 'one', dstFactor: 'one', operation: 'add'}}
});
// 文言：设备所求之限，取适配器之极。汉语：取得设备时按适配器的最大值申请这些限额（限额尽量取大）。
const 申请限额名 = ['maxBufferSize', 'maxStorageBufferBindingSize', 'maxTextureDimension2D', 'maxComputeWorkgroupsPerDimension',
  'maxComputeInvocationsPerWorkgroup', 'maxComputeWorkgroupSizeX', 'maxComputeWorkgroupSizeY', 'maxComputeWorkgroupSizeZ', 'maxComputeWorkgroupStorageSize'];
const 非负整 = 值 => Number.isSafeInteger(值) && 值 >= 0;
const 正整 = 值 => Number.isSafeInteger(值) && 值 > 0;

// 文言：浏览器之呈现器：帧纹理于呈现时复于 canvas 之当前纹理，候下一画帧而返。
// 汉语：浏览器的呈现器（创建图形能力缺省用它）：第一次取帧时给 canvas 取 webgpu 上下文并以首选格式 configure；呈现时把帧纹理复制到
//       canvas 当前纹理（getCurrentTexture）并提交，再等下一次 requestAnimationFrame 之后返回。
//       呈现器的形状：首选格式(面)；绑定(面, 设备) → 失败时 {码, 文}，成功时把帧纹理格式记在 面.呈现格式；备帧(面, 宽, 高)；交帧(面, 设备, 帧) → Promise。
function 创建画布呈现器({全局}) {
  const 首选格式 = () => {
    try { return 全局.navigator?.gpu?.getPreferredCanvasFormat?.() ?? 'bgra8unorm'; } catch { return 'bgra8unorm'; }
  };
  const 等下一帧 = () => new Promise(完成 => {
    const 后 = () => 全局.setTimeout(完成, 0);
    if (typeof 全局.requestAnimationFrame === 'function') 全局.requestAnimationFrame(后);
    else 全局.setTimeout(完成, 16);
  });
  return {
    首选格式,
    绑定: (面, 设备) => {
      if (!面.图形上下文) {
        const 境 = 面.画布.getContext('webgpu');
        if (!境) return {码: 码.暂不可用, 文: '显示面不能取得 webgpu 上下文'};
        面.图形上下文 = 境;
      }
      面.呈现格式 = 首选格式();
      面.图形上下文.configure({device: 设备, format: 面.呈现格式, usage: 纹理位.渲染 | 纹理位.复的, alphaMode: 'opaque'});
      return null;
    },
    备帧: (面, 宽, 高) => {
      if (面.画布.width !== 宽) 面.画布.width = 宽;
      if (面.画布.height !== 高) 面.画布.height = 高;
    },
    交帧: async (面, 设备, 帧) => {
      const 画纹理 = 面.图形上下文.getCurrentTexture();
      const 编 = 设备.createCommandEncoder();
      编.copyTextureToTexture({texture: 帧.物}, {texture: 画纹理},
        [Math.min(帧.宽, 画纹理.width ?? 帧.宽), Math.min(帧.高, 画纹理.height ?? 帧.高)]);
      设备.queue.submit([编.finish()]);
      await 等下一帧();
    }
  };
}

// 文言：图形之能：取器之术由 取图形 注入（浏览器为 navigator.gpu，节点为 Dawn），呈现之器可换，取视图之法亦可换。
// 汉语：图形处理器与图形显示桥。取图形() 返回 WebGPU 的 GPU 对象（浏览器缺省取 navigator.gpu；Node 注入 Dawn 的实例，取不到时抛出或返回空）；
//       呈现器缺省为浏览器 canvas 的呈现器，Node 注入 SDL 窗口的呈现器。取视图(纹理) 给渲染附件与纹理绑定用，缺省为 createView()；
//       Dawn（@kmamal/gpu 0.2.1）的 JS createView() 无论传什么描述都带上 swizzle 而校验失败，Node 改为直接传纹理（WebGPU 允许在要视图处传纹理）。
export function 创建图形能力({全局, 显示 = null, 取图形 = () => 全局.navigator?.gpu, 呈现器 = null, 取视图 = 纹 => 纹.createView()}) {
  const 器 = 呈现器 ?? 创建画布呈现器({全局});
  const 物表 = new Map();
  let 下号 = 1;
  const 果 = (码值, 号 = 0, 文 = '', 字节 = 空字节) => [码值, 号, String(文), 字节];
  const 失 = (码值, 文) => 果(码值, 0, 文);
  const 登记 = (种类, 设备号, 物, 额外 = {}) => {
    const 号 = 下号++;
    物表.set(号, {种类, 设备号, 物, 失效: false, ...额外});
    return 号;
  };
  // 文言：求物：种不合或伪号为非法；己失效、设备释放或丢失为已失效。汉语：查对象：伪造或种类不符的句柄是非法使用；对象已销毁、设备已释放或已丢失是资源已失效。
  const 查 = (号, 种类) => {
    const 项 = 物表.get(Number(号));
    if (!项 || (种类 && !种类.split('|').includes(项.种类))) return {码: 码.非法使用, 文: '图形句柄无效或种类不符：' + String(号)};
    const 设 = 项.种类 === '设备' ? 项 : 物表.get(项.设备号);
    if (项.失效) return {码: 码.已失效, 文: 项.种类 + '已失效', 项, 设};
    if (设.失效) return {码: 码.已失效, 文: '设备已释放', 项, 设};
    if (设.丢失 !== null) return {码: 码.已失效, 文: '设备已丢失：' + 设.丢失, 项, 设, 丢失: true};
    return {项, 设};
  };
  // 文言：同一设备之约。汉语：绑定组与命令里的对象须属于同一设备。
  const 同设备 = (查果, 设号) => 查果.项.设备号 === 设号;
  const 异常归类 = 错误 => {
    if (错误?.name === 'GPUPipelineError' && 错误?.reason === 'internal') return {码: 码.宿主失败, 文: 消息(错误)};
    return {码: 码.校验错误, 文: 消息(错误)};
  };
  // 文言：以误域包一事：校验之误为图形校验错误，存储之误为配额已尽，同步之异常亦为校验之误。
  // 汉语：用 WebGPU 错误域包住一次创建或录制：校验错误返回图形校验错误，内存不足返回资源配额已尽，同步抛出的异常（描述字段非法等）也按校验错误处理。
  //       Dawn（Node）把没被错误域接住的错误直接打印到标准输出，所以凡可能出错的调用都要包住。两个错误域同时弹出、一起等：
  //       @kmamal/gpu 每约 100 毫秒才处理一次异步回调，分两次等要多花一倍时间。
  const 包校验 = async (设备, 函) => {
    设备.pushErrorScope('out-of-memory');
    设备.pushErrorScope('validation');
    let 值;
    let 异常 = null;
    try { 值 = await 函(); } catch (错误) { 异常 = 错误; }
    const [校误, 存误] = await Promise.all([设备.popErrorScope().catch(错误 => 错误), 设备.popErrorScope().catch(错误 => 错误)]);
    if (异常) return {误: 异常归类(异常)};
    if (校误) return {误: {码: 码.校验错误, 文: 消息(校误)}};
    if (存误) return {误: {码: 码.配额已尽, 文: 消息(存误)}};
    return {值};
  };
  const 丢失后 = (设, 结果) => (设.丢失 !== null || 设.失效 ? 失(码.已失效, 设.失效 ? '设备已释放' : '设备已丢失：' + 设.丢失) : 结果);

  const 取得设备 = async () => {
    let 图形 = null;
    try { 图形 = 取图形(); } catch (错误) { return 失(码.暂不可用, 消息(错误)); }
    if (!图形) return 失(码.暂不可用, '此宿主没有 WebGPU');
    let 适配器 = null;
    try { 适配器 = await 图形.requestAdapter(); } catch (错误) { return 失(码.暂不可用, 消息(错误)); }
    if (!适配器) return 失(码.暂不可用, '没有可用的图形处理器');
    const 要求 = {};
    for (const 名 of 申请限额名) {
      const 值 = 适配器.limits?.[名];
      if (typeof 值 === 'number' && 值 > 0) 要求[名] = 值;
    }
    let 设备;
    try { 设备 = await 适配器.requestDevice({requiredLimits: 要求}); }
    catch {
      try { 设备 = await 适配器.requestDevice(); } catch (错误) { return 失(码.暂不可用, 消息(错误)); }
    }
    const 号 = 登记('设备', 0, 设备, {丢失: null});
    const 项 = 物表.get(号);
    项.设备号 = 号;
    Promise.resolve(设备.lost).then(信息 => {
      if (信息 && 项.丢失 === null) 项.丢失 = String(信息.message || 信息.reason || '设备已丢失');
    }, () => {});
    return 果(码.成, 号);
  };
  const 设备限额 = 参 => {
    const 项 = 物表.get(Number(参.设备));
    if (!项 || 项.种类 !== '设备' || 项.失效) return 失(码.非法使用, '设备句柄无效或已释放');
    const 限 = 项.物.limits;
    return 果(码.成, 0, JSON.stringify([限.maxBufferSize, 限.maxStorageBufferBindingSize, 限.maxTextureDimension2D,
      限.maxComputeWorkgroupsPerDimension, 限.maxComputeInvocationsPerWorkgroup, 单次交换上限]));
  };
  const 设备状态 = 参 => {
    const 项 = 物表.get(Number(参.设备));
    if (!项 || 项.种类 !== '设备') return 失(码.非法使用, '设备句柄无效');
    if (项.失效) return 果(码.成, 1, '设备已释放');
    if (项.丢失 !== null) return 果(码.成, 1, 项.丢失);
    return 果(码.成, 0);
  };
  const 释放设备 = 参 => {
    const 号 = Number(参.设备);
    const 项 = 物表.get(号);
    if (!项 || 项.种类 !== '设备') return 失(码.非法使用, '设备句柄无效');
    if (!项.失效) {
      项.失效 = true;
      for (const 物 of 物表.values()) if (物.设备号 === 号) 物.失效 = true;
      try { 项.物.destroy(); } catch { /* 忽略 */ }
    }
    return 果(码.成);
  };
  const 等待完成 = async 参 => {
    const 项 = 物表.get(Number(参.设备));
    if (!项 || 项.种类 !== '设备') return 失(码.非法使用, '设备句柄无效');
    if (项.失效 || 项.丢失 !== null) return 果(码.成);
    try { await 项.物.queue.onSubmittedWorkDone(); } catch { /* 文言：设备丢失则无可候。汉语：设备丢失时没有可等待的工作。 */ }
    return 果(码.成);
  };

  const 新建缓冲 = async 参 => {
    const 查果 = 查(参.设备, '设备');
    if (查果.码) return 失(查果.码, 查果.文);
    const 设 = 查果.项;
    const 大小 = 参.大小;
    const 用途 = 参.用途;
    if (!正整(大小) || 大小 % 4 !== 0) return 失(码.校验错误, '缓冲字节数须是 4 的正倍数');
    if (大小 > 设.物.limits.maxBufferSize) return 失(码.校验错误, '缓冲字节数超过设备限额');
    if (!Array.isArray(用途) || 用途.length === 0 || 用途.some(名 => !Object.hasOwn(缓冲用途表, 名))) return 失(码.校验错误, '缓冲用途须为非空列表');
    let 位 = 缓冲位.复源 | 缓冲位.复的;
    for (const 名 of 用途) 位 |= 缓冲用途表[名];
    const {值, 误} = await 包校验(设.物, () => 设.物.createBuffer({size: 大小, usage: 位}));
    if (误) return 失(误.码, 误.文);
    return 丢失后(设, 果(码.成, 登记('缓冲', 设.设备号, 值, {大小})));
  };
  // 文言：写与毁无错支：伪号或已毁者终止运行；设备丢失则默然略过。汉语：写入与销毁没有错误分支：伪造、已销毁或设备已释放的对象属非法使用（适配终止运行）；
  //       设备已丢失时静默忽略（设备丢失是异步发生的，应用无法事先避免）。
  const 查写 = (号, 种类) => {
    const 查果 = 查(号, 种类);
    if (!查果.码) return 查果;
    if (查果.丢失 && !查果.项.失效) return {忽略: true};
    return {码: 码.非法使用, 文: 查果.文};
  };
  const 写入缓冲 = (参, 字节) => {
    const 查果 = 查写(参.缓冲, '缓冲');
    if (查果.忽略) return 果(码.成);
    if (查果.码) return 失(查果.码, 查果.文);
    const 偏移 = 参.偏移;
    const 长 = 字节.length;
    if (!非负整(偏移) || 偏移 % 4 || 长 % 4 || 偏移 + 长 > 查果.项.大小 || 长 > 单次交换上限)
      return 失(码.非法使用, '写入图形缓冲的偏移或长度不合规（须为 4 的倍数且在缓冲范围与单次上限之内）');
    查果.设.物.queue.writeBuffer(查果.项.物, 偏移, 字节);
    return 果(码.成);
  };
  // 文言：读回：新建映读之暂存，录复制而交之，候映射乃取其字；录与交亦包于误域，不合则为校验之误。
  // 汉语：读回：新建 MAP_READ | COPY_DST 暂存缓冲，录制复制并提交，映射后取出字节；录制与提交也用错误域包住，校验不通过（如纹理没有复制源用途）
  //       返回 {误}，不再读出全零。
  const 暂存读 = async (设备, 录制, 长度) => {
    let 暂 = null;
    try {
      const {误} = await 包校验(设备, () => {
        暂 = 设备.createBuffer({size: 长度, usage: 缓冲位.映读 | 缓冲位.复的});
        const 编 = 设备.createCommandEncoder();
        录制(编, 暂);
        设备.queue.submit([编.finish()]);
      });
      if (误) return {误};
      await 暂.mapAsync(映读);
      const 字节 = new Uint8Array(暂.getMappedRange().slice(0));
      暂.unmap();
      return {字节};
    } finally { try { 暂?.destroy(); } catch { /* 忽略 */ } }
  };
  const 读回缓冲 = async 参 => {
    const 查果 = 查(参.缓冲, '缓冲');
    if (查果.码) return 失(查果.码, 查果.文);
    const 偏移 = 参.偏移;
    const 长度 = 参.长度;
    if (!非负整(偏移) || !非负整(长度) || 偏移 % 4 || 长度 % 4 || 偏移 + 长度 > 查果.项.大小)
      return 失(码.校验错误, '读回的偏移与长度须是 4 的倍数且在缓冲范围之内');
    if (长度 > 单次交换上限) return 失(码.校验错误, '读回长度超过单次交换上限 16 MiB');
    if (长度 === 0) return 果(码.成);
    try {
      const {字节, 误} = await 暂存读(查果.设.物, (编, 暂) => 编.copyBufferToBuffer(查果.项.物, 偏移, 暂, 0, 长度), 长度);
      if (误) return 丢失后(查果.设, 失(误.码, 误.文));
      return 果(码.成, 0, '', 字节);
    } catch (错误) { return 丢失后(查果.设, 失(码.宿主失败, 消息(错误))); }
  };
  const 销毁缓冲 = 参 => {
    const 项 = 物表.get(Number(参.缓冲));
    if (!项 || 项.种类 !== '缓冲') return 失(码.非法使用, '缓冲句柄无效');
    if (!项.失效) {
      项.失效 = true;
      try { 项.物.destroy(); } catch { /* 忽略 */ }
    }
    return 果(码.成);
  };

  const 新建纹理 = async 参 => {
    const 查果 = 查(参.设备, '设备');
    if (查果.码) return 失(查果.码, 查果.文);
    const 设 = 查果.项;
    const {宽, 高, 格式, 用途} = 参;
    const 边 = 设.物.limits.maxTextureDimension2D;
    if (!正整(宽) || !正整(高) || 宽 > 边 || 高 > 边) return 失(码.校验错误, '纹理宽高须在 1 至 ' + 边 + ' 之间');
    if (!Object.hasOwn(纹理每像素, 格式)) return 失(码.校验错误, '纹理格式无效');
    if (!Array.isArray(用途) || 用途.length === 0 || 用途.some(名 => !Object.hasOwn(纹理用途表, 名))) return 失(码.校验错误, '纹理用途须为非空列表');
    if (用途.includes('存储') && !存储纹理格式.has(格式)) return 失(码.校验错误, '存储纹理用途只允许 rgba8unorm、rgba16float、r32float');
    let 位 = 纹理位.复源 | 纹理位.复的;
    for (const 名 of 用途) 位 |= 纹理用途表[名];
    const {值, 误} = await 包校验(设.物, () => 设.物.createTexture({size: [宽, 高], format: 格式, usage: 位}));
    if (误) return 失(误.码, 误.文);
    return 丢失后(设, 果(码.成, 登记('纹理', 设.设备号, 值, {宽, 高, 格式, 每像素: 纹理每像素[格式]})));
  };
  const 写入纹理 = (参, 字节) => {
    const 查果 = 查写(参.纹理, '纹理');
    if (查果.忽略) return 果(码.成);
    if (查果.码) return 失(查果.码, 查果.文);
    const 纹 = 查果.项;
    const 行 = 纹.宽 * 纹.每像素;
    if (字节.length !== 行 * 纹.高 || 字节.length > 单次交换上限) return 失(码.非法使用, '写入图形纹理的字节数须恰为 宽×高×每像素字节 且不超过单次上限');
    查果.设.物.queue.writeTexture({texture: 纹.物}, 字节, {bytesPerRow: 行, rowsPerImage: 纹.高}, [纹.宽, 纹.高]);
    return 果(码.成);
  };
  // 文言：纹理读回，行须齐于二百五十六字；读毕去其衬。汉语：纹理复制到缓冲时每行字节数须按 256 对齐，读回后去掉每行末尾的填充，得到紧密排列的字节。
  const 读回纹理 = async 参 => {
    const 查果 = 查(参.纹理, '纹理');
    if (查果.码) return 失(查果.码, 查果.文);
    const 纹 = 查果.项;
    const 行 = 纹.宽 * 纹.每像素;
    if (行 * 纹.高 > 单次交换上限) return 失(码.校验错误, '纹理读回超过单次交换上限 16 MiB');
    const 对齐 = Math.ceil(行 / 256) * 256;
    try {
      const {字节: 原, 误} = await 暂存读(查果.设.物, (编, 暂) => 编.copyTextureToBuffer({texture: 纹.物}, {buffer: 暂, bytesPerRow: 对齐, rowsPerImage: 纹.高},
        [纹.宽, 纹.高]), 对齐 * 纹.高);
      if (误) return 丢失后(查果.设, 失(误.码, 误.文));
      const 紧 = new Uint8Array(行 * 纹.高);
      for (let 序 = 0; 序 < 纹.高; 序++) 紧.set(原.subarray(序 * 对齐, 序 * 对齐 + 行), 序 * 行);
      return 果(码.成, 0, '', 紧);
    } catch (错误) { return 丢失后(查果.设, 失(码.宿主失败, 消息(错误))); }
  };
  const 销毁纹理 = 参 => {
    const 项 = 物表.get(Number(参.纹理));
    if (!项 || 项.种类 !== '纹理') return 失(码.非法使用, '纹理句柄无效');
    if (!项.失效) {
      项.失效 = true;
      // 文言：显示面之帧纹理共用离屏之物，惟失其号而不毁。汉语：显示面帧纹理共用宿主的离屏纹理，只作废句柄，不销毁底层纹理。
      if (!项.帧) { try { 项.物.destroy(); } catch { /* 忽略 */ } }
    }
    return 果(码.成);
  };
  const 新建采样器 = 参 => {
    const 项 = 物表.get(Number(参.设备));
    if (!项 || 项.种类 !== '设备' || 项.失效) return 失(码.非法使用, '设备句柄无效或已释放');
    if (!过滤集.has(参.过滤) || !寻址集.has(参.寻址)) return 失(码.非法使用, '采样器参数无效');
    const 物 = 项.物.createSampler({magFilter: 参.过滤, minFilter: 参.过滤, mipmapFilter: 参.过滤,
      addressModeU: 参.寻址, addressModeV: 参.寻址, addressModeW: 参.寻址});
    return 果(码.成, 登记('采样器', 项.设备号, 物));
  };
  const 编译着色 = async (参, 字节) => {
    const 查果 = 查(参.设备, '设备');
    if (查果.码) return 失(查果.码, 查果.文);
    const 设备 = 查果.项.物;
    let 信息 = null;
    const {值, 误} = await 包校验(设备, async () => {
      const 模块 = 设备.createShaderModule({code: 文字化(字节)});
      try { 信息 = await 模块.getCompilationInfo(); } catch { /* 文言：无编译之讯则从误域。汉语：取不到编译信息时以错误域为准。 */ }
      return 模块;
    });
    const 诊断 = (信息?.messages ?? []).filter(项 => 项.type === 'error').map(项 => `第 ${项.lineNum} 行第 ${项.linePos} 列：${项.message}`).join('\n');
    if (诊断) return 失(码.校验错误, 诊断);
    if (误) return 失(误.码, 误.文);
    return 丢失后(查果.项, 果(码.成, 登记('着色模块', 查果.项.设备号, 值)));
  };
  const 新建计算管线 = async 参 => {
    const 设查 = 查(参.设备, '设备');
    if (设查.码) return 失(设查.码, 设查.文);
    const 模查 = 查(参.模块, '着色模块');
    if (模查.码) return 失(模查.码, 模查.文);
    if (!同设备(模查, 设查.项.设备号)) return 失(码.校验错误, '着色模块不属于此设备');
    const 设备 = 设查.项.物;
    const {值, 误} = await 包校验(设备, () => 设备.createComputePipelineAsync({layout: 'auto', compute: {module: 模查.项.物, entryPoint: String(参.入口)}}));
    if (误) return 失(误.码, 误.文);
    return 丢失后(设查.项, 果(码.成, 登记('计算管线', 设查.项.设备号, 值)));
  };
  const 新建渲染管线 = async 参 => {
    const 设查 = 查(参.设备, '设备');
    if (设查.码) return 失(设查.码, 设查.文);
    const 模查 = 查(参.模块, '着色模块');
    if (模查.码) return 失(模查.码, 模查.文);
    if (!同设备(模查, 设查.项.设备号)) return 失(码.校验错误, '着色模块不属于此设备');
    const 布局 = 参.布局;
    if (!Array.isArray(布局) || 布局.some(项 => !非负整(项?.步长) || !Array.isArray(项.属性) ||
      项.属性.some(属 => !非负整(属?.位) || !顶点格式集.has(属.格式) || !非负整(属.偏))))
      return 失(码.校验错误, '顶点缓冲布局无效');
    if (!拓扑集.has(参.拓扑) || !Object.hasOwn(纹理每像素, 参.格式) || !Object.hasOwn(混合表, 参.混合)) return 失(码.校验错误, '图元拓扑、颜色目标格式或混合方式无效');
    const 描述 = {
      layout: 'auto',
      vertex: {module: 模查.项.物, entryPoint: String(参.顶点入口),
        buffers: 布局.map(项 => ({arrayStride: 项.步长, attributes: 项.属性.map(属 => ({shaderLocation: 属.位, format: 属.格式, offset: 属.偏}))}))},
      fragment: {module: 模查.项.物, entryPoint: String(参.片元入口), targets: [{format: 参.格式, blend: 混合表[参.混合]}]},
      primitive: 参.拓扑 === 'triangle-strip' ? {topology: 参.拓扑, stripIndexFormat: 'uint32'} : {topology: 参.拓扑}
    };
    const 设备 = 设查.项.物;
    const {值, 误} = await 包校验(设备, () => 设备.createRenderPipelineAsync(描述));
    if (误) return 失(误.码, 误.文);
    return 丢失后(设查.项, 果(码.成, 登记('渲染管线', 设查.项.设备号, 值, {格式: 参.格式})));
  };
  const 新建绑定组 = async 参 => {
    const 设查 = 查(参.设备, '设备');
    if (设查.码) return 失(设查.码, 设查.文);
    const 设号 = 设查.项.设备号;
    const 管查 = 查(参.管线, 参.管线种 === '渲染' ? '渲染管线' : '计算管线');
    if (管查.码) return 失(管查.码, 管查.文);
    if (!同设备(管查, 设号)) return 失(码.校验错误, '管线不属于此设备');
    if (!非负整(参.组) || !Array.isArray(参.资源)) return 失(码.校验错误, '绑定组号或资源列表无效');
    const 项们 = [];
    for (const 资 of 参.资源) {
      const 种类 = {缓冲: '缓冲', 纹理: '纹理', 采样器: '采样器'}[资?.种];
      if (!种类 || !非负整(资.号)) return 失(码.校验错误, '绑定资源无效');
      const 物查 = 查(资.物, 种类);
      if (物查.码) return 失(物查.码, 物查.文);
      if (!同设备(物查, 设号)) return 失(码.校验错误, '绑定资源不属于此设备');
      项们.push({binding: 资.号, resource: 种类 === '缓冲' ? {buffer: 物查.项.物} : 种类 === '纹理' ? 取视图(物查.项.物) : 物查.项.物});
    }
    const 设备 = 设查.项.物;
    const {值, 误} = await 包校验(设备, () => 设备.createBindGroup({layout: 管查.项.物.getBindGroupLayout(参.组), entries: 项们}));
    if (误) return 失(误.码, 误.文);
    return 丢失后(设查.项, 果(码.成, 登记('绑定组', 设号, 值)));
  };

  // 文言：命令先解其号：失效者全列不行而告已失效，异设备与乱形者告校验之误。汉语：先把命令里的句柄全部解析：失效对象使整列不执行并返回资源已失效；
  //       不属于同一设备、种类或字段不对的返回图形校验错误。解析全部通过后才开始录制。
  const 解命令 = (命令们, 设号) => {
    const 取 = (号, 种类) => {
      const 查果 = 查(号, 种类);
      if (查果.码 === 码.已失效) throw {码: 码.已失效, 文: 查果.文};
      if (查果.码) throw {码: 码.校验错误, 文: 查果.文};
      if (!同设备(查果, 设号)) throw {码: 码.校验错误, 文: '命令里的对象不属于此设备'};
      return 查果.项;
    };
    const 取列 = (号们, 种类) => {
      if (!Array.isArray(号们)) throw {码: 码.校验错误, 文: '命令字段须为列表'};
      return 号们.map(号 => 取(号, 种类).物);
    };
    if (!Array.isArray(命令们)) throw {码: 码.校验错误, 文: '命令列表无效'};
    return 命令们.map(令 => {
      if (令?.种 === '派发') return {种: '派发', 管线: 取(令.管线, '计算管线').物, 组: 取列(令.组, '绑定组'), 数: 令.数};
      if (令?.种 === '通道') {
        const 纹 = 取(令.纹理, '纹理');
        if (!Array.isArray(令.绘)) throw {码: 码.校验错误, 文: '绘制列表无效'};
        return {种: '通道', 纹理: 纹.物, 清: 令.清, 绘: 令.绘.map(绘 => ({管线: 取(绘.管线, '渲染管线').物, 组: 取列(绘.组, '绑定组'),
          顶点: 取列(绘.顶点, '缓冲'), 索引: 绘.索引 === null || 绘.索引 === undefined ? null : 取(绘.索引, '缓冲').物, 数: 绘.数, 实例: 绘.实例}))};
      }
      if (令?.种 === '复制') return {种: '复制', 源: 取(令.源, '缓冲').物, 源偏: 令.源偏, 的: 取(令.的, '缓冲').物, 的偏: 令.的偏, 长: 令.长};
      throw {码: 码.校验错误, 文: '未知图形命令'};
    });
  };
  const 录命令 = (编, 令) => {
    if (令.种 === '派发') {
      const 通 = 编.beginComputePass();
      通.setPipeline(令.管线);
      令.组.forEach((组, 序) => 通.setBindGroup(序, 组));
      通.dispatchWorkgroups(令.数?.[0], 令.数?.[1], 令.数?.[2]);
      通.end();
    } else if (令.种 === '通道') {
      const 清 = Array.isArray(令.清) ? 令.清 : null;
      const 附件 = {view: 取视图(令.纹理), loadOp: 清 ? 'clear' : 'load', storeOp: 'store'};
      if (清) 附件.clearValue = {r: 清[0], g: 清[1], b: 清[2], a: 清[3]};
      const 通 = 编.beginRenderPass({colorAttachments: [附件]});
      for (const 绘 of 令.绘) {
        通.setPipeline(绘.管线);
        绘.组.forEach((组, 序) => 通.setBindGroup(序, 组));
        绘.顶点.forEach((缓, 序) => 通.setVertexBuffer(序, 缓));
        if (绘.索引) {
          通.setIndexBuffer(绘.索引, 'uint32');
          通.drawIndexed(绘.数, 绘.实例);
        } else 通.draw(绘.数, 绘.实例);
      }
      通.end();
    } else 编.copyBufferToBuffer(令.源, 令.源偏, 令.的, 令.的偏, 令.长);
  };
  // 文言：一列之命，录于一编，一交而已；有一不过，交则全弃。汉语：整列命令录进同一个命令编码器、一次提交：任一条校验不通过，WebGPU 使整个命令缓冲无效，提交时整列都不执行。
  const 提交命令 = async 参 => {
    const 设查 = 查(参.设备, '设备');
    if (设查.码) return 失(设查.码, 设查.文);
    let 令们;
    try { 令们 = 解命令(参.命令, 设查.项.设备号); } catch (错误) { return 失(错误.码 ?? 码.宿主失败, 错误.文 ?? 消息(错误)); }
    const 设备 = 设查.项.物;
    const {误} = await 包校验(设备, () => {
      const 编 = 设备.createCommandEncoder();
      for (const 令 of 令们) 录命令(编, 令);
      设备.queue.submit([编.finish()]);
    });
    if (误) return 失(误.码, 误.文);
    return 丢失后(设查.项, 果(码.成));
  };

  // ---- 图形显示桥：显示面帧纹理与呈现 ----
  const 呈现格式 = 参 => {
    const 面 = 显示?.取面(参.显示面);
    return 果(码.成, 0, 面?.呈现格式 ?? 器.首选格式(面));
  };
  // 文言：帧纹理者，宿主之离屏纹理也；呈现时乃交于显示面。如此则 JSPI 挂起之间帧纹理不失效，且可读回。
  // 汉语：显示面帧纹理是宿主持有的离屏纹理（首选格式，渲染目标、采样、复制源与目标用途），呈现时才由呈现器交给显示面：
  //       浏览器复制到 canvas 当前纹理，Node 画到 SDL 窗口的当前纹理。canvas 的当前纹理在浏览器每次刷新画面时过期，而应用在两次呈现之间会因
  //       JSPI 挂起多次让出事件循环，所以不直接交出它；离屏纹理也使帧纹理能像普通纹理一样读回。
  const 取帧纹理 = async 参 => {
    const 面 = 显示?.取面(参.显示面);
    if (!面 || 面.已关闭) return 失(码.已失效, '显示面已失效');
    const 设查 = 查(参.设备, '设备');
    if (设查.码) return 失(码.已失效, 设查.文);
    const 设号 = 设查.项.设备号;
    if (面.模式 === '二维') return 失(码.暂不可用, '此显示面已用于提交像素画面');
    if (面.绑定设备号 && 面.绑定设备号 !== 设号) return 失(码.输入无效, '此显示面已与别的设备绑定');
    const 旧帧 = 面.帧号 ? 物表.get(面.帧号) : null;
    if (旧帧 && !旧帧.失效) return 果(码.成, 面.帧号);
    let [宽, 高] = 显示.尺寸值(面);
    if (!(宽 > 0 && 高 > 0)) return 失(码.暂不可用, '显示面暂时没有正尺寸');
    const 设备 = 设查.项.物;
    if (!面.绑定设备号) {
      const 败 = await 器.绑定(面, 设备);
      if (败) return 失(败.码, 败.文);
      面.绑定设备号 = 设号;
      面.关闭钩子.push(() => {
        const 帧 = 面.帧号 ? 物表.get(面.帧号) : null;
        if (帧) 帧.失效 = true;
        try { 面.离屏?.destroy(); } catch { /* 忽略 */ }
      });
      // 文言：绑定或易其面（节点易窗），尺寸再量之。汉语：绑定可能换了显示面的底层窗口（Node 换成 WebGPU 窗口），重新量一次尺寸。
      [宽, 高] = 显示.尺寸值(面);
      if (!(宽 > 0 && 高 > 0)) return 失(码.暂不可用, '显示面暂时没有正尺寸');
    }
    面.模式 = '图形';
    器.备帧?.(面, 宽, 高);
    const 格式 = 面.呈现格式;
    if (!面.离屏 || 面.离屏宽 !== 宽 || 面.离屏高 !== 高) {
      const {值, 误} = await 包校验(设备, () => 设备.createTexture({size: [宽, 高], format: 格式,
        usage: 纹理位.渲染 | 纹理位.采样 | 纹理位.复源 | 纹理位.复的}));
      if (误) return 失(误.码, 误.文);
      try { 面.离屏?.destroy(); } catch { /* 忽略 */ }
      面.离屏 = 值;
      面.离屏宽 = 宽;
      面.离屏高 = 高;
    }
    面.帧号 = 登记('纹理', 设号, 面.离屏, {宽, 高, 格式, 每像素: 4, 帧: true, 显示面: 面.号});
    return 丢失后(设查.项, 果(码.成, 面.帧号));
  };
  // 文言：呈现：废帧号，交帧于呈现之器，候其可受下一帧而返。汉语：呈现：作废本帧句柄，由呈现器把帧纹理交给显示面，等可以接受下一帧时返回。
  const 呈现 = async 参 => {
    const 面 = 显示?.取面(参.显示面);
    if (!面 || 面.已关闭) return 失(码.已失效, '显示面已失效');
    const 帧 = 面.帧号 ? 物表.get(面.帧号) : null;
    if (!帧 || 帧.失效) return 失(码.输入无效, '本帧还没有取得显示面纹理');
    帧.失效 = true;
    面.帧号 = 0;
    const 设 = 物表.get(面.绑定设备号);
    if (!设 || 设.失效 || 设.丢失 !== null) return 失(码.已失效, '显示面绑定的设备已失效');
    try { await 器.交帧(面, 设.物, 帧); } catch (错误) { return 失(码.宿主失败, 消息(错误)); }
    return 果(码.成);
  };

  const 操作表 = {
    '设备.取得': 取得设备, '设备.限额': 设备限额, '设备.状态': 设备状态, '设备.释放': 释放设备, '设备.等待': 等待完成,
    '缓冲.新建': 新建缓冲, '缓冲.写入': 写入缓冲, '缓冲.读回': 读回缓冲, '缓冲.销毁': 销毁缓冲,
    '纹理.新建': 新建纹理, '纹理.写入': 写入纹理, '纹理.读回': 读回纹理, '纹理.销毁': 销毁纹理,
    '采样器.新建': 新建采样器, '着色.编译': 编译着色, '计算管线.新建': 新建计算管线, '渲染管线.新建': 新建渲染管线,
    '绑定组.新建': 新建绑定组, '命令.提交': 提交命令,
    '呈现.格式': 呈现格式, '呈现.取纹理': 取帧纹理, '呈现.呈现': 呈现
  };
  // 文言：一口而分诸术；参以 JSON 文，字节另传；异常皆归码。汉语：原语分派：参数是 JSON 文字，写入数据与着色程序源文字走字节参数；同步与异步异常都转成状态码。
  const 调用 = (操作, 参数文, 字节 = 空字节) => {
    const 函 = Object.hasOwn(操作表, 操作) ? 操作表[操作] : null;
    if (!函) return 失(码.宿主失败, '图形操作不受支持：' + String(操作).slice(0, 64));
    let 参;
    try { 参 = 参数文 ? JSON.parse(参数文) : {}; } catch { return 失(码.非法使用, '图形操作参数不是 JSON'); }
    if (!参 || typeof 参 !== 'object') return 失(码.非法使用, '图形操作参数须为对象');
    try {
      const 值 = 函(参, 字节 instanceof Uint8Array ? 字节 : 空字节);
      return 值 && typeof 值.then === 'function' ? 值.catch(错误 => 失(码.宿主失败, 消息(错误))) : 值;
    } catch (错误) { return 失(码.宿主失败, 消息(错误)); }
  };
  const 清理 = () => {
    for (const 项 of 物表.values()) {
      if (项.种类 === '设备' && !项.失效) { 项.失效 = true; try { 项.物.destroy(); } catch { /* 忽略 */ } }
      项.失效 = true;
    }
  };
  const 状态 = () => ({图形对象数: [...物表.values()].filter(项 => !项.失效).length});
  return {调用, 清理, 状态};
}
