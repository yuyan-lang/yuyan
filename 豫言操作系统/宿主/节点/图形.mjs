// 文言：路一节点宿主之显示、图形处理器与图形显示原语。显示面者，宿主之选项所授之名，初取乃开 SDL 之窗；图形处理器以 Dawn 行 WebGPU。
//       显示面之表与图形之能共用浏览器宿主之 图形.mjs，此篇惟供窗之后端与呈现之器。此模无所引，发行时内联于启动文件，所需之能皆由 应用宿主.mjs 注入；
//       原生之依赖（@kmamal/sdl、@kmamal/gpu）至初用乃载，不得则返资源暂不可用。
// 汉语：路一 Node 宿主的显示、图形处理器与图形显示原语（豫言_节点_显示、豫言_节点_图形），以及读字体文件的字体原语（豫言_诺节_字体）。显示面由宿主选项 --授权显示面 名称=宽x高 授予，
//       第一次取得时开一个 SDL 窗口（@kmamal/sdl）；图形处理器用 Dawn 的 WebGPU（@kmamal/gpu）。显示面表与图形能力共用浏览器宿主的
//       图形.mjs（创建显示面表、创建图形能力），本模块只提供 SDL 窗口的显示面后端与呈现器。
//       本模块不写 import：发行启动文件把它内联进来；共用工厂、状态码、原生依赖载入与标准错误输出都由 应用宿主.mjs 注入。
//       原生依赖在第一次用到时才载入：找不到时 取得已授显示面、取得图形设备 返回资源暂不可用，并在标准错误提示一次。
// 文言：后台之式（--显示面后台）：SDL 初始化前令为后台应用，窗皆隐而不激，自动之验用之，不夺前台。
// 汉语：后台模式（宿主选项 --显示面后台 或环境变量 YY_NODE_DISPLAY_BACKGROUND=1）：载入 SDL 前设 SDL_MAC_BACKGROUND_APP=1（macOS 上成为后台应用，
//       无 Dock 图标、不激活），窗口以 visible: false 创建（隐藏、不激活）。像素提交与 WebGPU 呈现在隐藏窗口上照常返回，自动验收用它，不抢前台焦点。

const 解码器 = new TextDecoder('utf-8', {ignoreBOM: true});
const 文字化 = 值 => (值 instanceof Uint8Array ? 解码器.decode(值) : String(值 ?? ''));
const 消息 = 错 => String(错?.message ?? 错);

// 文言：SDL 之键名译为语义键名：首版七者，零点二版增编辑与修饰之键；单一码点者原样，余皆不报。
// 汉语：SDL 虚拟键名译成规范的语义键名：0.1.0 的七个，0.2.0 增加 删除、起首、末尾、上翻页、下翻页、制表、退出 与修饰键 上档、控制、交替、命令；
//       单个 Unicode 标量值原样交付；其余键（f1、capsLock……）不交付。SDL 的回车键名为 return，小键盘回车为 enter，两者都译为回车；
//       左右两个修饰键同名（@kmamal/sdl 0.11 把右 GUI 键拼成 gUI，一并收下）。
const SDL键名表 = Object.freeze({left: '左', right: '右', up: '上', down: '下', return: '回车', enter: '回车', backspace: '退格', space: '空格',
  delete: '删除', home: '起首', end: '末尾', pageUp: '上翻页', pageDown: '下翻页', tab: '制表', escape: '退出', f2: '功能二',
  shift: '上档', ctrl: '控制', alt: '交替', gui: '命令', gUI: '命令'});
const 修饰键名 = new Set(['上档', '控制', '交替', '命令']);
// 文言：滚一格当四十八逻辑像素。汉语：SDL 的滚轮量以格计，一格折成 48 个逻辑像素再乘像素比。
const 滚动每格逻辑像素 = 48;
export const 译SDL键 = 事 => {
  const 键 = 事?.key;
  if (typeof 键 !== 'string') return null;
  if (Object.hasOwn(SDL键名表, 键)) return SDL键名表[键];
  if ([...键].length !== 1) return null;
  // 文言：SDL 字母之键名恒小写，依 Shift 与大写锁定定其大小。待办事项：他键随 Shift 而变者（如 Shift+1 为 !）当依键盘之布局求之。
  // 汉语：SDL 的字母键名总是小写，按 Shift 与大写锁定决定大小写，与浏览器的 event.key 一致。待办事项：其他随 Shift 变化的键（如 Shift+1 为 !）按键盘布局求值。
  const 大 = 键.toUpperCase();
  return Boolean(事.shift) !== Boolean(事.capslock) && [...大].length === 1 ? 大 : 键;
};

// 文言：搬运之着色：全屏一三角，以帧纹理之素覆写窗之当前纹理。汉语：把离屏帧纹理逐像素画到窗口当前纹理的着色程序：一个盖满画面的三角形，片元按像素坐标 textureLoad。
const 搬运着色 = `@group(0) @binding(0) var 帧: texture_2d<f32>;
@vertex fn 顶点(@builtin(vertex_index) 序: u32) -> @builtin(position) vec4f {
  var 点 = array<vec2f, 3>(vec2f(-1.0, -1.0), vec2f(3.0, -1.0), vec2f(-1.0, 3.0));
  return vec4f(点[序], 0.0, 1.0);
}
@fragment fn 片元(@builtin(position) 位: vec4f) -> @location(0) vec4f {
  return textureLoad(帧, vec2i(位.xy), 0);
}
`;

// 文言：造节点之显示与图形之能。汉语：创建 Node 的显示与图形能力，返回 {原语, 清理}。参数：共用工厂（创建显示面表、创建图形能力）、状态码、事件种类，
//       载入原生(名) → 模块（找不到时抛出），授权显示面（名称 → {宽, 高}，单位为 SDL 窗口的逻辑尺寸），后台，写错误(文)；
//       字体原语另需 文件系统（node:fs）、路径分隔、平台（process.platform）、家目录与环境变量，缺文件系统时字体原语只返回资源暂不可用。
export function 创建节点图形能力({创建显示面表, 创建图形能力, 状态码, 事件种类, 载入原生, 授权显示面 = new Map(), 后台 = false, 写错误 = () => {},
  文件系统 = null, 路径分隔 = '/', 平台 = 'linux', 家目录 = '', 环境 = {}}) {
  const 字体单次上限 = 16 * 1024 * 1024;
  const 码 = 状态码;
  let 视频 = null;
  let 图形模 = null;
  let 实例 = null;
  const 已报 = new Set();
  // 文言：载原生之模；不得则于标准误一告而抛。汉语：载入原生依赖；找不到时在标准错误提示一次，再抛出给调用者转成资源暂不可用。
  const 载 = 名 => {
    try { return 载入原生(名); } catch (错误) {
      if (!已报.has(名)) {
        已报.add(名);
        写错误('豫言节点宿主：' + 消息(错误));
      }
      throw 错误;
    }
  };
  const 载SDL = () => {
    if (!视频) {
      // 文言：后台之式须于 SDL 初始化前设之。汉语：后台模式的提示须在 SDL 初始化（载入模块）之前设好。
      if (后台) process.env.SDL_MAC_BACKGROUND_APP = '1';
      视频 = 载('@kmamal/sdl');
    }
    return 视频;
  };
  const 载GPU = () => {
    if (!实例) {
      图形模 = 载('@kmamal/gpu');
      实例 = 图形模.create([]);
    }
    return 实例;
  };

  // ---- 显示面后端：SDL 窗口 ----
  // 文言：SDL 之坐标以点计，乘其素比为物理之素。汉语：SDL 事件坐标以窗口的点为单位，乘以像素宽与点宽之比，得到物理像素坐标。
  const 坐标 = (窗, 事) => {
    const 横比 = 窗.width > 0 ? 窗.pixelWidth / 窗.width : 1;
    const 纵比 = 窗.height > 0 ? 窗.pixelHeight / 窗.height : 1;
    return [Math.floor(Number(事.x) * 横比), Math.floor(Number(事.y) * 纵比)];
  };
  // 文言：SDL 之钮左一中二右三，规范为零一二。汉语：SDL 按钮左 1、中 2、右 3，规范为主 0、辅助 1、次要 2，所以减一。
  const 钮 = 事 => Math.max(0, Number(事.button) - 1);
  // 文言：开窗：图形之式须以 webgpu 建窗；易窗时新窗居旧窗之位，旧窗之闭不作面闭。
  // 汉语：开窗：像素提交用普通 SDL 窗口，WebGPU 呈现须用 webgpu: true 建的窗口（两者不能互换）；显示面第一次用于 GPU 呈现时换成 WebGPU 窗口，
  //       新窗口放在旧窗口的位置。先把 面.窗口 指向新窗口再销毁旧窗口，旧窗口的关闭事件因此不当作显示面关闭。
  const 开窗 = (面, 图形乎) => {
    const 旧 = 面.窗口 && !面.窗口.destroyed ? 面.窗口 : null;
    const 选项 = {title: 面.名, width: 面.授.宽, height: 面.授.高, visible: !后台, webgpu: 图形乎};
    if (旧 && Number.isInteger(旧.x) && Number.isInteger(旧.y)) Object.assign(选项, {x: 旧.x, y: 旧.y});
    const 窗 = 载SDL().video.createWindow(选项);
    面.窗口 = 窗;
    const {推, 关闭} = 面.回调;
    窗.on('close', () => { if (面.窗口 === 窗) 关闭(); });
    // 汉语：窗口尺寸或所在显示器变化，唤醒应用重读尺寸及像素比。文言：窗尺或所处显示器易，唤客复读尺寸与像素比。
    const 告尺变 = () => { if (面.窗口 === 窗) 推({种:事件种类.显示尺寸变化}); };
    窗.on('resize', 告尺变);
    窗.on('displayChange', 告尺变);
    // 文言：修饰之键记其按住者，窗失焦则补其抬起。汉语：记下按着的修饰键，窗口失去焦点时为它们补发按键抬起。
    const 按住修饰 = new Set();
    const 按键 = 种 => 事 => {
      const 键 = 译SDL键(事);
      if (键 === null) return;
      if (修饰键名.has(键)) {
        if (种 === 事件种类.按键按下) 按住修饰.add(键);
        else 按住修饰.delete(键);
      }
      推({种, 文: 键});
    };
    窗.on('keyDown', 按键(事件种类.按键按下));
    窗.on('keyUp', 按键(事件种类.按键抬起));
    窗.on('blur', () => {
      for (const 键 of 按住修饰) 推({种: 事件种类.按键抬起, 文: 键});
      按住修饰.clear();
    });
    // 文言：滚轮之格折为物理之素；SDL 纵量向上为正，规范向下为正，故反之；flipped 则再反。
    // 汉语：滚轮：格数乘 48 个逻辑像素与像素比得物理像素；SDL 的纵向量向上为正，规范向下为正，所以取反；flipped（自然滚动）时方向再反。
    窗.on('mouseWheel', 事 => {
      const [甲, 乙] = 坐标(窗, 事);
      const 比 = 窗.width > 0 ? 窗.pixelWidth / 窗.width : 1;
      const 向 = 事.flipped ? -1 : 1;
      推({种: 事件种类.滚轮, 甲, 乙, 丙: Math.round(Number(事.dx || 0) * 向 * 滚动每格逻辑像素 * 比) || 0,
        丁: Math.round(-Number(事.dy || 0) * 向 * 滚动每格逻辑像素 * 比) || 0});
    });
    // 文言：SDL 之文字输入即输入法所成之文。汉语：SDL 的 textInput 是已提交的文字（含输入法提交的文字）。待办事项：组字期间的按键仍会交付（@kmamal/sdl 不给组字事件）。
    窗.on('textInput', 事 => { if (事.text) 推({种: 事件种类.文字输入, 文: String(事.text)}); });
    窗.on('mouseMove', 事 => {
      const [甲, 乙] = 坐标(窗, 事);
      推({种: 事件种类.指针移动, 甲, 乙});
    });
    窗.on('mouseButtonDown', 事 => {
      const [甲, 乙] = 坐标(窗, 事);
      推({种: 事件种类.指针按下, 甲, 乙, 丙: 钮(事)});
    });
    窗.on('mouseButtonUp', 事 => {
      const [甲, 乙] = 坐标(窗, 事);
      推({种: 事件种类.指针抬起, 甲, 乙, 丙: 钮(事)});
    });
    if (旧) 旧.destroy();
    return 窗;
  };
  const 窗口后端 = {
    找: 名 => {
      const 授 = 授权显示面.get(名);
      if (!授) return {码: 码.未获授权, 文: '显示面「' + 名 + '」未获授权（宿主选项 --授权显示面 名称=宽x高）'};
      try { 载SDL(); } catch (错误) { return {码: 码.暂不可用, 文: 消息(错误)}; }
      return {值: {名, 宽: 授.宽, 高: 授.高}};
    },
    同源: (面, 值) => 面.名 === 值.名,
    建: (面, 值, 回调) => {
      面.授 = 值;
      面.回调 = 回调;
      面.窗口 = null;
      try { 开窗(面, false); } catch (错误) { return {码: 码.暂不可用, 文: 'SDL 窗口创建失败：' + 消息(错误)}; }
      面.清理.push(() => {
        const 窗 = 面.窗口;
        if (窗 && !窗.destroyed) 窗.destroy();
      });
      return null;
    },
    已断开: 面 => Boolean(面.窗口?.destroyed),
    // 文言：尺以物理之素：取窗之 pixelWidth、pixelHeight，高分之屏为点之倍。汉语：尺寸按物理像素，取 SDL 窗口的 pixelWidth、pixelHeight（高分屏上是点数的倍数）。
    尺寸值: 面 => (面.窗口 && !面.窗口.destroyed ? [面.窗口.pixelWidth, 面.窗口.pixelHeight] : [0, 0]),
    // 文言：像素比为窗之物理宽与点宽之比。汉语：像素比 = 窗口像素宽 ÷ 点宽。待办事项：输入区域（设输入区域）无从设置——@kmamal/sdl 没有 SDL_SetTextInputRect，
    //       候选框位置由系统定；以后可考虑经 node:ffi 调同一个 libSDL2。
    像素比值: 面 => (面.窗口 && !面.窗口.destroyed ? [面.窗口.pixelWidth, 面.窗口.width] : [0, 0]),
    // 文言：素以 SDL 之 rgba32 提交，于小端之机即红绿蓝透之序。汉语：像素用 SDL 的 rgba32 格式提交（小端机器上即内存中 R、G、B、A 的次序），不混合，原样覆盖。
    画: (面, 宽, 高, 字节) => {
      面.窗口.render(宽, 高, 宽 * 4, 'rgba32', Buffer.from(字节.buffer, 字节.byteOffset, 字节.length));
      return null;
    },
    清理: () => {}
  };

  // ---- 呈现器：离屏帧纹理画到 SDL 窗口 ----
  // 文言：未候之误域，清理之前必候其毕；设备已毁而其回调犹悬，则进程退时段错。汉语：呈现时没有等待的错误域弹出，清理前要等它们完成；
  //       设备销毁时回调还挂着，进程退出时 Dawn 会段错误（2026-09-30 本机实测退出码 139）。
  const 待弹 = new Set();
  // 文言：呈现之器：初取帧纹理时易为 WebGPU 之窗，以 renderGPUDeviceToWindow 系之于设备；呈现则以搬运之管线画帧纹理于窗之当前纹理，swap 之。
  // 汉语：Node 呈现器：第一次取帧纹理时把显示面换成 WebGPU 窗口，用 renderGPUDeviceToWindow（presentMode fifo）把设备接到窗口；
  //       呈现时用一个搬运管线把离屏帧纹理画到窗口当前纹理（getCurrentTextureView），再 swap()。窗口纹理只有渲染目标用途，不能作复制目标，所以用画的；
  //       也不用渲染器的 getCurrentTexture()：它每调用一次少计一次设备的引用，退出时 Dawn 断言失败（SIGTRAP）。
  //       节奏：fifo 下窗口的交换链满了时 getCurrentTextureView 会等到下一次刷新，所以连续呈现按刷新节奏进行；swap 后再让出一次事件循环，好让 SDL 处理事件。
  const 窗口呈现器 = {
    首选格式: 面 => {
      try { return 面?.渲染器?.getPreferredFormat?.() ?? 实例?.getPreferredCanvasFormat?.() ?? 'bgra8unorm'; } catch { return 'bgra8unorm'; }
    },
    绑定: async (面, 设备) => {
      try {
        if (!面.窗口?.webgpu) 开窗(面, true);
        面.渲染器 = 图形模.renderGPUDeviceToWindow({device: 设备, window: 面.窗口, presentMode: 'fifo'});
        面.呈现宽 = 面.窗口.pixelWidth;
        面.呈现高 = 面.窗口.pixelHeight;
        面.呈现格式 = 面.渲染器.getPreferredFormat() ?? 'bgra8unorm';
      } catch (错误) { return {码: 码.暂不可用, 文: '显示面不能用于 WebGPU 呈现：' + 消息(错误)}; }
      设备.pushErrorScope('validation');
      let 异常 = null;
      try {
        const 模块 = 设备.createShaderModule({code: 搬运着色});
        面.搬运管线 = 设备.createRenderPipeline({layout: 'auto', vertex: {module: 模块, entryPoint: '顶点'},
          fragment: {module: 模块, entryPoint: '片元', targets: [{format: 面.呈现格式}]}, primitive: {topology: 'triangle-list'}});
      } catch (错误) { 异常 = 错误; }
      const 误 = await 设备.popErrorScope().catch(错误 => 错误);
      if (异常 || 误) return {码: 码.宿主失败, 文: '显示面搬运管线建立失败：' + 消息(异常 ?? 误)};
      return null;
    },
    交帧: async (面, 设备, 帧) => {
      const 窗 = 面.窗口;
      if (!窗 || 窗.destroyed) throw new Error('显示面窗口已关闭');
      // 文言：窗之素数易（移至他屏）则重配其面。汉语：窗口像素尺寸变了（如移到像素比不同的屏幕）就重新配置窗口表面。
      if (窗.pixelWidth !== 面.呈现宽 || 窗.pixelHeight !== 面.呈现高) {
        面.渲染器.resize();
        面.呈现宽 = 窗.pixelWidth;
        面.呈现高 = 窗.pixelHeight;
      }
      // 文言：搬运之误乃宿主之咎，不候其误域，异步书于标准误；呈现之节由交换之链定之。
      // 汉语：搬运出错只可能是宿主的缺陷，所以不等错误域（@kmamal/gpu 每约 100 毫秒才处理一次异步回调，等它会把呈现拖到每秒十帧以下），
      //       错误异步写到标准错误；呈现节奏由交换链决定（fifo 下取窗口当前纹理时等下一次刷新）。
      设备.pushErrorScope('validation');
      let 异常 = null;
      try {
        if (面.搬运源 !== 帧.物) {
          面.搬运组 = 设备.createBindGroup({layout: 面.搬运管线.getBindGroupLayout(0), entries: [{binding: 0, resource: 帧.物}]});
          面.搬运源 = 帧.物;
        }
        const 编 = 设备.createCommandEncoder();
        const 通 = 编.beginRenderPass({colorAttachments: [{view: 面.渲染器.getCurrentTextureView(), loadOp: 'clear',
          clearValue: {r: 0, g: 0, b: 0, a: 1}, storeOp: 'store'}]});
        通.setPipeline(面.搬运管线);
        通.setBindGroup(0, 面.搬运组);
        通.draw(3);
        通.end();
        设备.queue.submit([编.finish()]);
        面.渲染器.swap();
      } catch (错误) { 异常 = 错误; }
      const 弹 = 设备.popErrorScope().then(误 => { if (误) 写错误('豫言节点宿主：显示面搬运失败：' + 消息(误)); }, () => {})
        .finally(() => 待弹.delete(弹));
      待弹.add(弹);
      if (异常) throw 异常;
      await new Promise(完成 => setImmediate(完成));
    }
  };

  // ---- 字体：只读字体目录下的文件 ----
  // 文言：字体之原语：惟开字体之目下之文件，依偏移读之；族名至路径之表在豫言之适配。
  // 汉语：字体原语 豫言_诺节_字体(操作, 路径, 号, 偏移, 长度)，返回 [码, 号, 长度, 字节, 文字]。族名到路径的对照表写在豫言适配（字体诺节）里，
  //       宿主只做两件事：打开 只接受字体目录白名单下的文件（按真实路径判断，防符号链接越出目录），同一文件重复打开得同一个号；
  //       读取 按号与偏移读出一段（pread，不动文件位置）。单次读取上限同值桥的 16 MiB。待办事项：视窗的字体目录只看 WINDIR 与 LOCALAPPDATA。
  const 空字节 = new Uint8Array(0);
  const 字体目录们 = (() => {
    if (!文件系统) return [];
    const 家 = 家目录 || '';
    const 候 = 平台 === 'darwin' ? ['/System/Library/Fonts', '/Library/Fonts', 家 && 家 + '/Library/Fonts']
      : 平台 === 'win32' ? [(环境.WINDIR || 'C:\\Windows') + '\\Fonts', 环境.LOCALAPPDATA && 环境.LOCALAPPDATA + '\\Microsoft\\Windows\\Fonts']
      : ['/usr/share/fonts', '/usr/local/share/fonts', 家 && 家 + '/.local/share/fonts', 家 && 家 + '/.fonts'];
    return 候.filter(Boolean).map(目 => { try { return 文件系统.realpathSync(目); } catch { return 目; } });
  })();
  const 字体已开 = new Map();
  const 字体文件们 = new Map();
  let 字体下号 = 1;
  const 在字体目录内 = 真 => 字体目录们.some(目 => 真 === 目 || 真.startsWith(目 + 路径分隔));
  const 打开字体 = 径 => {
    if (!文件系统) return [码.暂不可用, 0, 0, 空字节, '宿主没有文件系统'];
    let 真;
    try { 真 = 文件系统.realpathSync(径); } catch (错误) {
      return [错误?.code === 'ENOENT' ? 码.不存在 : 码.宿主失败, 0, 0, 空字节, 消息(错误)];
    }
    if (!在字体目录内(真)) return [码.未获授权, 0, 0, 空字节, '只能读取字体目录下的文件：' + 径];
    const 旧号 = 字体已开.get(真);
    if (旧号) return [码.成, 旧号, 字体文件们.get(旧号).长度, 空字节, ''];
    try {
      const 描述符 = 文件系统.openSync(真, 'r');
      const 长度 = 文件系统.fstatSync(描述符).size;
      const 号 = 字体下号++;
      字体文件们.set(号, {描述符, 长度});
      字体已开.set(真, 号);
      return [码.成, 号, 长度, 空字节, ''];
    } catch (错误) { return [码.宿主失败, 0, 0, 空字节, 消息(错误)]; }
  };
  const 读取字体 = (号, 偏移, 长度) => {
    const 件 = 字体文件们.get(号);
    if (!件) return [码.已失效, 0, 0, 空字节, '字体文件号无效'];
    if (!(Number.isSafeInteger(偏移) && Number.isSafeInteger(长度) && 偏移 >= 0 && 长度 >= 0 && 偏移 + 长度 <= 件.长度)) {
      return [码.输入无效, 0, 0, 空字节, '读取范围越出字体文件'];
    }
    if (长度 > 字体单次上限) return [码.配额已尽, 0, 0, 空字节, '单次读取超过 16 MiB'];
    const 缓 = new Uint8Array(长度);
    let 已读 = 0;
    try {
      while (已读 < 长度) {
        const 本次 = 文件系统.readSync(件.描述符, 缓, 已读, 长度 - 已读, 偏移 + 已读);
        if (本次 <= 0) break;
        已读 += 本次;
      }
    } catch (错误) { return [码.宿主失败, 0, 0, 空字节, 消息(错误)]; }
    return [码.成, 号, 已读, 已读 === 长度 ? 缓 : 缓.subarray(0, 已读), ''];
  };
  const 字体原语 = (操作, 径, 号, 偏移, 长度) => {
    const 作 = 文字化(操作);
    if (作 === '打开') return 打开字体(文字化(径));
    if (作 === '读取') return 读取字体(Number(号), Number(偏移), Number(长度));
    return [码.输入无效, 0, 0, 空字节, '未知的字体操作：' + 作];
  };
  const 关闭字体们 = () => {
    for (const {描述符} of 字体文件们.values()) { try { 文件系统.closeSync(描述符); } catch { /* 忽略 */ } }
    字体文件们.clear();
    字体已开.clear();
  };

  const 显示 = 创建显示面表({后端: 窗口后端});
  // 文言：Dawn 之 createView 恒带 swizzle 而不过，故以纹理径代其视图。汉语：Dawn 的 JS createView() 总带 swizzle 而校验失败，渲染附件与纹理绑定直接传纹理。
  const 图形 = 创建图形能力({全局: {}, 显示, 取图形: 载GPU, 呈现器: 窗口呈现器, 取视图: 纹 => 纹});
  // 文言：二口：参皆经值桥，整数为 BigInt，文为字节。汉语：两个原语；值桥传来的整数是 BigInt、文字是字节，这里转成数与字符串。
  const 原语 = Object.freeze({
    豫言_节点_显示: (操作, 号, 甲, 乙, 字节, 丙 = 0, 丁 = 0) => 显示.调用(文字化(操作), Number(号), Number(甲), Number(乙), 字节, Number(丙), Number(丁)),
    豫言_节点_图形: (操作, 参数, 字节) => 图形.调用(文字化(操作), 文字化(参数), 字节),
    豫言_诺节_字体: 字体原语
  });
  // 文言：客毕则候未决之误域，乃闭诸窗、释诸设备、销 Dawn 之例，令进程得退。汉语：应用结束时先等未决的错误域弹出，再关闭全部窗口、释放全部设备并销毁 Dawn 实例；
  //       否则 SDL 的事件轮询与 Dawn 的计时器会拖住进程。返回 Promise。
  const 清理 = async () => {
    await Promise.allSettled([...待弹]);
    关闭字体们();
    try { 显示.清理(); } catch { /* 忽略 */ }
    try { 图形.清理(); } catch { /* 忽略 */ }
    if (实例) {
      try { 图形模.destroy(实例); } catch { /* 忽略 */ }
      实例 = null;
    }
  };
  return {原语, 清理, 显示, 图形};
}
