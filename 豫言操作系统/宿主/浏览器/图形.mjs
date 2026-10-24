// 文言：图形处理器（WebGPU 核心子集）、图形显示桥与字体之宿主术。显示面那一部分已拆到 显示.mjs（单应用开机只带它），此处转发其导出，故原先从本文件取显示面诸术者照旧。
// 汉语：图形处理器（WebGPU 核心子集）、图形显示桥与字体的宿主实现。显示面那一部分已拆到 显示.mjs（单应用开机只带它），这里转发它的导出，
//   原先从本文件取显示面诸术的调用方照旧可用。本文件其余不变：WebGPU 核心子集、图形显示桥（把图形处理器呈现到显示面）与页面声明的字体。
import {状态码, 码, 单次交换上限, 空字节, 消息, 文字化, 创建显示面表, 创建画布后端} from './显示.mjs';
export * from './显示.mjs';

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
const 纹理用途表 = Object.freeze({采样: 纹理位.采样, 存储: 纹理位.存储, 渲染: 纹理位.渲染, 深度: 纹理位.渲染});
export const 纹理每像素 = Object.freeze({'rgba8unorm': 4, 'rgba8unorm-srgb': 4, 'bgra8unorm': 4, 'bgra8unorm-srgb': 4, 'rgba16float': 8, 'r32float': 4,
  'r8unorm': 1, 'depth32float': 4});
// 文言：深度之格惟一，惟作深度之鹄。汉语：深度纹理格式（0.2.0）只有 depth32float，用途只能是“深度”（渲染附件），不能写入、读回或复制。
const 深度格式 = 'depth32float';
const 深度比较集 = new Set(['never', 'less', 'equal', 'less-equal', 'greater', 'not-equal', 'greater-equal', 'always']);
const 存储纹理格式 = new Set(['rgba8unorm', 'rgba16float', 'r32float']);
// 文言：多采与衍层之格（零点四版）：红单精度不可滤，不可多采、不可衍层。汉语：0.4.0：r32float 不能过滤，不能作多重采样纹理、不能生成渐远层，也不能混合。
const 不可滤格式 = new Set(['r32float']);
// 文言：步进之名：逐点与逐例。汉语：顶点布局的步进（0.4.0）：缺省为逐顶点，“实例”为逐实例（stepMode instance）。
const 步进表 = Object.freeze({顶点: 'vertex', 实例: 'instance'});
const 顶点格式集 = new Set(['float32', 'float32x2', 'float32x3', 'float32x4', 'unorm8x4']);
const 拓扑集 = new Set(['triangle-list', 'triangle-strip', 'line-list', 'point-list']);
// 文言：剔法之名（0.3.0）：none、back、front，正面恒逆时。汉语：剔除方式的名字（0.3.0）：none、back、front，正面恒为逆时针（WebGPU 缺省）。
const 剔除集 = new Set(['none', 'back', 'front']);
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
    // 文言：层数与采数（零点四版），缺则一。汉语：多级渐远层数与采样数（0.4.0），缺省都是 1。
    const 层数 = 参.层数 ?? 1;
    const 采样数 = 参.采样数 ?? 1;
    const 边 = 设.物.limits.maxTextureDimension2D;
    if (!正整(宽) || !正整(高) || 宽 > 边 || 高 > 边) return 失(码.校验错误, '纹理宽高须在 1 至 ' + 边 + ' 之间');
    if (!Object.hasOwn(纹理每像素, 格式)) return 失(码.校验错误, '纹理格式无效');
    if (!Array.isArray(用途) || 用途.length === 0 || 用途.some(名 => !Object.hasOwn(纹理用途表, 名))) return 失(码.校验错误, '纹理用途须为非空列表');
    if (用途.includes('存储') && !存储纹理格式.has(格式)) return 失(码.校验错误, '存储纹理用途只允许 rgba8unorm、rgba16float、r32float');
    const 是深度 = 格式 === 深度格式;
    // 文言：深度之纹须用深鹄，又惟许兼采样（零点四版）。汉语：深度纹理的用途须含“深度”，另外只能再含“采样”（0.4.0 起作阴影图）；“深度”也只配深度格式。
    if (是深度 !== 用途.includes('深度') || (是深度 && 用途.some(名 => 名 !== '深度' && 名 !== '采样')))
      return 失(码.校验错误, '深度单精度的用途须含深度目标用途、另外只能再含采样用途，深度目标用途也只配深度单精度');
    if (!正整(层数) || 层数 > Math.floor(Math.log2(Math.max(宽, 高))) + 1) return 失(码.校验错误, '多级渐远纹理的层数须在 1 至 ⌊log₂ max(宽, 高)⌋ + 1 之间');
    if (层数 > 1 && 是深度) return 失(码.校验错误, '多级渐远纹理不能是深度单精度');
    if (采样数 !== 1 && 采样数 !== 4) return 失(码.校验错误, '采样数只能是 1 或 4');
    if (采样数 > 1 && !((是深度 && 用途.length === 1) || (!是深度 && !不可滤格式.has(格式) && 用途.length === 1 && 用途[0] === '渲染')))
      return 失(码.校验错误, '多重采样纹理的用途只能是渲染目标用途（颜色格式，红单精度除外）或深度目标用途（深度单精度）');
    // 文言：深纹与多采之纹无复制之用；多级者另加采样与绘鹄之用以供衍层。汉语：深度纹理与多重采样纹理不带复制用途；多级纹理另加采样与渲染附件用途，供生成渐远层。
    let 位 = 是深度 || 采样数 > 1 ? 0 : (纹理位.复源 | 纹理位.复的);
    for (const 名 of 用途) 位 |= 纹理用途表[名];
    if (层数 > 1) 位 |= 纹理位.采样 | 纹理位.渲染;
    const {值, 误} = await 包校验(设.物, () => 设.物.createTexture({size: [宽, 高], format: 格式, usage: 位, mipLevelCount: 层数, sampleCount: 采样数}));
    if (误) return 失(误.码, 误.文);
    return 丢失后(设, 果(码.成, 登记('纹理', 设.设备号, 值, {宽, 高, 格式, 每像素: 纹理每像素[格式], 层数, 采样数})));
  };
  const 写入纹理 = (参, 字节) => {
    const 查果 = 查写(参.纹理, '纹理');
    if (查果.忽略) return 果(码.成);
    if (查果.码) return 失(查果.码, 查果.文);
    const 纹 = 查果.项;
    if (纹.格式 === 深度格式) return 失(码.非法使用, '深度纹理不能写入');
    if (纹.采样数 > 1) return 失(码.非法使用, '多重采样纹理不能写入');
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
    if (纹.格式 === 深度格式) return 失(码.校验错误, '深度纹理不能读回');
    if (纹.采样数 > 1) return 失(码.校验错误, '多重采样纹理不能读回');
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
    // 文言：比法可有（零点四版之比较之采器）。汉语：可带比较函数（0.4.0 的比较采样器，WebGPU 的 compare）。
    const 比较 = 参.比较 ?? null;
    if (!过滤集.has(参.过滤) || !寻址集.has(参.寻址) || (比较 !== null && !深度比较集.has(比较))) return 失(码.非法使用, '采样器参数无效');
    const 描述 = {magFilter: 参.过滤, minFilter: 参.过滤, mipmapFilter: 参.过滤, addressModeU: 参.寻址, addressModeV: 参.寻址, addressModeW: 参.寻址};
    if (比较 !== null) 描述.compare = 比较;
    const 物 = 项.物.createSampler(描述);
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
    if (!Array.isArray(布局) || 布局.some(项 => !非负整(项?.步长) || !Array.isArray(项.属性) || (项.步进 !== undefined && !Object.hasOwn(步进表, 项.步进)) ||
      项.属性.some(属 => !非负整(属?.位) || !顶点格式集.has(属.格式) || !非负整(属.偏))))
      return 失(码.校验错误, '顶点缓冲布局无效');
    // 文言：鹄之列（零点四版），缺则以格与混为一鹄。汉语：颜色目标列（0.4.0 的全述管线），没有时以旧的格式与混合为唯一的颜色目标。
    const 鹄们 = Array.isArray(参.鹄们) ? 参.鹄们 : [{格式: 参.格式, 混合: 参.混合}];
    if (!拓扑集.has(参.拓扑) || 鹄们.length > 8 || 鹄们.some(鹄 => !Object.hasOwn(纹理每像素, 鹄?.格式) || 鹄.格式 === 深度格式 || !Object.hasOwn(混合表, 鹄.混合)))
      return 失(码.校验错误, '图元拓扑、颜色目标格式或混合方式无效');
    if (鹄们.some(鹄 => 不可滤格式.has(鹄.格式) && 鹄.混合 !== '不混合')) return 失(码.校验错误, '红单精度的颜色目标不能混合');
    const 采样数 = 参.采样数 ?? 1;
    if (采样数 !== 1 && 采样数 !== 4) return 失(码.校验错误, '渲染管线的采样数只能是 1 或 4');
    const 片元入口 = String(参.片元入口 ?? '');
    if (片元入口 === '' && 鹄们.length) return 失(码.校验错误, '片元入口为空时不能有颜色目标');
    // 文言：有剔法者校其名。汉语：带剔除方式（0.3.0）时核对名字，照原样交给 WebGPU 的 primitive.cullMode。
    const 剔除 = 参.剔除 ?? null;
    if (剔除 !== null && !剔除集.has(剔除)) return 失(码.校验错误, '剔除方式无效');
    // 文言：有深测者附深度之状。汉语：带深度测试（0.2.0）时加深度模板状态，格式恒为 depth32float。
    const 深度 = 参.深度 ?? null;
    if (深度 !== null && (!深度比较集.has(深度?.比较) || typeof 深度.写 !== 'boolean')) return 失(码.校验错误, '深度测试无效');
    if (!鹄们.length && 深度 === null) return 失(码.校验错误, '没有颜色目标的渲染管线须带深度测试');
    const 描述 = {
      layout: 'auto',
      vertex: {module: 模查.项.物, entryPoint: String(参.顶点入口),
        buffers: 布局.map(项 => ({arrayStride: 项.步长, stepMode: 步进表[项.步进 ?? '顶点'],
          attributes: 项.属性.map(属 => ({shaderLocation: 属.位, format: 属.格式, offset: 属.偏}))}))},
      primitive: 参.拓扑 === 'triangle-strip' ? {topology: 参.拓扑, stripIndexFormat: 'uint32'} : {topology: 参.拓扑}
    };
    // 文言：片元之口空则不设片元，惟书深。汉语：片元入口为空文时不设片元阶段，管线只写深度（0.4.0）。
    if (片元入口 !== '') 描述.fragment = {module: 模查.项.物, entryPoint: 片元入口, targets: 鹄们.map(鹄 => ({format: 鹄.格式, blend: 混合表[鹄.混合]}))};
    if (剔除 !== null) 描述.primitive.cullMode = 剔除;
    if (深度 !== null) 描述.depthStencil = {format: 深度格式, depthWriteEnabled: 深度.写, depthCompare: 深度.比较};
    if (采样数 > 1) 描述.multisample = {count: 采样数};
    const 设备 = 设查.项.物;
    const {值, 误} = await 包校验(设备, () => 设备.createRenderPipelineAsync(描述));
    if (误) return 失(误.码, 误.文);
    return 丢失后(设查.项, 果(码.成, 登记('渲染管线', 设查.项.设备号, 值, {格式: 鹄们[0]?.格式 ?? null})));
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
    // 文言：索引之字段，或为号，或为首索基顶之录。汉语：绘制的索引字段可以是缓冲号（索引绘制），也可以是带首索引与基顶点的对象（索引段绘制，0.3.0）。
    const 解索引 = 号 => {
      if (号 === null || 号 === undefined) return null;
      if (typeof 号 === 'object') {
        if (!非负整(号.首) || !非负整(号.基)) throw {码: 码.校验错误, 文: '索引段的首索引或基顶点无效'};
        return {物: 取(号.缓冲, '缓冲').物, 首: 号.首, 基: 号.基};
      }
      return 取(号, '缓冲').物;
    };
    if (!Array.isArray(命令们)) throw {码: 码.校验错误, 文: '命令列表无效'};
    return 命令们.map(令 => {
      if (令?.种 === '派发') return {种: '派发', 管线: 取(令.管线, '计算管线').物, 组: 取列(令.组, '绑定组'), 数: 令.数};
      if (令?.种 === '通道') {
        if (!Array.isArray(令.绘)) throw {码: 码.校验错误, 文: '绘制列表无效'};
        // 文言：色附之列（零点四版），缺则以纹与清为一附。汉语：颜色附件列（0.4.0 的全述通道，每项纹理号、清屏色或空、合入纹理号或空），没有时以旧的纹理与清屏色为唯一的颜色附件。
        if (令.色附们 !== undefined && !Array.isArray(令.色附们)) throw {码: 码.校验错误, 文: '颜色附件列表无效'};
        const 色附们 = Array.isArray(令.色附们)
          ? 令.色附们.map(附 => ({纹理: 取(附?.纹理, '纹理').物, 清: 附.清, 合入: 附.合入 === null || 附.合入 === undefined ? null : 取(附.合入, '纹理').物}))
          : [{纹理: 取(令.纹理, '纹理').物, 清: 令.清, 合入: null}];
        // 文言：带深之道，解其深纹。汉语：带深度附件的通道（0.2.0）另解析深度纹理，清深值为数或空（保留深度）。
        const 深 = 令.深度 === null || 令.深度 === undefined ? null : {纹理: 取(令.深度.纹理, '纹理').物, 清: 令.深度.清};
        if (!色附们.length && !深) throw {码: 码.校验错误, 文: '渲染通道至少要有一个附件'};
        return {种: '通道', 色附们, 深度: 深, 绘: 令.绘.map(绘 => ({管线: 取(绘.管线, '渲染管线').物, 组: 取列(绘.组, '绑定组'),
          顶点: 取列(绘.顶点, '缓冲'), 索引: 解索引(绘.索引), 数: 绘.数, 实例: 绘.实例}))};
      }
      // 文言：衍层之令（零点四版）：其纹须可滤可绘而单采。汉语：生成渐远层（0.4.0）：纹理须是能过滤、能作渲染目标的单采样颜色纹理。
      if (令?.种 === '衍层') {
        const 纹 = 取(令.纹理, '纹理');
        if (纹.格式 === 深度格式 || 不可滤格式.has(纹.格式) || (纹.采样数 ?? 1) > 1)
          throw {码: 码.校验错误, 文: '渐远层只能由能过滤、能作渲染目标的单采样颜色纹理生成'};
        return {种: '衍层', 纹理: 纹};
      }
      if (令?.种 === '复制') return {种: '复制', 源: 取(令.源, '缓冲').物, 源偏: 令.源偏, 的: 取(令.的, '缓冲').物, 的偏: 令.的偏, 长: 令.长};
      throw {码: 码.校验错误, 文: '未知图形命令'};
    });
  };
  // 文言：网页图枢无现成之衍层，以内置之绘管逐层缩画：每层以线滤采上层而画之，得邻二乘二素之均。管线依设备与格式而存。
  // 汉语：WebGPU 没有现成的渐远层生成：用一条内置渲染管线逐层缩画，每层以线性过滤在上一层相邻二乘二纹素的交点采样，得它们的平均；管线按设备与格式缓存。
  const 衍层着色 = `@group(0) @binding(0) var 源: texture_2d<f32>;
@group(0) @binding(1) var 采: sampler;
struct 出 { @builtin(position) 位: vec4f, @location(0) 坐: vec2f };
@vertex fn 顶(@builtin(vertex_index) 序: u32) -> 出 {
  let 坐 = vec2f(f32((序 << 1u) & 2u), f32(序 & 2u));
  return 出(vec4f(坐.x * 2.0 - 1.0, 1.0 - 坐.y * 2.0, 0.0, 1.0), 坐);
}
@fragment fn 片(入: 出) -> @location(0) vec4f { return textureSampleLevel(源, 采, 入.坐, 0.0); }`;
  const 衍层缓存 = new WeakMap();
  const 衍层管 = (设备, 格式) => {
    let 表 = 衍层缓存.get(设备);
    if (!表) { 表 = new Map(); 衍层缓存.set(设备, 表); }
    let 项 = 表.get(格式);
    if (!项) {
      const 模块 = 设备.createShaderModule({code: 衍层着色});
      项 = {管线: 设备.createRenderPipeline({layout: 'auto', vertex: {module: 模块, entryPoint: '顶'},
        fragment: {module: 模块, entryPoint: '片', targets: [{format: 格式}]}, primitive: {topology: 'triangle-list'}}),
      采样器: 设备.createSampler({magFilter: 'linear', minFilter: 'linear'})};
      表.set(格式, 项);
    }
    return 项;
  };
  const 录衍层 = (编, 设备, 纹) => {
    const {管线, 采样器} = 衍层管(设备, 纹.格式);
    for (let 层 = 1; 层 < (纹.层数 ?? 1); 层++) {
      const 组 = 设备.createBindGroup({layout: 管线.getBindGroupLayout(0),
        entries: [{binding: 0, resource: 纹.物.createView({baseMipLevel: 层 - 1, mipLevelCount: 1})}, {binding: 1, resource: 采样器}]});
      const 通 = 编.beginRenderPass({colorAttachments: [{view: 纹.物.createView({baseMipLevel: 层, mipLevelCount: 1}), loadOp: 'clear',
        clearValue: {r: 0, g: 0, b: 0, a: 0}, storeOp: 'store'}]});
      通.setPipeline(管线);
      通.setBindGroup(0, 组);
      通.draw(3);
      通.end();
    }
  };
  const 录命令 = (编, 令, 设备) => {
    if (令.种 === '派发') {
      const 通 = 编.beginComputePass();
      通.setPipeline(令.管线);
      令.组.forEach((组, 序) => 通.setBindGroup(序, 组));
      通.dispatchWorkgroups(令.数?.[0], 令.数?.[1], 令.数?.[2]);
      通.end();
    } else if (令.种 === '衍层') {
      录衍层(编, 设备, 令.纹理);
    } else if (令.种 === '通道') {
      // 文言：多采之附设合入之鹄，书回弃其多采之内。汉语：多重采样附件设 resolveTarget 合入，多重采样纹理本身的内容不保留（storeOp discard）。
      const 附件们 = 令.色附们.map(附 => {
        const 清 = Array.isArray(附.清) ? 附.清 : null;
        const 附件 = {view: 取视图(附.纹理), loadOp: 清 ? 'clear' : 'load', storeOp: 附.合入 ? 'discard' : 'store'};
        if (清) 附件.clearValue = {r: 清[0], g: 清[1], b: 清[2], a: 清[3]};
        if (附.合入) 附件.resolveTarget = 取视图(附.合入);
        return 附件;
      });
      const 通道描述 = {colorAttachments: 附件们};
      if (令.深度) {
        const 深清 = typeof 令.深度.清 === 'number' ? 令.深度.清 : null;
        通道描述.depthStencilAttachment = {view: 取视图(令.深度.纹理), depthLoadOp: 深清 === null ? 'load' : 'clear', depthStoreOp: 'store'};
        if (深清 !== null) 通道描述.depthStencilAttachment.depthClearValue = 深清;
      }
      const 通 = 编.beginRenderPass(通道描述);
      for (const 绘 of 令.绘) {
        通.setPipeline(绘.管线);
        绘.组.forEach((组, 序) => 通.setBindGroup(序, 组));
        绘.顶点.forEach((缓, 序) => 通.setVertexBuffer(序, 缓));
        if (绘.索引) {
          const 索引物 = typeof 绘.索引 === 'object' ? 绘.索引.物 : 绘.索引;
          通.setIndexBuffer(索引物, 'uint32');
          // 文言：有首索基顶者依之而绘。汉语：索引段绘制带上首索引与基顶点（0.3.0），索引绘制两者都是零。
          if (typeof 绘.索引 === 'object') 通.drawIndexed(绘.数, 绘.实例, 绘.索引.首, 绘.索引.基);
          else 通.drawIndexed(绘.数, 绘.实例);
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
      for (const 令 of 令们) 录命令(编, 令, 设备);
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

// ---------------------------------------------------------------------------
// 字体：页面声明的字体文件
// ---------------------------------------------------------------------------
// 文言：字体之能：页以 <link data-yy-字体="族名" href> 声明字体之文件；初取乃全取而存之，读则切之。族名未声明者返资源不存在。
// 汉语：字体接口的浏览器宿主实现（原语 豫言_浏览器_字体(操作, 族名, 号, 偏移, 长度)，返回 [码, 号, 长度, 字体集序号, 字节, 文字]）。
//       页面用 <link data-yy-字体="族名" href="…" data-yy-字体序号="0"> 声明字体文件（可兼作 rel="preload" as="font"）；取得 第一次按 href
//       取回整个文件并缓存，之后同一族名得同一个号；读取 按偏移切片。没有声明的族名返回资源不存在。待办事项：大文件整取占内存，以后可改按 Range 分段取。
export function 创建字体能力({根, 全局, 单次上限 = 单次交换上限}) {
  const 空 = new Uint8Array(0);
  const 果 = (码值, 号 = 0, 长度 = 0, 序号 = 0, 字节 = 空, 文 = '') => [码值, 号, 长度, 序号, 字节, 文];
  const 表 = new Map();
  const 族号 = new Map();
  let 下号 = 1;
  const 找声明 = 族名 => [...(根?.querySelectorAll?.('link[data-yy-字体]') ?? [])].find(元素 => 元素.getAttribute('data-yy-字体') === 族名);
  const 取得 = async 族名 => {
    const 旧 = 族号.get(族名);
    if (旧) { const 项 = await 旧; return 项.码 ? 果(项.码, 0, 0, 0, 空, 项.文) : 果(码.成, 项.号, 项.字节.length, 项.序号); }
    const 元素 = 找声明(族名);
    if (!元素) return 果(码.不存在, 0, 0, 0, 空, '页面没有声明字体「' + 族名 + '」（<link data-yy-字体="' + 族名 + '" href="…">）');
    const 序号 = Math.max(0, Math.trunc(Number(元素.getAttribute('data-yy-字体序号') ?? 0)) || 0);
    const 址 = 元素.href || 元素.getAttribute('href') || '';
    const 取 = (async () => {
      try {
        const 应 = await 全局.fetch(址);
        if (!应.ok) return {码: 码.宿主失败, 文: '取字体文件失败：HTTP ' + 应.status + ' ' + 址};
        const 字节 = new Uint8Array(await 应.arrayBuffer());
        const 号 = 下号++;
        表.set(号, 字节);
        return {码: 码.成, 号, 字节, 序号};
      } catch (错) { return {码: 码.宿主失败, 文: '取字体文件失败：' + 消息(错)}; }
    })();
    族号.set(族名, 取);
    const 项 = await 取;
    if (项.码) { 族号.delete(族名); return 果(项.码, 0, 0, 0, 空, 项.文); }
    return 果(码.成, 项.号, 项.字节.length, 项.序号);
  };
  const 读取 = (号, 偏移, 长度) => {
    const 字节 = 表.get(号);
    if (!字节) return 果(码.已失效, 0, 0, 0, 空, '字体号无效');
    if (!(Number.isSafeInteger(偏移) && Number.isSafeInteger(长度) && 偏移 >= 0 && 长度 >= 0 && 偏移 + 长度 <= 字节.length)) {
      return 果(码.输入无效, 0, 0, 0, 空, '读取范围越出字体文件');
    }
    if (长度 > 单次上限) return 果(码.配额已尽, 0, 0, 0, 空, '单次读取超过上限');
    return 果(码.成, 号, 长度, 0, 字节.slice(偏移, 偏移 + 长度));
  };
  const 调用 = (操作, 族名, 号, 偏移, 长度) => {
    if (操作 === '取得') return 取得(族名);
    if (操作 === '读取') return 读取(号, 偏移, 长度);
    return 果(码.输入无效, 0, 0, 0, 空, '未知的字体操作：' + 操作);
  };
  return {调用};
}
