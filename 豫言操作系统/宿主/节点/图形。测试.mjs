// 文言：验节点之显示与图形之宿主术：授权、依赖之缺、后台之窗、事之转译、易窗、离屏之帧画于窗而交换、清理；以伪 SDL 与伪 GPU 行之，不开真窗。
// 汉语：Node 宿主显示与图形原语（图形.mjs）的单元测试：授权与依赖缺失、后台隐藏窗口、SDL 输入事件转换、换成 WebGPU 窗口、离屏帧纹理画到窗口并交换、
//       两种提交互斥、应用结束时的清理。注入伪 SDL 与伪 GPU 运行，不开真窗口、不需要原生依赖，全树测试可直接跑；真窗口的验收见
//       豫言操作系统/适配/图形显示节点/一致性验证/。
import assert from 'node:assert/strict';
import {EventEmitter} from 'node:events';
import 路径 from 'node:path';
import test from 'node:test';
import {创建显示面表, 创建图形能力, 状态码, 事件种类} from '../浏览器/图形.mjs';
import {创建节点图形能力, 译SDL键} from './图形.mjs';
import {解析宿主参数, 创建原生载入器} from './应用宿主.mjs';

const 码 = 状态码;
const 编码 = new TextEncoder();
const 字 = 文 => 编码.encode(文);

// ---------------------------------------------------------------------------
// 伪 SDL：窗口是事件发射器，像素比为二
// ---------------------------------------------------------------------------
class 伪窗 extends EventEmitter {
  constructor(选项) {
    super();
    Object.assign(this, {选项, title: 选项.title, width: 选项.width, height: 选项.height, pixelWidth: 选项.width * 2, pixelHeight: 选项.height * 2,
      webgpu: Boolean(选项.webgpu), visible: 选项.visible, x: 选项.x ?? 100, y: 选项.y ?? 50, destroyed: false, 画们: []});
  }
  render(宽, 高, 跨, 格式, 缓) {
    if (this.webgpu) throw Error("can't call render in webgpu mode");
    assert.ok(Buffer.isBuffer(缓), 'render 须收 Buffer');
    this.画们.push({宽, 高, 跨, 格式, 缓: Uint8Array.from(缓)});
  }
  destroy() {
    if (this.destroyed) throw Error('window is destroyed');
    this.destroyed = true;
    this.emit('close', {type: 'close'});
  }
}
const 造SDL = () => {
  const 窗们 = [];
  return {窗们, video: {createWindow: 选项 => { const 窗 = new 伪窗(选项); 窗们.push(窗); return 窗; }}};
};

// ---------------------------------------------------------------------------
// 伪 GPU：够测离屏帧、搬运画到窗口、读回与校验
// ---------------------------------------------------------------------------
const 每像素 = {'rgba8unorm': 4, 'bgra8unorm': 4};
function 伪设备() {
  const 域 = [];
  const 设备 = {
    limits: {maxBufferSize: 1 << 26, maxStorageBufferBindingSize: 1 << 26, maxTextureDimension2D: 4096, maxComputeWorkgroupsPerDimension: 65535,
      maxComputeInvocationsPerWorkgroup: 256},
    报错(消息) { const 顶 = [...域].reverse().find(项 => 项.类 === 'validation'); if (顶 && !顶.误) 顶.误 = {message: 消息}; else if (!顶) 设备.未捕获.push(消息); },
    未捕获: [],
    pushErrorScope(类) { 域.push({类, 误: null}); },
    popErrorScope() { const 顶 = 域.pop(); return 顶 ? Promise.resolve(顶.误) : Promise.reject(Error('错误域栈空')); },
    createBuffer({size, usage}) {
      return {size, usage, 数据: new Uint8Array(size), destroy() {}, async mapAsync() {}, getMappedRange() { return this.数据.buffer; }, unmap() {}};
    },
    createTexture({size: [宽, 高], format, usage}) {
      const 纹 = {width: 宽, height: 高, format, usage, 数据: new Uint8Array(宽 * 高 * 每像素[format]), 已毁: false, destroy() { this.已毁 = true; }};
      // 文言：仿 Dawn：createView 恒不过。汉语：模仿 Dawn 0.2.1：JS 的 createView() 总是校验失败，Node 路径不应调用它。
      纹.createView = () => { throw Error('swizzle used without the FeatureName::TextureComponentSwizzle feature enabled.'); };
      return 纹;
    },
    createShaderModule({code}) { return {code, async getCompilationInfo() { return {messages: []}; }}; },
    createRenderPipeline(描述) { return {描述, 搬运: 描述.vertex.entryPoint === '顶点', getBindGroupLayout: 序 => ({序})}; },
    async createRenderPipelineAsync(描述) { return {描述, getBindGroupLayout: 序 => ({序})}; },
    createBindGroup(描述) { return {描述}; },
    createCommandEncoder() {
      const 编 = {令: [], 有效: true};
      编.copyTextureToBuffer = ({texture}, {buffer, bytesPerRow}, [宽, 高]) => {
        if (!(texture.usage & 1)) { 编.有效 = false; 设备.报错('纹理没有复制源用途'); }
        const 行 = 宽 * 每像素[texture.format];
        编.令.push(() => { for (let 序 = 0; 序 < 高; 序++) buffer.数据.set(texture.数据.subarray(序 * 行, 序 * 行 + 行), 序 * bytesPerRow); });
      };
      编.beginRenderPass = ({colorAttachments: [附件]}) => {
        // 文言：附件或为视图，或径为纹理。汉语：附件可以是视图，也可以直接是纹理（Node 直接传纹理）。
        const 纹 = 附件.view.纹理 ?? 附件.view;
        if (附件.loadOp === 'clear') {
          const 色 = 附件.clearValue;
          编.令.push(() => { for (let 位 = 0; 位 < 纹.数据.length; 位 += 4) 纹.数据.set([色.r, 色.g, 色.b, 色.a].map(值 => Math.round(值 * 255)), 位); });
        }
        const 通 = {管线: null, 组们: [], setPipeline(管) { this.管线 = 管; }, setBindGroup(序, 组) { this.组们[序] = 组; },
          setVertexBuffer() {}, setIndexBuffer() {}, drawIndexed() {},
          draw() {
            // 文言：搬运之绘：以所绑之纹理覆写目标。汉语：搬运管线的绘制：把绑定的帧纹理数据复制到目标纹理。
            if (!this.管线?.搬运) return;
            const 资 = this.组们[0].描述.entries[0].resource;
            const 源 = 资.纹理 ?? 资;
            编.令.push(() => 纹.数据.set(源.数据.subarray(0, 纹.数据.length)));
          },
          end() {}};
        return 通;
      };
      编.finish = () => ({令: 编.令, 有效: 编.有效});
      return 编;
    },
    queue: {
      writeBuffer(缓, 偏移, 数据) { 缓.数据.set(数据, 偏移); },
      writeTexture({texture}, 数据) { texture.数据.set(数据); },
      submit(缓冲们) {
        for (const 缓 of 缓冲们) if (!缓.有效) { 设备.报错('命令缓冲无效'); return; }
        for (const 缓 of 缓冲们) for (const 令 of 缓.令) 令();
      },
      async onSubmittedWorkDone() {}
    },
    已毁: false,
    destroy() { 设备.已毁 = true; }
  };
  设备.lost = new Promise(() => {});
  return 设备;
}
const 造GPU模 = () => {
  const 记 = {设备们: [], 渲染器们: [], 已销毁实例: 0};
  const 实例 = {
    getPreferredCanvasFormat: () => 'bgra8unorm',
    requestAdapter: async () => ({limits: {maxBufferSize: 1 << 26}, requestDevice: async () => { const 设 = 伪设备(); 记.设备们.push(设); return 设; }})
  };
  return {
    记,
    create: () => 实例,
    destroy: 例 => { assert.equal(例, 实例); 记.已销毁实例++; },
    renderGPUDeviceToWindow: ({device, window, presentMode}) => {
      if (!window.webgpu) throw Error('窗口须以 webgpu 创建');
      const 渲 = {device, window, presentMode, 交换数: 0, 重配数: 0, 表面们: [],
        getPreferredFormat: () => 'bgra8unorm',
        getCurrentTextureView() {
          const 纹 = device.createTexture({size: [window.pixelWidth, window.pixelHeight], format: 'bgra8unorm', usage: 16});
          this.表面们.push(纹);
          return {纹理: 纹};
        },
        getCurrentTexture() { throw Error('不应调用 getCurrentTexture（Dawn 退出时引用计数断言失败）'); },
        swap() { this.交换数++; },
        resize() { this.重配数++; }};
      记.渲染器们.push(渲);
      return 渲;
    }
  };
};

const 造 = ({授权 = [['甲', {宽: 32, 高: 24}], ['乙', {宽: 8, 高: 4}]], 有SDL = true, 有GPU = true, 后台 = true} = {}) => {
  const sdl = 造SDL();
  const gpu = 造GPU模();
  const 误们 = [];
  const 载入原生 = 名 => {
    if (名 === '@kmamal/sdl' && 有SDL) return sdl;
    if (名 === '@kmamal/gpu' && 有GPU) return gpu;
    throw Error('找不到原生依赖 ' + 名);
  };
  const 能 = 创建节点图形能力({创建显示面表, 创建图形能力, 状态码, 事件种类, 载入原生, 授权显示面: new Map(授权), 后台, 写错误: 文 => 误们.push(文)});
  const 显 = (操作, 号 = 0, 甲 = 0, 乙 = 0, 字节 = new Uint8Array(), 丙 = 0, 丁 = 0) =>
    能.原语.豫言_节点_显示(字(操作), BigInt(号), BigInt(甲), BigInt(乙), 字节, BigInt(丙), BigInt(丁));
  const 图 = (操作, 参 = {}, 字节 = new Uint8Array()) => 能.原语.豫言_节点_图形(字(操作), 字(JSON.stringify(参)), 字节);
  const 取面 = 名 => { const 果 = 显('取得', 0, 0, 0, 字(名)); assert.equal(果[0], 码.成, 果[5]); return 果[1]; };
  return {能, sdl, gpu, 误们, 显, 图, 取面};
};
const 取号 = async 承诺 => { const 果 = await 承诺; assert.equal(果[0], 码.成, 果[2]); return 果[1]; };

test('SDL尺寸及显示器变化唤醒等待，输入顺序保留而相邻尺寸通知合并', async () => {
  const {显,sdl,取面} = 造(); const 号 = 取面('甲'), 窗 = sdl.窗们[0];
  const 候 = 显('等待',号); 窗.emit('resize',{});
  assert.equal((await 候)[1],事件种类.显示尺寸变化);
  窗.emit('resize',{}); 窗.emit('displayChange',{});
  窗.emit('textInput',{text:'保留输入'}); 窗.emit('resize',{});
  assert.equal((await 显('等待',号))[1],事件种类.显示尺寸变化);
  const 文 = await 显('等待',号); assert.equal(文[1],事件种类.文字输入); assert.equal(文[5],'保留输入');
  assert.equal((await 显('等待',号))[1],事件种类.显示尺寸变化);
  显('归还',号); 窗.emit('resize',{}); assert.equal((await 显('等待',号))[0],码.已失效);
});

// ---------------------------------------------------------------------------
// 一、宿主选项与原生依赖
// ---------------------------------------------------------------------------
test('归还仅关闭当前窗口，授权保留可再开，新窗口号独立', async () => {
  const {能, sdl, 显, 取面} = 造();
  const 甲 = 取面('甲'), 乙 = 取面('乙');
  const 窗 = sdl.窗们[0];
  窗.emit('mouseMove', {x: 1, y: 2});
  assert.equal(显('归还', 甲)[0], 码.成);
  assert.ok(窗.destroyed);
  assert.equal(显('尺寸', 甲)[0], 码.已失效);
  assert.equal(显('等待', 甲)[0], 码.已失效);
  assert.equal(显('尺寸', 乙)[0], 码.成);
  const 新甲 = 取面('甲');
  assert.notEqual(新甲, 甲);
  assert.equal(显('尺寸', 新甲)[0], 码.成);
  await 能.清理();
});

test('宿主选项：授权显示面、显示面后台、原生依赖目录与同义环境变量', () => {
  const 配 = 解析宿主参数(['--授权显示面', '主画面=640x480', '--显示面后台', '--原生依赖目录', '依赖', '--授权显示面', '小=3×4', '--', '应用参数'], '/根', {});
  assert.deepEqual([...配.显示面], [['主画面', {宽: 640, 高: 480}], ['小', {宽: 3, 高: 4}]]);
  assert.equal(配.显示面后台, true);
  assert.equal(配.原生依赖目录, 路径.resolve('/根', '依赖'));
  assert.deepEqual(配.应用参数, ['应用参数']);
  const 环 = 解析宿主参数(['--原生依赖目录', '选项目录'], '/根', {YY_NODE_NATIVE_DIR: '环境目录', YY_NODE_DISPLAY_BACKGROUND: '1'});
  assert.equal(环.原生依赖目录, 路径.resolve('/根', '选项目录'), '选项优先于环境变量');
  assert.equal(环.显示面后台, true);
  assert.equal(解析宿主参数([], '/根', {YY_NODE_NATIVE_DIR: '环境目录'}).原生依赖目录, 路径.resolve('/根', '环境目录'));
  assert.equal(解析宿主参数([], '/根', {}).显示面后台, false);
  assert.throws(() => 解析宿主参数(['--授权显示面', '坏'], '/根', {}), /名称=宽x高/u);
  assert.throws(() => 解析宿主参数(['--授权显示面', '大=70000x1'], '/根', {}), /不得超过/u);
});

test('原生依赖找不到时抛出带提示的错误，并缓存结果', () => {
  const 载 = 创建原生载入器(null, '/');
  assert.throws(() => 载('@kmamal/并不存在的包'), /--原生依赖目录/u);
  assert.throws(() => 载('@kmamal/并不存在的包'), /YY_NODE_NATIVE_DIR/u);
});

// ---------------------------------------------------------------------------
// 二、显示
// ---------------------------------------------------------------------------
test('显示面：未授权为未获授权；缺 SDL 为资源暂不可用且只提示一次；后台模式开隐藏窗口并设 SDL_MAC_BACKGROUND_APP；同名得同一显示面', () => {
  const 缺 = 造({有SDL: false});
  assert.equal(缺.显('取得', 0, 0, 0, 字('甲'))[0], 码.暂不可用);
  assert.equal(缺.显('取得', 0, 0, 0, 字('甲'))[0], 码.暂不可用);
  assert.equal(缺.误们.length, 1, '标准错误只提示一次');
  assert.match(缺.误们[0], /@kmamal\/sdl/u);
  const {显, sdl, 取面} = 造();
  assert.equal(显('取得', 0, 0, 0, 字('丙'))[0], 码.未获授权);
  assert.equal(显('取得', 0, 0, 0, 字(''))[0], 码.未获授权);
  const 号 = 取面('甲');
  assert.equal(取面('甲'), 号, '同名得同一显示面');
  assert.equal(sdl.窗们.length, 1);
  const 窗 = sdl.窗们[0];
  assert.deepEqual([窗.title, 窗.width, 窗.height, 窗.visible, 窗.webgpu], ['甲', 32, 24, false, false]);
  assert.equal(process.env.SDL_MAC_BACKGROUND_APP, '1');
  const 前台 = 造({后台: false});
  前台.取面('甲');
  assert.equal(前台.sdl.窗们[0].visible, true, '非后台模式开可见窗口');
});

test('尺寸按物理像素；提交像素：宽高与字节数不合为输入无效、尺寸不同为资源暂不可用，合规则以 rgba32 提交', () => {
  const {显, sdl, 取面} = 造();
  const 号 = 取面('甲');
  assert.deepEqual(显('尺寸', 号).slice(0, 3), [码.成, 64, 48]);
  const 帧 = new Uint8Array(64 * 48 * 4).fill(9);
  assert.equal(显('提交', 号, 64, 48, new Uint8Array(3))[0], 码.输入无效);
  assert.equal(显('提交', 号, 0, 48, new Uint8Array())[0], 码.输入无效);
  assert.equal(显('提交', 号, 48, 64, 帧)[0], 码.暂不可用);
  assert.equal(显('提交', 号, 64, 48, 帧)[0], 码.成);
  const 画 = sdl.窗们[0].画们[0];
  assert.deepEqual([画.宽, 画.高, 画.跨, 画.格式, 画.缓.length, 画.缓[0]], [64, 48, 256, 'rgba32', 64 * 48 * 4, 9]);
});

test('SDL 键名译法：七个语义键与 0.2.0 的编辑键、修饰键，单个标量值原样，字母随 Shift 与大写锁定变大小写，其余不交付', () => {
  assert.deepEqual(['left', 'right', 'up', 'down', 'return', 'enter', 'backspace', 'space'].map(键 => 译SDL键({key: 键})),
    ['左', '右', '上', '下', '回车', '回车', '退格', '空格']);
  assert.deepEqual([{key: 'a'}, {key: 'a', shift: true}, {key: 'a', capslock: true}, {key: 'a', shift: true, capslock: true}, {key: '1', shift: true}, {key: 'ö'}]
    .map(译SDL键), ['a', 'A', 'A', 'a', '1', 'ö']);
  assert.deepEqual(['delete', 'home', 'end', 'pageUp', 'pageDown', 'tab', 'escape'].map(键 => 译SDL键({key: 键})),
    ['删除', '起首', '末尾', '上翻页', '下翻页', '制表', '退出']);
  assert.deepEqual(['shift', 'ctrl', 'alt', 'gui', 'gUI'].map(键 => 译SDL键({key: 键})), ['上档', '控制', '交替', '命令', '命令']);
  assert.deepEqual(['f1', 'capsLock', null, ''].map(键 => 译SDL键({key: 键})), [null, null, null, null]);
  assert.equal(译SDL键({key: 'f2'}), '功能二');
});

test('输入事件：坐标乘像素比、按钮号减一、相邻移动合并、文字输入；关闭窗口后先报关闭再为资源已失效', async () => {
  const {显, sdl, 取面} = 造();
  const 号 = 取面('甲');
  const 窗 = sdl.窗们[0];
  const 等 = 显('等待', 号);
  assert.ok(等 instanceof Promise, '无事件时挂起');
  assert.equal(显('等待', 号)[0], 码.输入无效, '第二个并发等候者得输入无效');
  窗.emit('mouseButtonDown', {x: 5, y: 2.5, button: 1});
  assert.deepEqual(await 等, [码.成, 事件种类.指针按下, 10, 5, 0, '', 0]);
  窗.emit('mouseMove', {x: 1, y: 1});
  窗.emit('mouseMove', {x: 10, y: 3});
  窗.emit('mouseButtonUp', {x: 10, y: 3, button: 3});
  窗.emit('keyDown', {key: 'left'});
  窗.emit('keyDown', {key: 'shift'});
  窗.emit('keyUp', {key: 'left'});
  窗.emit('keyDown', {key: 'a'});
  窗.emit('textInput', {text: '豫言'});
  const 取 = () => 显('等待', 号);
  assert.deepEqual(取(), [码.成, 事件种类.指针移动, 20, 6, 0, '', 0], '两次移动合并为最后一次');
  assert.deepEqual(取(), [码.成, 事件种类.指针抬起, 20, 6, 2, '', 0]);
  assert.deepEqual(取(), [码.成, 事件种类.按键按下, 0, 0, 0, '左', 0]);
  assert.deepEqual(取(), [码.成, 事件种类.按键按下, 0, 0, 0, '上档', 0], '0.2.0 交付修饰键');
  assert.deepEqual(取(), [码.成, 事件种类.按键抬起, 0, 0, 0, '左', 0]);
  assert.deepEqual(取(), [码.成, 事件种类.按键按下, 0, 0, 0, 'a', 0]);
  assert.deepEqual(取(), [码.成, 事件种类.文字输入, 0, 0, 0, '豫言', 0]);
  窗.emit('blur', {});
  assert.deepEqual(取(), [码.成, 事件种类.按键抬起, 0, 0, 0, '上档', 0], '窗口失焦时为按着的修饰键补发抬起');
  窗.emit('mouseWheel', {x: 5, y: 5, dx: 0, dy: -1, flipped: false});
  窗.emit('mouseWheel', {x: 6, y: 7, dx: 1, dy: 0, flipped: false});
  assert.deepEqual(取(), [码.成, 事件种类.滚轮, 12, 14, 96, '', 96], '一格 48 逻辑像素乘像素比；SDL 向下为负故取反；相邻两次合并');
  窗.emit('mouseWheel', {x: 1, y: 1, dx: 0, dy: 1, flipped: true});
  assert.deepEqual(取(), [码.成, 事件种类.滚轮, 2, 2, 0, '', 96], 'flipped 时方向再反');
  assert.deepEqual(显('像素比', 号).slice(0, 3), [码.成, 64, 32], '像素比为像素宽比点宽');
  assert.equal(显('输入区域', 号, 1, 2, new Uint8Array(), 3, 4)[0], 码.成, 'SDL 没有输入区域，照常返回');
  const 再等 = 取();
  窗.destroy();
  assert.deepEqual(await 再等, [码.成, 事件种类.显示面已关闭, 0, 0, 0, '', 0]);
  assert.equal(取()[0], 码.已失效);
  assert.equal(显('尺寸', 号)[0], 码.已失效);
  assert.equal(显('提交', 号, 64, 48, new Uint8Array(64 * 48 * 4))[0], 码.已失效);
});

// ---------------------------------------------------------------------------
// 三、图形处理器与图形显示
// ---------------------------------------------------------------------------
test('图形处理器：缺 @kmamal/gpu 为资源暂不可用且只提示一次；有则取得设备、限额六项', async () => {
  const 缺 = 造({有GPU: false});
  assert.equal((await 缺.图('设备.取得'))[0], 码.暂不可用);
  assert.equal((await 缺.图('设备.取得'))[0], 码.暂不可用);
  assert.equal(缺.误们.length, 1);
  assert.match(缺.误们[0], /@kmamal\/gpu/u);
  const {图} = 造();
  const 设 = await 取号(图('设备.取得'));
  assert.equal(JSON.parse(图('设备.限额', {设备: 设})[2]).length, 6);
});

test('读回的复制校验不通过时返回图形校验错误，不读出全零', async () => {
  const {图, gpu} = 造();
  const 设 = await 取号(图('设备.取得'));
  const 纹 = await 取号(图('纹理.新建', {设备: 设, 宽: 2, 高: 2, 格式: 'rgba8unorm', 用途: ['渲染']}));
  assert.equal((await 图('纹理.读回', {纹理: 纹}))[3].length, 16);
  // 文言：伪去其复制源之位，验误域之包。汉语：把伪纹理的复制源用途位去掉，验证读回的错误域。
  const 设备 = gpu.记.设备们[0];
  const 原建 = 设备.createTexture;
  设备.createTexture = 描述 => 原建({...描述, usage: 描述.usage & ~1});
  const 无源 = await 取号(图('纹理.新建', {设备: 设, 宽: 2, 高: 2, 格式: 'rgba8unorm', 用途: ['渲染']}));
  const 果 = await 图('纹理.读回', {纹理: 无源});
  assert.equal(果[0], 码.校验错误);
  assert.match(果[2], /复制源/u);
  assert.equal(设备.未捕获.length, 0, '错误都被错误域接住');
});

test('图形显示：取纹理时换成 WebGPU 窗口；离屏帧可读回；呈现把帧画到窗口当前纹理并交换；旧帧失效；两种提交互斥；清理关窗并销毁 Dawn 实例', async () => {
  const {能, 显, 图, sdl, gpu, 取面} = 造();
  const 面 = 取面('甲');
  const 像面 = 取面('乙');
  const 设 = await 取号(图('设备.取得'));
  assert.equal(图('呈现.格式', {显示面: 面, 设备: 设})[2], 'bgra8unorm');
  assert.equal((await 图('呈现.呈现', {显示面: 面}))[0], 码.输入无效, '未取纹理即呈现');
  const 帧一 = await 取号(图('呈现.取纹理', {显示面: 面, 设备: 设}));
  assert.equal(await 取号(图('呈现.取纹理', {显示面: 面, 设备: 设})), 帧一, '呈现前重复取得同一纹理');
  const [旧窗, 像窗, 新窗] = sdl.窗们;
  assert.ok(旧窗.destroyed && !旧窗.webgpu, '原来的普通窗口已换掉');
  assert.deepEqual([新窗.webgpu, 新窗.visible, 新窗.title, 新窗.x, 新窗.y], [true, false, '甲', 旧窗.x, 旧窗.y], '新窗口在旧窗口的位置');
  assert.equal(显('尺寸', 面)[0], 码.成, '换窗不算显示面关闭');
  const 渲 = gpu.记.渲染器们[0];
  assert.deepEqual([渲.window, 渲.presentMode], [新窗, 'fifo']);
  assert.equal((await 图('纹理.读回', {纹理: 帧一}))[3].length, 64 * 48 * 4, '帧纹理尺寸等于显示面尺寸');
  assert.equal((await 图('命令.提交', {设备: 设, 命令: [{种: '通道', 纹理: 帧一, 清: [0, 0, 1, 1], 绘: []}]}))[0], 码.成);
  assert.equal((await 图('呈现.呈现', {显示面: 面}))[0], 码.成);
  assert.equal(渲.交换数, 1);
  const 表面 = 渲.表面们.at(-1);
  assert.deepEqual([...表面.数据.subarray(0, 4)], [0, 0, 255, 255], '帧纹理画到了窗口当前纹理');
  assert.equal((await 图('命令.提交', {设备: 设, 命令: [{种: '通道', 纹理: 帧一, 清: [0, 0, 1, 1], 绘: []}]}))[0], 码.已失效, '呈现后旧帧失效');
  assert.equal((await 图('呈现.呈现', {显示面: 面}))[0], 码.输入无效);
  const 帧二 = await 取号(图('呈现.取纹理', {显示面: 面, 设备: 设}));
  assert.notEqual(帧二, 帧一);
  assert.equal(显('提交', 面, 64, 48, new Uint8Array(64 * 48 * 4))[0], 码.暂不可用, 'GPU 呈现过的显示面不能提交像素');
  assert.equal(显('提交', 像面, 16, 8, new Uint8Array(16 * 8 * 4))[0], 码.成);
  assert.equal((await 图('呈现.取纹理', {显示面: 像面, 设备: 设}))[0], 码.暂不可用, '提交过像素的显示面不能取纹理');
  assert.ok(!像窗.destroyed && !像窗.webgpu);
  const 另设 = await 取号(图('设备.取得'));
  assert.equal((await 图('呈现.取纹理', {显示面: 面, 设备: 另设}))[0], 码.输入无效, '显示面已与别的设备绑定');
  // 文言：窗之素数易则重配其面。汉语：窗口像素尺寸变了，呈现前重配窗口表面。
  新窗.pixelWidth = 96;
  新窗.pixelHeight = 72;
  const 帧三 = await 取号(图('呈现.取纹理', {显示面: 面, 设备: 设}));
  assert.equal(帧三, 帧二, '本帧尚未呈现，仍是同一纹理');
  assert.equal((await 图('呈现.呈现', {显示面: 面}))[0], 码.成);
  assert.equal(渲.重配数, 1);
  assert.equal((await 图('纹理.读回', {纹理: await 取号(图('呈现.取纹理', {显示面: 面, 设备: 设}))}))[3].length, 96 * 72 * 4, '下一帧随窗口尺寸');
  await 能.清理();
  assert.ok(sdl.窗们.every(窗 => 窗.destroyed), '清理关闭全部窗口');
  assert.equal(gpu.记.已销毁实例, 1, '清理销毁 Dawn 实例');
  assert.ok(gpu.记.设备们.every(设备 => 设备.已毁), '清理释放全部设备');
});

// ---------------------------------------------------------------------------
// 字体：只读字体目录下的文件
// ---------------------------------------------------------------------------
test('字体：只开字体目录下的文件；同一文件同一个号；按偏移读取；越界为输入无效；目录外为未获授权；不存在为资源不存在；清理后号失效', async () => {
  const 文件系统 = await import('node:fs');
  const 系统 = await import('node:os');
  const 临时 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), '豫言字体测试-'));
  try {
    文件系统.mkdirSync(路径.join(临时, '.fonts'));
    const 字体径 = 路径.join(临时, '.fonts', '甲.ttf');
    文件系统.writeFileSync(字体径, Uint8Array.from([0, 1, 0, 0, 0, 4, 0x80, 0, 9, 8, 7]));
    文件系统.writeFileSync(路径.join(临时, '目录外.ttf'), Uint8Array.from([1, 2, 3]));
    const 能 = 创建节点图形能力({创建显示面表, 创建图形能力, 状态码, 事件种类, 载入原生: () => { throw Error('不需要'); }, 文件系统, 路径分隔: 路径.sep,
      平台: 'linux', 家目录: 临时, 环境: {}});
    const 字体 = (操作, 径 = '', 号 = 0, 偏移 = 0, 长度 = 0) => 能.原语.豫言_诺节_字体(字(操作), 字(径), BigInt(号), BigInt(偏移), BigInt(长度));
    const 开 = 字体('打开', 字体径);
    assert.equal(开[0], 码.成, 开[4]);
    assert.equal(开[2], 11);
    assert.equal(字体('打开', 字体径)[1], 开[1], '同一文件再开得同一个号');
    const 读 = 字体('读取', '', 开[1], 8, 3);
    assert.equal(读[0], 码.成, 读[4]);
    assert.deepEqual([...读[3]], [9, 8, 7]);
    assert.equal(字体('读取', '', 开[1], 0, 0)[3].length, 0, '长度为零得空字节');
    assert.equal(字体('读取', '', 开[1], 9, 3)[0], 码.输入无效);
    assert.equal(字体('读取', '', 开[1], -1, 1)[0], 码.输入无效);
    assert.equal(字体('读取', '', 开[1], 0, 17 * 1024 * 1024)[0], 码.输入无效, '越过文件末尾先报输入无效');
    assert.equal(字体('打开', 路径.join(临时, '目录外.ttf'))[0], 码.未获授权);
    assert.equal(字体('打开', 路径.join(临时, '.fonts', '无此.ttf'))[0], 码.不存在);
    assert.equal(字体('读取', '', 999, 0, 1)[0], 码.已失效);
    assert.equal(字体('别的')[0], 码.输入无效);
    await 能.清理();
    assert.equal(字体('读取', '', 开[1], 0, 1)[0], 码.已失效, '清理后号失效');
  } finally {
    文件系统.rmSync(临时, {recursive: true, force: true});
  }
});

test('字体：没有文件系统时打开返回资源暂不可用', () => {
  const 能 = 创建节点图形能力({创建显示面表, 创建图形能力, 状态码, 事件种类, 载入原生: () => { throw Error('不需要'); }});
  assert.equal(能.原语.豫言_诺节_字体(字('打开'), 字('/usr/share/fonts/甲.ttf'), 0n, 0n, 0n)[0], 码.暂不可用);
});
