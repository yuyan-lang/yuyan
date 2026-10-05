// 文言：验显示与图形之宿主工厂：授权、错误之路、事之转译、命令之编解与整列不行；以伪文档与伪 GPU 行之，不涉 Wasm 与 JSDOM。
// 汉语：显示与图形处理器宿主工厂的单元测试：授权、错误路径、输入事件转换、命令 JSON 解码与整列不执行、读回的行对齐、帧纹理与呈现。
//       用手写的伪文档、伪 canvas 与伪 WebGPU 运行，不需要 Wasm 与 JSDOM（CI 的全树测试可直接跑）；真实浏览器行为见图形显示浏览器适配的一致性验证。
import assert from 'node:assert/strict';
import test from 'node:test';
import {创建显示能力, 创建图形能力, 创建字体能力, 状态码, 译键, 事件种类, 单次交换上限} from './图形.mjs';

const 码 = 状态码;
const 编码 = new TextEncoder();
const 字 = 文 => 编码.encode(文);

// ---------------------------------------------------------------------------
// 伪文档与伪 canvas
// ---------------------------------------------------------------------------
class 伪元素 extends EventTarget {
  constructor(标签, 文档) {
    super();
    this.localName = 标签;
    this.ownerDocument = 文档;
    this.属性 = new Map();
    this.style = {};
    this.value = '';
    this.isConnected = true;
    this.聚焦次数 = 0;
  }
  setAttribute(名, 值) { this.属性.set(名, String(值)); if (名 === 'style') this.style = {文: String(值)}; }
  getAttribute(名) { return this.属性.has(名) ? this.属性.get(名) : null; }
  focus() { this.聚焦次数++; }
  remove() { this.isConnected = false; this.ownerDocument.元素们 = this.ownerDocument.元素们.filter(元 => 元 !== this); }
  after(元) { this.ownerDocument.元素们.push(元); }
}
class 伪画布 extends 伪元素 {
  constructor(文档, 名, {宽 = 32, 高 = 24} = {}) {
    super('canvas', 文档);
    if (名 !== null) this.setAttribute('data-yy-显示面', 名);
    this.width = 300;
    this.height = 150;
    this.矩 = {left: 10, top: 20, width: 宽, height: 高};
    this.二维 = {画: [], putImageData: (图, 横, 纵) => this.二维.画.push({图, 横, 纵})};
    this.图形境 = null;
    this.已取 = null;
  }
  getBoundingClientRect() { return this.矩; }
  getContext(种) {
    if (this.已取 && this.已取 !== 种) return null;
    this.已取 = 种;
    if (种 === '2d') return this.二维;
    if (种 === 'webgpu') return this.图形境 ??= 伪画布境(this);
    return null;
  }
}
const 造文档 = () => {
  const 文档 = {元素们: [], documentElement: {}};
  文档.querySelectorAll = 选 => (选 === 'canvas' ? 文档.元素们.filter(元 => 元.localName === 'canvas') : []);
  文档.createElement = 标签 => new 伪元素(标签, 文档);
  return 文档;
};
class 伪图像 { constructor(数据, 宽, 高) { this.data = 数据; this.width = 宽; this.height = 高; } }
const 造全局 = (额外 = {}) => ({devicePixelRatio: 2, ImageData: 伪图像, setTimeout, ...额外});
const 事件 = (类, 字段) => Object.assign(new Event(类), 字段);
const 可取消事件 = (类, 字段) => Object.assign(new Event(类, {cancelable: true}), 字段);

const 造显示 = ({关闭 = () => false} = {}) => {
  const 文档 = 造文档();
  const 全局 = 造全局();
  const 显示 = 创建显示能力({根: 文档, 全局, 已关闭: 关闭});
  const 加画布 = (名, 选项) => { const 画布 = new 伪画布(文档, 名, 选项); 文档.元素们.push(画布); return 画布; };
  return {文档, 全局, 显示, 加画布};
};

// ---------------------------------------------------------------------------
// 一、显示
// ---------------------------------------------------------------------------
test('归还显示会话结束等待并清理输入，同名再取新面号，旧号失效', async () => {
  const {显示, 加画布} = 造显示();
  加画布('甲');
  const 旧号 = 显示.调用('取得', 0, 0, 0, 字('甲'))[1];
  const 等待 = 显示.调用('等待', 旧号);
  assert.equal(显示.调用('归还', 旧号)[0], 码.成);
  assert.equal((await 等待)[1], 事件种类.显示面已关闭);
  assert.equal(显示.状态().显示面数, 0);
  assert.equal(显示.调用('尺寸', 旧号)[0], 码.已失效);
  assert.equal(显示.调用('归还', 旧号)[0], 码.已失效);
  const 新号 = 显示.调用('取得', 0, 0, 0, 字('甲'))[1];
  assert.notEqual(新号, 旧号);
  assert.equal(显示.调用('尺寸', 新号)[0], 码.成);
  显示.清理();
});

test('显示面按 data-yy-显示面 名称授予：无此名为未获授权，同一 canvas 取得同一显示面，宿主关闭后为资源暂不可用', () => {
  let 关 = false;
  const {显示, 加画布} = 造显示({关闭: () => 关});
  加画布('甲');
  加画布(null);
  assert.equal(显示.调用('取得', 0, 0, 0, 字('乙'))[0], 码.未获授权);
  assert.equal(显示.调用('取得', 0, 0, 0, 字(''))[0], 码.未获授权);
  const 一 = 显示.调用('取得', 0, 0, 0, 字('甲'));
  const 二 = 显示.调用('取得', 0, 0, 0, 字('甲'));
  assert.deepEqual([一[0], 二[0]], [码.成, 码.成]);
  assert.equal(一[1], 二[1], '同一 canvas 返回同一显示面号');
  关 = true;
  assert.equal(显示.调用('取得', 0, 0, 0, 字('甲'))[0], 码.暂不可用);
  assert.equal(显示.调用('未知', 0, 0, 0, 字(''))[0], 码.宿主失败);
});

test('尺寸按物理像素：CSS 尺寸乘像素比；零尺寸为资源暂不可用；canvas 离开文档后为资源已失效', () => {
  const {显示, 加画布} = 造显示();
  const 画布 = 加画布('甲', {宽: 32, 高: 24});
  const 号 = 显示.调用('取得', 0, 0, 0, 字('甲'))[1];
  assert.deepEqual(显示.调用('尺寸', 号).slice(0, 3), [码.成, 64, 48]);
  画布.矩 = {left: 0, top: 0, width: 0, height: 24};
  assert.equal(显示.调用('尺寸', 号)[0], 码.暂不可用);
  画布.isConnected = false;
  assert.equal(显示.调用('尺寸', 号)[0], 码.已失效);
  assert.equal(显示.调用('尺寸', 999)[0], 码.已失效);
});

test('布局尺寸跟随像素数的 canvas 在首次取得时固定 CSS 尺寸，已有内联尺寸的不动', () => {
  const {显示, 加画布} = 造显示();
  const 随 = 加画布('随');
  随.矩 = {left: 0, top: 0, width: 300, height: 150};
  const 定 = 加画布('定');
  定.style = {width: '64px', height: '48px'};
  显示.调用('取得', 0, 0, 0, 字('随'));
  显示.调用('取得', 0, 0, 0, 字('定'));
  assert.deepEqual([随.style.width, 随.style.height], ['300px', '150px']);
  assert.deepEqual([定.style.width, 定.style.height], ['64px', '48px']);
});

test('提交像素画面：宽高与字节数不合为输入无效，尺寸不同为资源暂不可用，合规则设像素数并 putImageData', () => {
  const {显示, 加画布} = 造显示();
  const 画布 = 加画布('甲', {宽: 2, 高: 1});
  const 号 = 显示.调用('取得', 0, 0, 0, 字('甲'))[1];
  const 帧 = new Uint8Array(4 * 2 * 4).fill(7);
  assert.equal(显示.调用('提交', 号, 4, 2, new Uint8Array(3))[0], 码.输入无效);
  assert.equal(显示.调用('提交', 号, 0, 2, new Uint8Array())[0], 码.输入无效);
  assert.equal(显示.调用('提交', 号, 2 ** 40, 2 ** 40, new Uint8Array(4))[0], 码.输入无效, '乘法溢出');
  assert.equal(显示.调用('提交', 号, 2, 4, 帧)[0], 码.暂不可用, '宽高与显示面不同');
  assert.equal(显示.调用('提交', 号, 4, 2, 帧)[0], 码.成);
  assert.deepEqual([画布.width, 画布.height], [4, 2]);
  assert.equal(画布.二维.画.length, 1);
  const {图, 横, 纵} = 画布.二维.画[0];
  assert.deepEqual([图.width, 图.height, 图.data.length, 图.data[0], 横, 纵], [4, 2, 32, 7, 0, 0]);
});

test('键名译法：0.1.0 的七个语义键、0.2.0 的七个编辑键与四个修饰键、单个标量值原样，其余不交付', () => {
  assert.deepEqual(['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown', 'Enter', 'Backspace', ' '].map(译键), ['左', '右', '上', '下', '回车', '退格', '空格']);
  assert.deepEqual(['Delete', 'Home', 'End', 'PageUp', 'PageDown', 'Tab', 'Escape'].map(译键), ['删除', '起首', '末尾', '上翻页', '下翻页', '制表', '退出']);
  assert.deepEqual(['Shift', 'Control', 'Alt', 'Meta'].map(译键), ['上档', '控制', '交替', '命令']);
  assert.deepEqual(['a', 'Z', '中', '😀'].map(译键), ['a', 'Z', '中', '😀']);
  assert.deepEqual(['F1', 'CapsLock', 'Process', 'Unidentified', ''].map(译键), [null, null, null, null, null]);
  assert.equal(译键('F2'), '功能二');
});

test('输入事件：指针坐标按像素比取整、相邻移动合并、按键与输入法文字、组字中不交付、关闭后先报关闭再为资源已失效', async () => {
  const {显示, 加画布, 文档} = 造显示();
  const 画布 = 加画布('甲');
  const 号 = 显示.调用('取得', 0, 0, 0, 字('甲'))[1];
  const 框 = 文档.元素们.find(元 => 元.getAttribute('data-yy-显示面输入') === '甲');
  assert.ok(框, '宿主在 canvas 后插入隐形输入框');
  const 等 = 显示.调用('等待', 号);
  assert.ok(等 instanceof Promise, '无事件时挂起');
  assert.equal(显示.调用('等待', 号)[0], 码.输入无效, '第二个并发等候者得输入无效');
  画布.dispatchEvent(事件('pointerdown', {clientX: 10 + 5.25, clientY: 20 + 2.5, button: 0, pointerId: 1}));
  assert.deepEqual(await 等, [码.成, 事件种类.指针按下, 10, 5, 0, '', 0]);
  assert.equal(框.聚焦次数, 1, '点击 canvas 聚焦输入框');
  画布.dispatchEvent(事件('pointermove', {clientX: 10 + 1, clientY: 20 + 1}));
  画布.dispatchEvent(事件('pointermove', {clientX: 10 + 3, clientY: 20 + 4}));
  画布.dispatchEvent(事件('pointerup', {clientX: 10 + 3, clientY: 20 + 4, button: 2}));
  框.dispatchEvent(事件('keydown', {key: 'ArrowLeft'}));
  框.dispatchEvent(事件('keydown', {key: 'Shift'}));
  框.dispatchEvent(事件('keydown', {key: 'x', keyCode: 229}));
  框.dispatchEvent(事件('keyup', {key: 'Enter'}));
  框.value = '中';
  框.dispatchEvent(事件('input', {isComposing: false}));
  框.dispatchEvent(事件('compositionstart', {}));
  框.value = '豫';
  框.dispatchEvent(事件('input', {isComposing: true}));
  框.dispatchEvent(事件('keydown', {key: 'a'}));
  框.value = '豫言';
  框.dispatchEvent(事件('compositionend', {data: '豫言'}));
  框.dispatchEvent(事件('compositionstart', {}));
  框.dispatchEvent(事件('compositionend', {data: '言'}));
  const 取 = () => 显示.调用('等待', 号);
  assert.deepEqual(取(), [码.成, 事件种类.指针移动, 6, 8, 0, '', 0], '两次移动合并为最后一次');
  assert.deepEqual(取(), [码.成, 事件种类.指针抬起, 6, 8, 2, '', 0]);
  assert.deepEqual(取(), [码.成, 事件种类.按键按下, 0, 0, 0, '左', 0]);
  assert.deepEqual(取(), [码.成, 事件种类.按键按下, 0, 0, 0, '上档', 0], '0.2.0 交付修饰键');
  assert.deepEqual(取(), [码.成, 事件种类.按键抬起, 0, 0, 0, '回车', 0]);
  assert.deepEqual(取(), [码.成, 事件种类.文字输入, 0, 0, 0, '中', 0]);
  assert.deepEqual(取(), [码.成, 事件种类.文字输入, 0, 0, 0, '豫言', 0], '组字期间的 input 与按键不交付');
  assert.deepEqual(取(), [码.成, 事件种类.文字输入, 0, 0, 0, '言', 0], '输入框为空时取 compositionend 的 data');
  assert.equal(框.value, '');
  框.dispatchEvent(事件('blur', {}));
  assert.deepEqual(取(), [码.成, 事件种类.按键抬起, 0, 0, 0, '上档', 0], '失去焦点时为按着的修饰键补发抬起');
  const 再等 = 取();
  画布.isConnected = false;
  显示.关闭面(显示.取面(号));
  assert.deepEqual(await 再等, [码.成, 事件种类.显示面已关闭, 0, 0, 0, '', 0]);
  assert.equal(取()[0], 码.已失效);
  assert.ok(!文档.元素们.includes(框), '关闭时移除输入框');
});

test('关闭前积压的事件先交付，再交付一次显示面已关闭', () => {
  const {显示, 加画布} = 造显示();
  const 画布 = 加画布('甲');
  const 号 = 显示.调用('取得', 0, 0, 0, 字('甲'))[1];
  画布.dispatchEvent(事件('pointerdown', {clientX: 10, clientY: 20, button: 1}));
  画布.isConnected = false;
  assert.equal(显示.调用('等待', 号)[1], 事件种类.指针按下);
  assert.equal(显示.调用('等待', 号)[1], 事件种类.显示面已关闭);
  assert.equal(显示.调用('等待', 号)[0], 码.已失效);
  assert.equal(显示.调用('提交', 号, 1, 1, new Uint8Array(4))[0], 码.已失效);
});

// ---------------------------------------------------------------------------
// 伪 WebGPU：够测解码、校验与读回即可
// ---------------------------------------------------------------------------
const 每像素 = {'rgba8unorm': 4, 'bgra8unorm': 4, 'rgba16float': 8, 'r32float': 4};
function 伪设备(限额 = {}) {
  const 域 = [];
  const 设备 = {
    limits: {maxBufferSize: 64 * 1024 * 1024, maxStorageBufferBindingSize: 1 << 27, maxTextureDimension2D: 256, maxComputeWorkgroupsPerDimension: 65535,
      maxComputeInvocationsPerWorkgroup: 256, ...限额},
    已毁: false,
    提交数: 0,
    报错(消息, 类 = 'validation') { const 顶 = [...域].reverse().find(项 => 项.类 === 类); if (顶 && !顶.误) 顶.误 = {message: 消息}; },
    pushErrorScope(类) { 域.push({类, 误: null}); },
    popErrorScope() { const 顶 = 域.pop(); return 顶 ? Promise.resolve(顶.误) : Promise.reject(Error('错误域栈空')); },
    createBuffer({size, usage}) {
      const 缓 = {size, usage, 数据: new Uint8Array(size), 已毁: false, destroy() { this.已毁 = true; },
        async mapAsync() { if (this.已毁) throw Error('缓冲已销毁'); }, getMappedRange() { return this.数据.buffer; }, unmap() {}};
      return 缓;
    },
    createTexture({size: [宽, 高], format, usage}) {
      return {width: 宽, height: 高, format, usage, 数据: new Uint8Array(宽 * 高 * 每像素[format]), 已毁: false, destroy() { this.已毁 = true; },
        createView() { return {纹理: this}; }};
    },
    createSampler(描述) { return {描述}; },
    createShaderModule({code}) {
      return {code, async getCompilationInfo() {
        return {messages: code.includes('坏') ? [{type: 'error', lineNum: 1, linePos: 3, message: '语法错误'}] : [{type: 'warning', lineNum: 1, linePos: 1, message: '只是警告'}]};
      }};
    },
    async createComputePipelineAsync({compute}) {
      if (!compute.module.code.includes(compute.entryPoint)) throw Object.assign(Error('入口不存在'), {name: 'GPUPipelineError', reason: 'validation'});
      return {getBindGroupLayout: 序 => ({序})};
    },
    async createRenderPipelineAsync(描述) { return {描述, getBindGroupLayout: 序 => ({序})}; },
    createBindGroup(描述) { return {描述}; },
    createCommandEncoder() {
      const 编 = {令: [], 有效: true};
      const 校 = (条件, 消息) => { if (!条件) { 编.有效 = false; 设备.报错(消息); } };
      编.copyBufferToBuffer = (源, 源偏, 的, 的偏, 长) => {
        校(源偏 % 4 === 0 && 的偏 % 4 === 0 && 长 % 4 === 0, '偏移不是 4 的倍数');
        编.令.push(() => 的.数据.set(源.数据.subarray(源偏, 源偏 + 长), 的偏));
      };
      编.copyTextureToBuffer = ({texture}, {buffer, bytesPerRow}, [宽, 高]) => {
        校(bytesPerRow % 256 === 0, '每行字节数须按 256 对齐');
        const 行 = 宽 * 每像素[texture.format];
        编.令.push(() => { for (let 序 = 0; 序 < 高; 序++) buffer.数据.set(texture.数据.subarray(序 * 行, 序 * 行 + 行), 序 * bytesPerRow); });
      };
      编.copyTextureToTexture = ({texture: 源}, {texture: 的}) => 编.令.push(() => 的.数据.set(源.数据.subarray(0, 的.数据.length)));
      编.beginComputePass = () => ({setPipeline() {}, setBindGroup() {}, dispatchWorkgroups(x, y, z) { if (![x, y, z].every(Number.isSafeInteger)) throw TypeError('工作组数无效'); }, end() {}});
      编.beginRenderPass = ({colorAttachments: [附件]}) => {
        const 纹 = 附件.view.纹理;
        if (附件.loadOp === 'clear') {
          const 色 = 附件.clearValue;
          编.令.push(() => { for (let 位 = 0; 位 < 纹.数据.length; 位 += 4) 纹.数据.set([色.r, 色.g, 色.b, 色.a].map(值 => Math.round(值 * 255)), 位); });
        }
        return {setPipeline() {}, setBindGroup() {}, setVertexBuffer() {}, setIndexBuffer() {}, draw() {}, drawIndexed() {}, end() {}};
      };
      编.finish = () => ({令: 编.令, 有效: 编.有效});
      return 编;
    },
    queue: {
      writeBuffer(缓, 偏移, 数据) { 缓.数据.set(数据, 偏移); },
      writeTexture({texture}, 数据) { texture.数据.set(数据); },
      submit(缓冲们) {
        设备.提交数++;
        for (const 缓 of 缓冲们) if (!缓.有效) { 设备.报错('命令缓冲无效'); return; }
        for (const 缓 of 缓冲们) for (const 令 of 缓.令) 令();
      },
      async onSubmittedWorkDone() {}
    },
    destroy() { 设备.已毁 = true; 设备.兑丢失({reason: 'destroyed', message: ''}); }
  };
  设备.lost = new Promise(兑 => { 设备.兑丢失 = 兑; });
  return 设备;
}
function 伪画布境(画布) {
  return {配置: null, 纹理们: [], configure(配置) { this.配置 = 配置; },
    getCurrentTexture() { const 纹 = this.配置.device.createTexture({size: [画布.width, 画布.height], format: this.配置.format, usage: this.配置.usage}); this.纹理们.push(纹); return 纹; }};
}
const 造图形 = ({有GPU = true, 限额} = {}) => {
  const 设备们 = [];
  const 文档 = 造文档();
  const 全局 = 造全局({
    navigator: 有GPU ? {gpu: {
      getPreferredCanvasFormat: () => 'bgra8unorm',
      requestAdapter: async () => ({limits: {maxBufferSize: 1 << 30}, requestDevice: async () => { const 设备 = 伪设备(限额); 设备们.push(设备); return 设备; }})
    }} : {},
    requestAnimationFrame: 回调 => setTimeout(回调, 0)
  });
  const 显示 = 创建显示能力({根: 文档, 全局});
  const 图形 = 创建图形能力({全局, 显示});
  const 调 = (操作, 参 = {}, 字节) => 图形.调用(操作, JSON.stringify(参), 字节);
  const 加画布 = (名, 选项) => { const 画布 = new 伪画布(文档, 名, 选项); 文档.元素们.push(画布); return 画布; };
  return {图形, 显示, 调, 设备们, 加画布};
};
const 取号 = async 承诺 => { const 果 = await 承诺; assert.equal(果[0], 码.成, 果[2]); return 果[1]; };

// ---------------------------------------------------------------------------
// 二、图形处理器
// ---------------------------------------------------------------------------
test('取得设备：没有 WebGPU 为资源暂不可用；限额六项，末项为单次交换上限；释放后状态为已丢失', async () => {
  assert.equal((await 造图形({有GPU: false}).调('设备.取得'))[0], 码.暂不可用);
  const {调} = 造图形();
  const 设 = await 取号(调('设备.取得'));
  const 限 = JSON.parse(调('设备.限额', {设备: 设})[2]);
  assert.equal(限.length, 6);
  assert.ok(限.every(数 => 数 > 0));
  assert.equal(限[5], 单次交换上限);
  assert.deepEqual(调('设备.状态', {设备: 设}).slice(0, 2), [码.成, 0]);
  assert.equal(调('设备.释放', {设备: 设})[0], 码.成);
  assert.deepEqual(调('设备.状态', {设备: 设}).slice(0, 3), [码.成, 1, '设备已释放']);
  assert.equal((await 调('缓冲.新建', {设备: 设, 大小: 16, 用途: ['存储']}))[0], 码.已失效);
  assert.equal(调('设备.限额', {设备: 设})[0], 码.非法使用);
  assert.equal(调('设备.限额', {设备: 12345})[0], 码.非法使用);
  assert.equal(调('未知操作')[0], 码.宿主失败);
  assert.equal(造图形().图形.调用('设备.限额', '{不是', new Uint8Array())[0], 码.非法使用, '参数不是 JSON');
});

test('缓冲：字节数与用途校验、写入后读回、读回偏移不是 4 的倍数与超过单次上限为图形校验错误、写入越界属非法使用', async () => {
  const {调} = 造图形();
  const 设 = await 取号(调('设备.取得'));
  assert.equal((await 调('缓冲.新建', {设备: 设, 大小: 6, 用途: ['存储']}))[0], 码.校验错误);
  assert.equal((await 调('缓冲.新建', {设备: 设, 大小: 16, 用途: []}))[0], 码.校验错误);
  assert.equal((await 调('缓冲.新建', {设备: 设, 大小: 128 * 1024 * 1024, 用途: ['存储']}))[0], 码.校验错误, '超过设备限额');
  const 缓 = await 取号(调('缓冲.新建', {设备: 设, 大小: 16, 用途: ['存储', '顶点']}));
  assert.equal(调('缓冲.写入', {缓冲: 缓, 偏移: 4}, Uint8Array.of(1, 2, 3, 4))[0], 码.成);
  const 回 = await 调('缓冲.读回', {缓冲: 缓, 偏移: 0, 长度: 16});
  assert.deepEqual([...回[3]], [0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0]);
  assert.equal((await 调('缓冲.读回', {缓冲: 缓, 偏移: 2, 长度: 4}))[0], 码.校验错误);
  assert.equal((await 调('缓冲.读回', {缓冲: 缓, 偏移: 0, 长度: 20}))[0], 码.校验错误);
  assert.deepEqual((await 调('缓冲.读回', {缓冲: 缓, 偏移: 0, 长度: 0}))[3].length, 0);
  const 大 = await 取号(调('缓冲.新建', {设备: 设, 大小: 单次交换上限 + 4, 用途: ['存储']}));
  assert.equal((await 调('缓冲.读回', {缓冲: 大, 偏移: 0, 长度: 单次交换上限 + 4}))[0], 码.校验错误);
  assert.equal(调('缓冲.写入', {缓冲: 缓, 偏移: 14}, Uint8Array.of(1, 2, 3, 4))[0], 码.非法使用);
  assert.equal(调('缓冲.销毁', {缓冲: 缓})[0], 码.成);
  assert.equal(调('缓冲.写入', {缓冲: 缓, 偏移: 0}, Uint8Array.of(1, 2, 3, 4))[0], 码.非法使用, '写入已销毁的缓冲终止运行');
  assert.equal((await 调('缓冲.读回', {缓冲: 缓, 偏移: 0, 长度: 4}))[0], 码.已失效);
});

test('纹理：参数校验、存储用途只许三种格式、读回按 256 字节对齐后去掉填充', async () => {
  const {调} = 造图形();
  const 设 = await 取号(调('设备.取得'));
  assert.equal((await 调('纹理.新建', {设备: 设, 宽: 0, 高: 2, 格式: 'rgba8unorm', 用途: ['渲染']}))[0], 码.校验错误);
  assert.equal((await 调('纹理.新建', {设备: 设, 宽: 300, 高: 2, 格式: 'rgba8unorm', 用途: ['渲染']}))[0], 码.校验错误);
  assert.equal((await 调('纹理.新建', {设备: 设, 宽: 2, 高: 2, 格式: 'bgra8unorm', 用途: ['存储']}))[0], 码.校验错误);
  assert.equal((await 调('纹理.新建', {设备: 设, 宽: 2, 高: 2, 格式: 'xyz', 用途: ['渲染']}))[0], 码.校验错误);
  const 纹 = await 取号(调('纹理.新建', {设备: 设, 宽: 3, 高: 2, 格式: 'rgba8unorm', 用途: ['采样', '存储']}));
  const 像素 = Uint8Array.from({length: 24}, (_, 序) => 序 + 1);
  assert.equal(调('纹理.写入', {纹理: 纹}, 像素)[0], 码.成);
  assert.equal(调('纹理.写入', {纹理: 纹}, 像素.subarray(0, 20))[0], 码.非法使用);
  const 回 = await 调('纹理.读回', {纹理: 纹});
  assert.deepEqual([...回[3]], [...像素]);
});

test('着色模块与管线：编译错误附行列与消息，警告不影响；入口不存在为校验错误', async () => {
  const {调} = 造图形();
  const 设 = await 取号(调('设备.取得'));
  const 坏 = await 调('着色.编译', {设备: 设}, 字('fn 坏('));
  assert.equal(坏[0], 码.校验错误);
  assert.match(坏[2], /第 1 行第 3 列：语法错误/u);
  const 模 = await 取号(调('着色.编译', {设备: 设}, 字('fn 加倍() {}')));
  assert.equal((await 调('计算管线.新建', {设备: 设, 模块: 模, 入口: '不存在'}))[0], 码.校验错误);
  assert.equal((await 调('计算管线.新建', {设备: 设, 模块: 模, 入口: '加倍'}))[0], 码.成);
  const 渲 = await 调('渲染管线.新建', {设备: 设, 模块: 模, 顶点入口: '甲', 片元入口: '乙', 布局: [{步长: 8, 属性: [{位: 0, 格式: 'float32x2', 偏: 0}]}],
    拓扑: 'triangle-strip', 格式: 'rgba8unorm', 混合: '透明'});
  assert.equal(渲[0], 码.成);
  assert.equal((await 调('渲染管线.新建', {设备: 设, 模块: 模, 顶点入口: '甲', 片元入口: '乙', 布局: [], 拓扑: '三角', 格式: 'rgba8unorm', 混合: '不混合'}))[0], 码.校验错误);
});

test('命令：整列录进一个编码器一次提交，含一条坏命令时整列不执行；失效对象为资源已失效；别的设备的对象为校验错误', async () => {
  const {调, 设备们} = 造图形();
  const 设 = await 取号(调('设备.取得'));
  const 甲 = await 取号(调('缓冲.新建', {设备: 设, 大小: 8, 用途: ['存储']}));
  const 乙 = await 取号(调('缓冲.新建', {设备: 设, 大小: 8, 用途: ['存储']}));
  调('缓冲.写入', {缓冲: 甲, 偏移: 0}, Uint8Array.of(1, 2, 3, 4, 5, 6, 7, 8));
  const 复 = (源偏, 长) => ({种: '复制', 源: 甲, 源偏, 的: 乙, 的偏: 0, 长});
  const 坏 = await 调('命令.提交', {设备: 设, 命令: [复(0, 8), 复(2, 4)]});
  assert.equal(坏[0], 码.校验错误);
  assert.deepEqual([...(await 调('缓冲.读回', {缓冲: 乙, 偏移: 0, 长度: 8}))[3]], [0, 0, 0, 0, 0, 0, 0, 0], '第一条合规的复制也没有执行');
  assert.equal((await 调('命令.提交', {设备: 设, 命令: [复(0, 8)]}))[0], 码.成);
  assert.deepEqual([...(await 调('缓冲.读回', {缓冲: 乙, 偏移: 0, 长度: 8}))[3]], [1, 2, 3, 4, 5, 6, 7, 8]);
  const 提交前 = 设备们[0].提交数;
  assert.equal((await 调('命令.提交', {设备: 设, 命令: [{种: '派发', 管线: 甲, 组: [], 数: [1, 1, 1]}]}))[0], 码.校验错误, '种类不符');
  assert.equal((await 调('命令.提交', {设备: 设, 命令: [{种: '未知'}]}))[0], 码.校验错误);
  assert.equal(设备们[0].提交数, 提交前, '解析句柄失败时不提交');
  const 另设 = await 取号(调('设备.取得'));
  const 另缓 = await 取号(调('缓冲.新建', {设备: 另设, 大小: 8, 用途: ['存储']}));
  assert.equal((await 调('命令.提交', {设备: 设, 命令: [{...复(0, 8), 的: 另缓}]}))[0], 码.校验错误);
  调('缓冲.销毁', {缓冲: 甲});
  assert.equal((await 调('命令.提交', {设备: 设, 命令: [复(0, 8)]}))[0], 码.已失效);
});

test('渲染通道清屏与绑定组：清屏色写满目标纹理；绑定资源不属于该设备为校验错误', async () => {
  const {调} = 造图形();
  const 设 = await 取号(调('设备.取得'));
  const 纹 = await 取号(调('纹理.新建', {设备: 设, 宽: 2, 高: 2, 格式: 'rgba8unorm', 用途: ['渲染']}));
  assert.equal((await 调('命令.提交', {设备: 设, 命令: [{种: '通道', 纹理: 纹, 清: [1, 0, 0, 1], 绘: []}]}))[0], 码.成);
  assert.deepEqual([...(await 调('纹理.读回', {纹理: 纹}))[3]], [255, 0, 0, 255, 255, 0, 0, 255, 255, 0, 0, 255, 255, 0, 0, 255]);
  const 模 = await 取号(调('着色.编译', {设备: 设}, 字('fn 主() {}')));
  const 管 = await 取号(调('计算管线.新建', {设备: 设, 模块: 模, 入口: '主'}));
  const 缓 = await 取号(调('缓冲.新建', {设备: 设, 大小: 4, 用途: ['存储']}));
  assert.equal((await 调('绑定组.新建', {设备: 设, 管线: 管, 管线种: '计算', 组: 0, 资源: [{号: 0, 种: '缓冲', 物: 缓}]}))[0], 码.成);
  assert.equal((await 调('绑定组.新建', {设备: 设, 管线: 管, 管线种: '渲染', 组: 0, 资源: []}))[0], 码.非法使用, '管线种类不符');
  const 另设 = await 取号(调('设备.取得'));
  const 另缓 = await 取号(调('缓冲.新建', {设备: 另设, 大小: 4, 用途: ['存储']}));
  assert.equal((await 调('绑定组.新建', {设备: 设, 管线: 管, 管线种: '计算', 组: 0, 资源: [{号: 0, 种: '缓冲', 物: 另缓}]}))[0], 码.校验错误);
});

// ---------------------------------------------------------------------------
// 三、图形显示桥
// ---------------------------------------------------------------------------
test('图形显示：重复取得同一帧纹理；呈现复制到 canvas 当前纹理并作废旧帧；未取即呈现为输入无效；与像素提交互斥；换设备为输入无效', async () => {
  const {调, 显示, 加画布} = 造图形();
  const 画布 = 加画布('画', {宽: 2, 高: 1});
  const 像素画布 = 加画布('像', {宽: 1, 高: 1});
  const 面 = 显示.调用('取得', 0, 0, 0, 字('画'))[1];
  const 像面 = 显示.调用('取得', 0, 0, 0, 字('像'))[1];
  const 设 = await 取号(调('设备.取得'));
  assert.equal(调('呈现.格式', {显示面: 面, 设备: 设})[2], 'bgra8unorm');
  assert.equal((await 调('呈现.呈现', {显示面: 面}))[0], 码.输入无效);
  const 帧一 = await 取号(调('呈现.取纹理', {显示面: 面, 设备: 设}));
  assert.equal(await 取号(调('呈现.取纹理', {显示面: 面, 设备: 设})), 帧一, '呈现前重复取得同一纹理');
  assert.deepEqual([画布.width, 画布.height], [4, 2], 'canvas 像素数设成显示面物理尺寸');
  assert.equal((await 调('纹理.读回', {纹理: 帧一}))[3].length, 4 * 2 * 4, '帧纹理尺寸等于显示面尺寸');
  assert.equal((await 调('命令.提交', {设备: 设, 命令: [{种: '通道', 纹理: 帧一, 清: [0, 0, 1, 1], 绘: []}]}))[0], 码.成);
  assert.equal((await 调('呈现.呈现', {显示面: 面}))[0], 码.成);
  const 当前 = 画布.图形境.纹理们.at(-1);
  assert.deepEqual([...当前.数据.subarray(0, 4)], [0, 0, 255, 255], '帧纹理复制到了 canvas 当前纹理');
  assert.equal((await 调('命令.提交', {设备: 设, 命令: [{种: '通道', 纹理: 帧一, 清: [0, 0, 1, 1], 绘: []}]}))[0], 码.已失效, '呈现后旧帧失效');
  assert.equal((await 调('呈现.呈现', {显示面: 面}))[0], 码.输入无效);
  const 帧二 = await 取号(调('呈现.取纹理', {显示面: 面, 设备: 设}));
  assert.notEqual(帧二, 帧一);
  assert.equal(显示.调用('提交', 面, 4, 2, new Uint8Array(32))[0], 码.暂不可用, 'GPU 呈现过的显示面不能提交像素');
  assert.equal(显示.调用('提交', 像面, 2, 2, new Uint8Array(16))[0], 码.成);
  assert.equal((await 调('呈现.取纹理', {显示面: 像面, 设备: 设}))[0], 码.暂不可用, '提交过像素的显示面不能取纹理');
  assert.equal(像素画布.已取, '2d');
  const 另设 = await 取号(调('设备.取得'));
  assert.equal((await 调('呈现.取纹理', {显示面: 面, 设备: 另设}))[0], 码.输入无效, '显示面已与别的设备绑定');
  画布.isConnected = false;
  assert.equal((await 调('呈现.取纹理', {显示面: 面, 设备: 设}))[0], 码.已失效);
  assert.equal((await 调('命令.提交', {设备: 设, 命令: [{种: '通道', 纹理: 帧二, 清: [0, 0, 1, 1], 绘: []}]}))[0], 码.已失效, '显示面关闭后帧纹理失效');
});

// ---------------------------------------------------------------------------
// 字体：页面声明的字体文件
// ---------------------------------------------------------------------------
test('字体：按 link[data-yy-字体] 取回并缓存；同族名同号；按偏移切片；越界为输入无效；未声明为资源不存在；取回失败为宿主操作失败', async () => {
  const 链接 = (族名, 址, 序号 = null) => ({href: 址, getAttribute: 名 => (名 === 'data-yy-字体' ? 族名 : 名 === 'data-yy-字体序号' ? 序号 : 名 === 'href' ? 址 : null)});
  const 根 = {querySelectorAll: 选 => (选 === 'link[data-yy-字体]' ? [链接('无衬线', 'https://例/甲.ttc', '1'), 链接('衬线', 'https://例/坏.ttf')] : [])};
  let 取次 = 0;
  const 全局 = {fetch: async 址 => {
    取次++;
    if (址.endsWith('坏.ttf')) return {ok: false, status: 404};
    return {ok: true, status: 200, arrayBuffer: async () => Uint8Array.from([0x74, 0x74, 0x63, 0x66, 0, 2, 0, 0, 0, 0, 0, 2, 9]).buffer};
  }};
  const 能 = 创建字体能力({根, 全局, 单次上限: 8});
  const 取 = await 能.调用('取得', '无衬线');
  assert.equal(取[0], 码.成, 取[5]);
  assert.equal(取[2], 13);
  assert.equal(取[3], 1, '字体集序号取 data-yy-字体序号');
  const 再取 = await 能.调用('取得', '无衬线');
  assert.equal(再取[1], 取[1], '同族名同号');
  assert.equal(取次, 1, '只取回一次');
  const 读 = 能.调用('读取', '', 取[1], 0, 4);
  assert.equal(读[0], 码.成);
  assert.deepEqual([...读[4]], [0x74, 0x74, 0x63, 0x66]);
  assert.equal(能.调用('读取', '', 取[1], 0, 0)[4].length, 0);
  assert.equal(能.调用('读取', '', 取[1], 10, 4)[0], 码.输入无效);
  assert.equal(能.调用('读取', '', 取[1], 0, 9)[0], 码.配额已尽, '超过单次上限');
  assert.equal(能.调用('读取', '', 99, 0, 1)[0], 码.已失效);
  assert.equal((await 能.调用('取得', '等宽'))[0], 码.不存在);
  const 坏 = await 能.调用('取得', '衬线');
  assert.equal(坏[0], 码.宿主失败);
  assert.match(坏[5], /404/);
});

test('0.2.0：滚轮按 deltaMode 折成物理像素并合并相邻两次；制表键阻止默认动作；像素比；输入区域移动隐形输入框', () => {
  const {显示, 加画布, 文档} = 造显示();
  const 画布 = 加画布('甲');
  const 号 = 显示.调用('取得', 0, 0, 0, 字('甲'))[1];
  const 框 = 文档.元素们.find(元 => 元.getAttribute('data-yy-显示面输入') === '甲');
  const 滚 = 可取消事件('wheel', {clientX: 10 + 5, clientY: 20 + 5, deltaMode: 0, deltaX: 0, deltaY: 30});
  画布.dispatchEvent(滚);
  assert.equal(滚.defaultPrevented, true, '滚轮阻止页面滚动');
  画布.dispatchEvent(事件('wheel', {clientX: 10 + 6, clientY: 20 + 6, deltaMode: 1, deltaX: 1, deltaY: 2}));
  assert.deepEqual(显示.调用('等待', 号), [码.成, 事件种类.滚轮, 12, 12, 96, '', 60 + 192], '像素式乘像素比二，行式每行 48 逻辑像素；相邻两次合并、位置取新');
  const 制表 = 可取消事件('keydown', {key: 'Tab'});
  框.dispatchEvent(制表);
  assert.equal(制表.defaultPrevented, true, '制表键不让焦点离开');
  assert.deepEqual(显示.调用('等待', 号), [码.成, 事件种类.按键按下, 0, 0, 0, '制表', 0]);
  assert.deepEqual(显示.调用('像素比', 号).slice(0, 3), [码.成, 2000000, 1000000]);
  assert.deepEqual(显示.调用('输入区域', 号, 40, 20, 字(''), 4, 30).slice(0, 1), [码.成]);
  assert.equal(框.style.left, (10 + 20) + 'px');
  assert.equal(框.style.top, (20 + 10) + 'px');
  assert.equal(框.style.height, '15px');
  assert.equal(显示.调用('输入区域', 号, 0, 0, 字(''), -1, 0)[0], 码.输入无效);
  assert.equal(显示.调用('像素比', 999)[0], 码.已失效);
});
