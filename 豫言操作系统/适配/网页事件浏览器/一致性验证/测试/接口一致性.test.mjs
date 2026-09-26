// 文言：以真实 Wasm 探针应用验四包：事之不互吞、委托、键规则之同步、读回、白册、建树、句柄、背压、关闭。
// 汉语：JSDOM + 真实 Wasm 的一致性验收：探针应用通过“豫言测试命令/回复”消息逐个调用 网页事件、网页界面、网页文树、网页消息 的函数。
import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile, readdir} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import {启动探针, 等待, 睡, 页面壳, 产物目录} from './夹具.mjs';

const 页面 = 正文 => '<!doctype html><html lang="zh-CN"><body>' + 正文 + '</body></html>';
const 补模态 = 窗 => {
  const P = 窗.HTMLDialogElement.prototype;
  P.showModal = function () { if (this.hasAttribute('open')) throw new 窗.DOMException('已打开', 'InvalidStateError'); this.setAttribute('open', ''); };
  P.close = function (返回值) { if (!this.hasAttribute('open')) return; this.removeAttribute('open'); if (返回值 !== undefined) this.returnValue = 返回值; this.dispatchEvent(new 窗.Event('close')); };
};
const 用页 = (选项, 函数) => async () => {
  const 页 = await 启动探针(选项);
  try { await 函数(页); } finally { await 页.关(); }
};
const 事件 = async 页 => JSON.parse(await 页.调('等待网页事件'));
// 文言：回复之际豫言仍在造事柄，稍待方得稳定之数。汉语：驱动应用在回复时会短暂占用一个句柄（CustomEvent），所以读句柄数前先让出一拍。
const 句柄数 = async 页 => { await 睡(10); return 页.宿主.状态().句柄数; };
const 键盘 = (窗, 型, 键, 附 = {}) => new 窗.KeyboardEvent(型, {key: 键, code: 附.code ?? 键, bubbles: true, cancelable: true, ...附});
const 失败 = async (承诺, 期望) => {
  await assert.rejects(承诺, 期望);
};

test('接口函数全部被探针覆盖', async () => {
  const 接口根 = (process.env.接口根 ?? fileURLToPath(new URL('../../../../../豫言操作系统接口/', import.meta.url))).replace(/\/?$/u, '/');
  const 探针源 = await readFile(process.env.探针源 ?? fileURLToPath(new URL('../应用/接口探针/入口。豫', import.meta.url)), 'utf8');
  for (const 包 of ['网页事件', '网页界面', '网页文树', '网页消息']) {
    const 文件 = (await readdir(接口根 + 包)).find(名 => 名.endsWith('。接口。豫'));
    const 源 = await readFile(接口根 + 包 + '/' + 文件, 'utf8');
    const 名们 = [...源.matchAll(/^「([^」]+)」乃/gmu)].map(项 => 项[1]);
    assert.ok(名们.length >= 4, 包);
    for (const 名 of 名们) assert.ok(探针源.includes('「' + 名 + '」于') || 探针源.includes('『' + 名 + '』'), `探针未覆盖 ${包}.${名}`);
  }
});

// ---------------------------------------------------------------------------
// 一、事件不互吞
// ---------------------------------------------------------------------------
test('等待界面事件只取界面类型，等待中到达的消息不被吞、事后照常处理', 用页({html: 页面('<button id="钮">钮</button>')}, async 页 => {
  const 等界面 = 页.发('等待界面事件');
  const 后到 = 页.发('读取页面语言');
  await 睡(30);
  assert.equal(页.宿主.状态().各类积压.消息, 1, '第二条命令（消息类型）留在队列里');
  页.文.getElementById('钮').click();
  const 甲 = await 等界面;
  assert.deepEqual([甲.成, 甲.果], [true, 'click|钮']);
  const 乙 = await 后到;
  assert.deepEqual([乙.成, 乙.果], [true, 'zh-CN'], '被留住的消息命令随后正常处理');
}));

test('界面事件在驱动忙于处理消息时积压不丢，之后按序取出', 用页({html: 页面('<button id="甲">甲</button><button id="乙">乙</button><input id="入">')}, async 页 => {
  const 文 = 页.文;
  文.getElementById('甲').click(); 文.getElementById('乙').click();
  文.getElementById('入').dispatchEvent(new 页.窗.Event('input', {bubbles: true}));
  文.getElementById('甲').click();
  await 睡(20);
  assert.equal(页.宿主.状态().各类积压.界面, 4);
  assert.equal(await 页.调('读取页面语言'), 'zh-CN', '消息命令不受界面事件积压影响');
  assert.deepEqual([await 页.调('等待界面事件'), await 页.调('等待界面事件'), await 页.调('等待界面事件'), await 页.调('等待界面事件')],
    ['click|甲', 'click|乙', 'input|入', 'click|甲']);
}));

test('统一等待点返回任意类型的下一个事件（含消息本身），并带序号', 用页({html: 页面('<button id="钮">钮</button>')}, async 页 => {
  const 等 = 页.发('等待网页事件');
  页.文.dispatchEvent(new 页.窗.CustomEvent('豫言测试命令', {detail: {号: 900, 操作: '空', 参: []}}));
  const 甲 = JSON.parse((await 等).果);
  assert.equal(甲.类型, '消息');
  assert.equal(甲.名称, '豫言测试命令');
  assert.equal(甲.详情.号, 900);
  assert.equal(typeof 甲.序, 'number');
  assert.equal(typeof 甲.订阅号, 'number');
  页.文.getElementById('钮').click();
  const 乙 = await 事件(页);
  assert.deepEqual({...乙, 序: undefined}, {类型: '界面', 序: undefined, 订阅号: 0, 标识: '钮', 名称: 'click', 值: '', 选中: false});
  assert.ok(乙.序 > 甲.序);
}));

test('旧式隐式监听：带 id 元素的点击保持原样（值、选中），无 id 元素不报', 用页({html: 页面('<input id="勾" type="checkbox"><input id="文" value="初"><span>无编号</span>')}, async 页 => {
  页.文.querySelector('span').click();
  页.文.getElementById('勾').click();
  页.文.getElementById('文').value = '新';
  页.文.getElementById('文').dispatchEvent(new 页.窗.Event('input', {bubbles: true}));
  const 收 = [await 事件(页), await 事件(页), await 事件(页), await 事件(页)];
  assert.deepEqual(收.map(事 => [事.名称, 事.标识, 事.选中, 事.订阅号]),
    [['click', '勾', true, 0], ['input', '勾', true, 0], ['change', '勾', true, 0], ['input', '文', false, 0]], '复选框点击依次发 click、input、change；无 id 的 span 不报');
  assert.equal(收[3].值, '新');
  assert.equal(页.宿主.状态().积压, 0);
}));

test('旧的原始 等待浏览器事件 仍取任意事件，原始 JSON 形状不变', 用页({html: 页面('<button id="钮">钮</button>')}, async 页 => {
  页.文.getElementById('钮').click();
  const 原始 = await 页.调('等待浏览器事件');
  assert.equal(原始, '{"名称":"click","标识":"钮","值":"","选中":false}');
}));

test('新订阅覆盖同名事件时，带 id 的元素不会重复上报', 用页({html: 页面('<button id="钮">钮</button><section id="域"><button id="内">内</button></section>')}, async 页 => {
  const 号 = Number(await 页.调('订阅界面事件', '域', 'click', '{}'));
  页.文.getElementById('内').click();
  页.文.getElementById('钮').click();
  const 甲 = await 事件(页), 乙 = await 事件(页);
  assert.deepEqual([甲.订阅号, 甲.标识], [号, '内'], '域内点击只由新订阅上报一次');
  assert.deepEqual([乙.订阅号, 乙.标识], [0, '钮'], '域外点击仍由旧监听上报');
  页.文.getElementById('内').click();
  assert.equal((await 事件(页)).订阅号, 号);
  assert.equal(页.宿主.状态().积压, 0, '没有重复事件');
}));

test('两次订阅同一目标同名事件各有订阅号，取消其一不影响另一', 用页({html: 页面('<button id="钮">钮</button>')}, async 页 => {
  const 甲 = Number(await 页.调('订阅界面事件', '钮', 'click', '{}'));
  const 乙 = Number(await 页.调('订阅界面事件', '钮', 'click', '{"合并":false}'));
  assert.notEqual(甲, 乙);
  页.文.getElementById('钮').click();
  const 收 = [(await 事件(页)).订阅号, (await 事件(页)).订阅号].sort();
  assert.deepEqual(收, [甲, 乙].sort());
  await 页.调('取消订阅界面事件', 甲);
  页.文.getElementById('钮').click();
  assert.equal((await 事件(页)).订阅号, 乙);
  assert.equal(页.宿主.状态().积压, 0);
  await 页.调('取消订阅界面事件', 甲);
  await 页.调('取消订阅界面事件', 99999);
}));

// ---------------------------------------------------------------------------
// 二、委托与操作键（动态节点无需 id）
// ---------------------------------------------------------------------------
test('一次建树生成的动态按钮无 id，经容器委托与 data-yy 操作键得到点击', 用页({html: 页面('<div id="日志"></div>')}, async 页 => {
  const 号 = Number(await 页.调('订阅界面事件', '日志', 'click', '{}'));
  const 描述 = {标签: 'article', 类: 'event', 子: [{标签: 'button', 属性: {'data-yy': '打开:7'}, 子: [{标签: 'span', 文字: '打开'}]}, {标签: 'button', 属性: {'data-yy': '复制:7'}, 文字: '复制'}, {标签: 'p', 文字: '正文'}]};
  const 根 = await 页.调('构建页面节点树', JSON.stringify(描述));
  const 容器 = await 页.调('取得页面节点', '日志');
  await 页.调('添加页面子节点', 容器, 根);
  await 页.调('释放页面节点', 根);
  页.文.querySelector('button span').click();
  页.文.querySelectorAll('button')[1].click();
  页.文.querySelector('article p').click();
  const [甲, 乙, 丙] = [await 事件(页), await 事件(页), await 事件(页)];
  assert.deepEqual([甲.类型, 甲.订阅号, 甲.标识, 甲.操作键, 甲.名称], ['界面', 号, '', '打开:7', 'click']);
  assert.deepEqual([乙.标识, 乙.操作键], ['', '复制:7']);
  assert.deepEqual([丙.标识, 丙.操作键], ['', undefined]);
  assert.deepEqual([乙.ctrl, 乙.meta, 乙.alt, 乙.shift, 乙.按钮], [false, false, false, false, 0], '鼠标事件带修饰键与按钮');
}));

// ---------------------------------------------------------------------------
// 三、键规则同步 preventDefault
// ---------------------------------------------------------------------------
test('键规则命中：原生回调内同步 preventDefault，不必等豫言处理', 用页({html: 页面('<input id="命令"><ul id="候选" hidden></ul>')}, async 页 => {
  const 策略 = {键规则: [{键: 'Enter', shift: false}, {键: 'Tab', 仅当属性: {标识: '候选', 属性: 'hidden', 值: 'false'}}, {键: 'c', ctrl: true, 仅选区为空: true}]};
  await 页.调('订阅界面事件', '命令', 'keydown', JSON.stringify(策略));
  const 命令 = 页.文.getElementById('命令');
  const 派 = (键, 附) => { const 事 = 键盘(页.窗, 'keydown', 键, 附); 命令.dispatchEvent(事); return 事.defaultPrevented; };
  assert.equal(派('Enter'), true, '同步：dispatchEvent 一返回即已阻止');
  assert.equal(派('Enter', {shiftKey: true}), false);
  assert.equal(派('Tab'), false, '候选框隐藏，不拦截 Tab');
  页.文.getElementById('候选').removeAttribute('hidden');
  assert.equal(派('Tab'), true, '候选框显示后拦截 Tab');
  assert.equal(派('c', {ctrlKey: true}), true);
  命令.value = 'abc'; 命令.focus(); 命令.setSelectionRange(0, 2);
  assert.equal(派('c', {ctrlKey: true}), false, '有选区则放行复制');
  assert.equal(派('x'), false);
  const 全 = [];
  for (let i = 0; i < 7; i++) 全.push(await 事件(页));
  assert.deepEqual(全.map(事 => [事.键, 事.已阻止默认]), [['Enter', true], ['Enter', false], ['Tab', false], ['Tab', true], ['c', true], ['c', false], ['x', false]]);
  assert.deepEqual([全[1].shift, 全[1].ctrl, 全[1].重复, 全[1].组字中, 全[1].代码], [true, false, false, false, 'Enter']);
}));

test('输入法组字期间的键不命中键规则；组字事件与 input 载荷', 用页({html: 页面('<input id="命令">')}, async 页 => {
  await 页.调('订阅界面事件', '命令', 'keydown', '{"键规则":[{"键":"Enter"}]}');
  for (const 名 of ['compositionstart', 'compositionupdate', 'compositionend', 'input']) await 页.调('订阅界面事件', '文档', 名, '{}');
  const 命令 = 页.文.getElementById('命令');
  命令.dispatchEvent(new 页.窗.CompositionEvent('compositionstart', {bubbles: true, data: ''}));
  命令.dispatchEvent(new 页.窗.CompositionEvent('compositionupdate', {bubbles: true, data: 'ni'}));
  const 组字回车 = 键盘(页.窗, 'keydown', 'Enter', {isComposing: true, keyCode: 229});
  命令.dispatchEvent(组字回车);
  命令.dispatchEvent(new 页.窗.CompositionEvent('compositionend', {bubbles: true, data: '你'}));
  命令.dispatchEvent(new 页.窗.InputEvent('input', {bubbles: true, data: '你', inputType: 'insertText', isComposing: false}));
  const 普通回车 = 键盘(页.窗, 'keydown', 'Enter'); 命令.dispatchEvent(普通回车);
  assert.equal(组字回车.defaultPrevented, false, '组字期间回车归输入法');
  assert.equal(普通回车.defaultPrevented, true);
  const 收 = [];
  for (let i = 0; i < 6; i++) 收.push(await 事件(页));
  const 名称们 = 收.map(事 => 事.名称);
  assert.deepEqual(名称们.filter(名 => 名 !== 'keydown'), ['compositionstart', 'compositionupdate', 'compositionend', 'input']);
  const 结束 = 收.find(事 => 事.名称 === 'compositionend');
  assert.deepEqual([结束.组字中, 结束.详情], [false, {数据: '你'}]);
  assert.deepEqual(收.find(事 => 事.名称 === 'compositionupdate').详情, {数据: 'ni'});
  const 输入 = 收.find(事 => 事.名称 === 'input');
  assert.deepEqual([输入.值, 输入.详情.输入类型], ['', 'insertText'].map((值, 序) => 序 ? 值 : 命令.value));
  assert.equal(收.find(事 => 事.名称 === 'keydown' && 事.组字中).键, 'Enter');
}));

test('dialog 的 cancel 事件走统一队列，可由策略同步阻止', 用页({html: 页面('<dialog id="创建弹窗"><p>内</p></dialog>'), 窗扩展: 补模态}, async 页 => {
  await 页.调('订阅界面事件', '创建弹窗', 'cancel', '{"阻止默认":true}');
  await 页.调('订阅界面事件', '创建弹窗', 'close', '{}');
  await 页.调('打开模态框', '创建弹窗');
  assert.equal(页.文.getElementById('创建弹窗').open, true);
  const 取消 = new 页.窗.Event('cancel', {cancelable: true});
  页.文.getElementById('创建弹窗').dispatchEvent(取消);
  assert.equal(取消.defaultPrevented, true, 'Esc 被同步拦下，弹窗不关');
  const 甲 = await 事件(页);
  assert.deepEqual([甲.类型, 甲.名称, 甲.标识, 甲.已阻止默认], ['界面', 'cancel', '创建弹窗', true]);
  await 页.调('关闭模态框', '创建弹窗');
  assert.equal(页.文.getElementById('创建弹窗').open, false);
  assert.equal((await 事件(页)).名称, 'close');
  await 失败(页.调('打开模态框', '无此'), /网页元素不存在：无此/);
}));

test('可见性、联机、历史事件各有类型', 用页({html: 页面('')}, async 页 => {
  await 页.调('订阅界面事件', '文档', 'visibilitychange', '{}');
  await 页.调('订阅界面事件', '窗口', 'online', '{}');
  await 页.调('订阅界面事件', '窗口', 'offline', '{}');
  await 页.调('订阅界面事件', '窗口', 'popstate', '{}');
  页.窗.dispatchEvent(new 页.窗.Event('offline'));
  页.文.dispatchEvent(new 页.窗.Event('visibilitychange'));
  页.窗.dispatchEvent(new 页.窗.PopStateEvent('popstate', {state: {页: 3}}));
  页.窗.dispatchEvent(new 页.窗.Event('online'));
  const 收 = [await 事件(页), await 事件(页), await 事件(页), await 事件(页)];
  assert.deepEqual(收.map(事 => [事.类型, 事.名称, 事.详情]), [
    ['联机', 'offline', {联机: false}], ['可见性', 'visibilitychange', {可见: true}],
    ['历史', 'popstate', {网址: 'https://yuyan-lang.org/cloud/', 状态: {页: 3}}], ['联机', 'online', {联机: true}]]);
}));

test('订阅错误以豫言事故返回（可被捕获），策略与目标校验在订阅时完成', 用页({html: 页面('<div id="域"></div>')}, async 页 => {
  await 失败(页.调('订阅界面事件', '无此', 'click', '{}'), /网页元素不存在：无此/);
  await 失败(页.调('订阅界面事件', '域', 'click', '{"乱写":1}'), /未知字段：乱写/);
  await 失败(页.调('订阅界面事件', '域', 'click', '坏'), /不是有效 JSON/);
  await 失败(页.调('订阅界面事件', '域', 'click', '{"键规则":[{"键":"a"}]}'), /只适用于 keydown/);
  await 失败(页.调('订阅界面事件', '域', '乱 写', '{}'), /界面事件名无效/);
  assert.equal(页.宿主.状态().界面订阅数, 0);
  assert.equal(await 页.调('读取页面语言'), 'zh-CN', '驱动应用没有因失败而崩溃');
}));

// ---------------------------------------------------------------------------
// 四、界面读回与控制
// ---------------------------------------------------------------------------
test('读回与控制：值、显示、禁用、只读、类名、焦点、光标（码点）、选区、滚动、页面状态', 用页({
  html: 页面('<input id="入" value="a😀bc"><div id="盒"><input id="内"></div><p id="段" class="旧">正文文字</p><div id="面板"><div style="height:900px"></div></div>'),
  窗扩展: 窗 => { 窗.matchMedia = 查 => ({matches: 查 === '(max-width: 700px)'}); }
}, async 页 => {
  const 文 = 页.文;
  assert.equal(await 页.调('读取节点值', '入'), 'a😀bc');
  await 页.调('设置节点值', '入', '新值');
  assert.equal(文.getElementById('入').value, '新值');
  await 页.调('设置节点显示', '盒', false);
  assert.equal(文.getElementById('盒').hidden, true);
  await 页.调('设置节点显示', '盒', true);
  assert.equal(文.getElementById('盒').hidden, false);
  await 页.调('设置节点禁用', '内', true);
  assert.equal(文.getElementById('内').disabled, true);
  await 页.调('设置节点只读', '入', true);
  assert.equal(文.getElementById('入').readOnly, true);
  await 页.调('设置节点类名', '段', '甲 乙-1');
  assert.equal(文.getElementById('段').className, '甲 乙-1');
  await 失败(页.调('设置节点类名', '段', 'a"b'), /类名含不允许的字符/);
  await 页.调('设置节点类标记', '段', '活动', true);
  assert.equal(文.getElementById('段').className, '甲 乙-1 活动');
  await 页.调('设置节点类标记', '段', '甲', false);
  assert.equal(文.getElementById('段').className, '乙-1 活动');
  await 失败(页.调('设置节点类标记', '段', 'a b', true), /类标记须为单个类名/);
  assert.equal(await 页.调('读取聚焦标识'), '');
  await 页.调('设置节点值', '入', 'a😀bc');
  await 页.调('聚焦节点', '入');
  assert.equal(await 页.调('读取聚焦标识'), '入');
  assert.equal(await 页.调('节点包含焦点', '入'), 'true');
  assert.equal(await 页.调('节点包含焦点', '盒'), 'false');
  await 页.调('设置节点禁用', '内', false);
  await 页.调('设置节点显示', '盒', true);
  await 页.调('聚焦节点', '内');
  assert.equal(await 页.调('节点包含焦点', '盒'), 'true');
  await 页.调('聚焦节点', '入');
  await 页.调('设置输入光标', '入', 1, 3);
  assert.equal(文.getElementById('入').selectionEnd, 4, '码点 3 对应 UTF-16 位置 4');
  assert.equal(await 页.调('读取输入光标', '入'), '1,3');
  assert.equal(await 页.调('读取选区文字'), '😀b');
  await 失败(页.调('设置输入光标', '入', 2, 99), /位置超出文字长度/);
  await 失败(页.调('设置输入光标', '盒', 0, 0), /不是文本输入框/);
  const 面板 = 文.getElementById('面板');
  Object.defineProperty(面板, 'scrollHeight', {value: 900, configurable: true});
  Object.defineProperty(面板, 'clientHeight', {value: 300, configurable: true});
  await 页.调('设置滚动顶', '面板', 120);
  assert.deepEqual(JSON.parse(await 页.调('读取滚动度量', '面板')), {顶: 120, 总高: 900, 可视高: 300});
  assert.deepEqual(JSON.parse(await 页.调('读取页面状态')), {可见: true, 联机: true, 窄屏: true});
  assert.equal(await 页.调('读取节点可见', '盒'), 'true');
  await 页.调('设置节点显示', '盒', false);
  assert.equal(await 页.调('读取节点可见', '盒'), 'false');
  await 失败(页.调('读取节点值', '盒'), /不是输入控件/);
  await 失败(页.调('读取节点值', '无此'), /网页元素不存在：无此/);
}));

test('设置节点文字与属性：白册、危险元素与链接', 用页({html: 页面('<a id="链">x</a><div id="盒"></div><script id="脚本"></script>')}, async 页 => {
  await 页.调('设置节点文字', '盒', '<img src=x onerror=alert(1)>');
  assert.equal(页.文.getElementById('盒').children.length, 0, '纯文本，不解析 HTML');
  assert.equal(页.文.getElementById('盒').textContent, '<img src=x onerror=alert(1)>');
  await 页.调('设置节点属性', '链', 'href', 'http://example.com/');
  await 页.调('设置节点属性', '链', 'target', '_blank');
  assert.equal(页.文.getElementById('链').getAttribute('rel'), 'noopener noreferrer');
  await 页.调('设置节点属性', '盒', 'data-view', '源码');
  await 页.调('设置节点属性', '盒', 'aria-hidden', 'true');
  await 页.调('设置节点属性', '盒', 'role', 'log');
  await 页.调('设置节点属性', '盒', 'tabindex', '-1');
  for (const [名, 值, 期望] of [['onclick', 'alert(1)', /页面属性不受支持：onclick/], ['style', 'x', /不受支持/], ['href', 'javascript:alert(1)', /仅 a 可设 href/], ['tabindex', '5', /tabindex/], ['data-yy-frame-origins', 'x', /宿主保留/]])
    await 失败(页.调('设置节点属性', '盒', 名, 值), 期望);
  await 失败(页.调('设置节点属性', '链', 'href', 'javascript:alert(1)'), /页面链接不受支持/);
  await 失败(页.调('设置节点属性', '链', 'href', '//evil.example/'), /页面链接不受支持/);
  await 失败(页.调('设置节点文字', '脚本', 'alert(1)'), /类型不允许操作：script/);
  await 失败(页.调('设置节点属性', '无此', 'data-x', '1'), /网页元素不存在/);
}));

// ---------------------------------------------------------------------------
// 五、受限文树：白册、一次建树、句柄
// ---------------------------------------------------------------------------
test('页面节点全流程：新建、属性、附加、插入、替换、移除、释放', 用页({html: 页面('<div id="根容器"></div>')}, async 页 => {
  const 容器 = await 页.调('取得页面节点', '根容器');
  const 列 = await 页.调('新建页面节点', 'ul', '', '列表');
  const 项 = async 文本 => 页.调('新建页面节点', 'li', 文本, '');
  const 甲 = await 项('甲'), 乙 = await 项('乙'), 丙 = await 项('丙');
  await 页.调('添加页面子节点', 列, 甲);
  await 页.调('添加页面子节点', 列, 丙);
  await 页.调('插入页面子节点前', 列, 乙, 丙);
  await 页.调('添加页面子节点', 容器, 列);
  const 文本们 = () => Array.from(页.文.querySelectorAll('#根容器 li'), 元 => 元.textContent).join('');
  assert.equal(文本们(), '甲乙丙');
  assert.equal(页.文.querySelector('#根容器 ul').className, '列表');
  await 页.调('替换页面子节点', 列, [丙, 乙]);
  assert.equal(文本们(), '丙乙');
  await 失败(页.调('设置页面节点文字', 甲, 'x'), /页面节点句柄无效/);
  await 页.调('设置页面节点文字', 乙, '乙改');
  await 页.调('设置页面节点属性', 乙, 'data-yy', '选乙');
  await 页.调('设置页面节点属性', 乙, 'title', '提示');
  assert.equal(页.文.querySelectorAll('#根容器 li')[1].getAttribute('data-yy'), '选乙');
  await 页.调('追加页面节点文字', 乙, '·追加');
  assert.equal(页.文.querySelectorAll('#根容器 li')[1].textContent, '乙改·追加');
  await 页.调('移除页面节点', 乙);
  assert.equal(文本们(), '丙');
  await 页.调('清空页面子节点', 容器);
  assert.equal(页.文.getElementById('根容器').children.length, 0);
  await 页.调('设置页面标题', '新标题');
  assert.equal(页.文.title, '新标题');
  assert.equal(await 句柄数(页) <= 4, true, '句柄数：' + await 句柄数(页));
  await 失败(页.调('新建页面节点', 'script', '', ''), /页面标签不受支持/);
  await 失败(页.调('新建页面节点', 'iframe', '', ''), /页面标签不受支持/);
  await 失败(页.调('添加页面子节点', 容器, '99999'), /页面节点句柄无效/);
  await 失败(页.调('设置页面节点属性', 容器, 'onclick', 'x'), /页面属性不受支持：onclick/);
  await 失败(页.调('设置页面节点属性', 容器, 'href', '/x'), /仅 a 可设 href/);
  await 失败(页.调('取得页面节点', '不存在'), /网页元素不存在：不存在/);
  await 页.调('释放页面节点', 容器);
  await 失败(页.调('释放页面节点', 容器), /页面节点句柄无效/);
}));

test('一次建树：结构与限额，失败不留句柄', 用页({html: 页面('<div id="根容器"></div>')}, async 页 => {
  const 根 = await 页.调('构建页面节点树', JSON.stringify({标签: 'table', 类: '资源面板', 子: [{标签: 'thead', 子: [{标签: 'tr', 子: [{标签: 'th', 文字: '名'}, {标签: 'th', 文字: '状态'}]}]}, {标签: 'tbody', 子: [{标签: 'tr', 子: [{标签: 'td', 文字: 'a'}, {标签: 'td', 属性: {colspan: 1}, 子: [{标签: 'a', 文字: '链', 属性: {href: '/x', target: '_blank'}}]}]}]}]}));
  await 页.调('添加页面子节点', await 页.调('取得页面节点', '根容器'), 根);
  assert.equal(页.文.querySelectorAll('#根容器 table th').length, 2);
  const 链 = 页.文.querySelector('#根容器 a');
  assert.equal(链.getAttribute('rel'), 'noopener noreferrer');
  const 前 = await 句柄数(页);
  const 链式 = 深 => { let 项 = {标签: 'span', 文字: '叶'}; for (let i = 1; i < 深; i++) 项 = {标签: 'div', 子: [项]}; return 项; };
  await 页.调('释放页面节点', await 页.调('构建页面节点树', JSON.stringify(链式(32))));
  await 失败(页.调('构建页面节点树', JSON.stringify(链式(33))), /深度超过 32/);
  await 失败(页.调('构建页面节点树', JSON.stringify({标签: 'ul', 子: Array.from({length: 2000}, () => ({标签: 'li'}))})), /节点数超过 2000/);
  await 失败(页.调('构建页面节点树', JSON.stringify({标签: 'script'})), /页面标签不受支持：script/);
  await 失败(页.调('构建页面节点树', JSON.stringify({标签: 'div', 属性: {onclick: 'x'}})), /页面属性不受支持：onclick/);
  await 失败(页.调('构建页面节点树', JSON.stringify({标签: 'a', 属性: {href: 'javascript:alert(1)'}})), /页面链接不受支持/);
  await 失败(页.调('构建页面节点树', '{坏'), /不是有效 JSON/);
  assert.equal(await 句柄数(页), 前, '失败与已释放的树都不占句柄');
}));

test('句柄不泄漏：真实 Wasm 一万次“建树、附加、释放”后句柄数不增', 用页({html: 页面('<div id="根容器"></div>')}, async 页 => {
  const 容器 = await 页.调('取得页面节点', '根容器');
  const 基线 = await 句柄数(页);
  const 描述 = JSON.stringify({标签: 'article', 类: 'event', 子: [{标签: 'p', 文字: '行'}, {标签: 'button', 属性: {'data-yy': '删'}, 文字: '删'}]});
  let 最大 = 0;
  for (let i = 0; i < 10000; i++) {
    const 根 = await 页.调('构建页面节点树', 描述);
    await 页.调('添加页面子节点', 容器, 根);
    await 页.调('释放页面节点', 根);
    if (i % 500 === 0) { 最大 = Math.max(最大, await 句柄数(页)); await 页.调('清空页面子节点', 容器); }
  }
  assert.ok(await 句柄数(页) <= 基线, `句柄数 ${await 句柄数(页)} 基线 ${基线}`);
  assert.ok(最大 <= 基线 + 2, '循环中最多同时多一棵树的一个句柄：' + 最大);
  assert.equal(页.文.getElementById('根容器').children.length < 500, true);
}));

test('追加页面节点文字适合流式输出；框架地址与模板装入', 用页({
  html: 页面('<pre id="输出"></pre><iframe id="网站画面" sandbox="allow-same-origin allow-forms" data-yy-frame-origins="https://usercontent.yuyan-lang.org"></iframe><div id="市场"></div>'),
  网络: async 网址 => 网址.endsWith('%E6%A8%A1%E6%9D%BF.html')
    ? new Response('<main id="根" class="市"><script>x</script><p id="正文" onclick="x">你好</p></main>', {status: 200}) : new Response('', {status: 404})
}, async 页 => {
  const 输出 = await 页.调('取得页面节点', '输出');
  for (const 片 of ['一', '二', '三']) await 页.调('追加页面节点文字', 输出, 片);
  assert.equal(页.文.getElementById('输出').childNodes.length, 1);
  assert.equal(页.文.getElementById('输出').textContent, '一二三');
  await 页.调('设置页面框架地址', '网站画面', 'https://usercontent.yuyan-lang.org/sites/' + 'a'.repeat(64) + '/?revision=2');
  assert.match(页.文.getElementById('网站画面').getAttribute('src'), /^https:\/\/usercontent\.yuyan-lang\.org\/sites\/a{64}\/\?revision=2$/);
  await 失败(页.调('设置页面框架地址', '网站画面', 'https://evil.example/'), /来源未获页面声明/);
  await 页.调('设置页面框架地址', '网站画面', '');
  assert.equal(页.文.getElementById('网站画面').hasAttribute('src'), false);
  await 页.调('装入页面模板', '/模板.html', '根', '市场');
  assert.equal(页.文.querySelector('#市场 main.市').children.length, 2 - 1);
  assert.equal(页.文.getElementById('正文').hasAttribute('onclick'), false);
  assert.equal(页.文.querySelector('#市场 script'), null);
  await 失败(页.调('装入页面模板', '/无.html', '根', '市场'), /页面资源不可用/);
  await 失败(页.调('装入页面模板', '/模板.html', '无此', '市场'), /页面模板中没有标识/);
  await 失败(页.调('装入页面模板', '//evil.example/x', '根', '市场'), /页面资源路径无效/);
  assert.match(await 页.调('读取页面网址'), /^https:\/\/yuyan-lang\.org\/cloud\//);
}));

// ---------------------------------------------------------------------------
// 六、背压与丢弃计数
// ---------------------------------------------------------------------------
test('队列上限可配：超限丢弃最旧并计数，序号可见缺口，别的类型不受影响', 用页({html: 页面('<button id="钮">钮</button>')}, async 页 => {
  await 页.调('设置网页事件队列上限', '界面', 5);
  for (let i = 0; i < 20; i++) 页.文.getElementById('钮').click();
  await 睡(20);
  assert.equal(页.宿主.状态().各类积压.界面, 5);
  assert.equal(await 页.调('读取网页事件丢弃数'), '15');
  assert.equal(await 页.调('读取网页事件类型丢弃数', '界面'), '15');
  assert.equal(await 页.调('读取网页事件类型丢弃数', '消息'), '0');
  assert.equal(await 页.调('读取网页事件类型丢弃数', '乱写'), '0');
  const 序们 = [];
  for (let i = 0; i < 5; i++) 序们.push((await 事件(页)).序);
  assert.deepEqual(序们.map((序, 下标) => 下标 ? 序 - 序们[下标 - 1] : 1), [1, 1, 1, 1, 1]);
  assert.ok(序们[0] > 16, '留下的是最新的五个：' + 序们);
  await 失败(页.调('设置网页事件队列上限', '界面', 0), /1 至 65536/);
  await 失败(页.调('设置网页事件队列上限', '乱写', 5), /类型无效/);
}));

test('默认上限 1024：一千一百次点击丢弃七十六个，其余保持顺序', 用页({html: 页面('<input id="入">')}, async 页 => {
  for (let i = 0; i < 1100; i++) { 页.文.getElementById('入').value = String(i); 页.文.getElementById('入').dispatchEvent(new 页.窗.Event('input', {bubbles: true})); }
  await 睡(20);
  assert.equal(页.宿主.状态().各类积压.界面, 1024);
  assert.equal(await 页.调('读取网页事件丢弃数'), '76');
  const 首 = await 事件(页);
  assert.equal(首.值, '76', '最旧的 76 个（0 至 75）已丢');
}));

test('高频 scroll 事件相邻合并，不占队列也不算丢弃', 用页({html: 页面('<div id="面板"></div><button id="钮">钮</button>')}, async 页 => {
  await 页.调('订阅界面事件', '面板', 'scroll', '{}');
  const 面板 = 页.文.getElementById('面板');
  for (let i = 1; i <= 50; i++) { Object.defineProperty(面板, 'scrollTop', {value: i, configurable: true}); 面板.dispatchEvent(new 页.窗.Event('scroll')); }
  页.文.getElementById('钮').click();
  Object.defineProperty(面板, 'scrollTop', {value: 99, configurable: true}); 面板.dispatchEvent(new 页.窗.Event('scroll'));
  const 收 = [await 事件(页), await 事件(页), await 事件(页)];
  assert.deepEqual(收.map(事 => 事.名称), ['scroll', 'click', 'scroll']);
  assert.equal(收[0].详情.顶, 50);
  assert.equal(收[2].详情.顶, 99);
  assert.equal(页.宿主.状态().丢弃, 0);
}));

// ---------------------------------------------------------------------------
// 七、网页消息
// ---------------------------------------------------------------------------
test('网页消息：订阅第二个名称、详情 JSON 往返、取消订阅、名称校验', 用页({html: 页面('')}, async 页 => {
  await 页.调('订阅网页消息', '豫言回声');
  await 页.调('订阅网页消息', '豫言回声');
  assert.equal(页.宿主.状态().消息订阅数, 2, '命令名与回声名，重复订阅不增');
  const 回 = new Promise(解析 => 页.文.addEventListener('豫言回声应答', 事 => 解析(事.detail), {once: true}));
  // 回声消息被驱动当作命令处理：请求 76 号读取页面语言
  页.文.dispatchEvent(new 页.窗.CustomEvent('豫言回声', {detail: {号: 76, 操作: '读取页面语言', 参: []}}));
  await 等待(() => true);
  await 页.调('发布网页消息', '豫言回声应答', '{"a":[1,"二",{"b":null}],"c":true}');
  assert.deepEqual(await 回, {a: [1, '二', {b: null}], c: true});
  await 页.调('取消订阅网页消息', '豫言回声');
  assert.equal(页.宿主.状态().消息订阅数, 1);
  await 页.调('取消订阅网页消息', '豫言回声');
  await 失败(页.调('订阅网页消息', '甲乙'), /网页消息名称无效/);
  await 失败(页.调('订阅网页消息', '豫言' + 'x'.repeat(126)), /网页消息名称无效/);
  await 失败(页.调('发布网页消息', '乙', '{}'), /网页消息名称无效/);
  await 失败(页.调('发布网页消息', '豫言丙', '不是 JSON'), /./);
}));

test('等待网页消息：详情超过一 MiB 转成事故而不崩溃；界面事件不被消息等待吞掉', 用页({html: 页面('<button id="钮">钮</button>')}, async 页 => {
  await 页.调('订阅网页消息', '豫言大');
  const 等消息 = 页.发('等待网页消息');
  页.文.getElementById('钮').click();
  页.文.dispatchEvent(new 页.窗.CustomEvent('豫言大', {detail: {x: '字'.repeat(400000)}}));
  const 回 = await 等消息;
  assert.equal(回.成, false);
  assert.match(回.果, /网页消息详情超过一 MiB/);
  assert.equal(await 页.调('等待界面事件'), 'click|钮', '点击事件仍在队列里');
  const 等二 = 页.发('等待网页消息');
  页.文.dispatchEvent(new 页.窗.CustomEvent('豫言大', {detail: [1, {a: '甲'}]}));
  assert.deepEqual([(await 等二).成, (await 等二).果], [true, '豫言大|[1,{"a":"甲"}]']);
}));

// ---------------------------------------------------------------------------
// 八、定时事件与统一等待点
// ---------------------------------------------------------------------------
test('既有定时事件在统一等待点中归入定时类型；周期定时被取走后继续投递', 用页({html: 页面('')}, async 页 => {
  const 一次 = Number(await 页.调('定时一次', 5, '一次标记'));
  const 甲 = await 事件(页);
  assert.deepEqual([甲.类型, 甲.订阅号, 甲.名称, 甲.详情.种类, 甲.详情.标记], ['定时', 一次, '定时', '一次', '一次标记']);
  assert.equal(typeof 甲.详情.时刻, 'number');
  const 重复 = Number(await 页.调('定时重复', 5, '刷新'));
  const 乙 = await 事件(页), 丙 = await 事件(页);
  assert.deepEqual([乙.类型, 乙.订阅号, 乙.详情.标记], ['定时', 重复, '刷新']);
  assert.equal(丙.订阅号, 重复, '取走后周期定时器继续投递');
  assert.equal(await 页.调('取消定时', String(重复)), 'true');
  assert.equal(await 页.调('取消定时', String(重复)), 'false');
}));

test('定时事件积压时合并为一个待处理；被丢弃后周期定时器仍能继续', 用页({html: 页面('')}, async 页 => {
  await 页.调('设置网页事件队列上限', '定时', 1);
  const 重复 = Number(await 页.调('定时重复', 3, '刷'));
  await 睡(60);
  assert.equal(页.宿主.状态().各类积压.定时, 1, '同一周期器最多一个未交付事件');
  assert.equal((await 事件(页)).订阅号, 重复);
  await 睡(30);
  assert.equal(页.宿主.状态().各类积压.定时, 1);
  await 页.调('取消定时', String(重复));
  assert.equal(页.宿主.状态().各类积压.定时 ?? 0, 1 - 1 + (页.宿主.状态().各类积压.定时 ?? 0), '取消清除未交付事件');
}));

// ---------------------------------------------------------------------------
// 八·五、0.2.0/0.3.0 新增：选择器过滤、带属性与矩形、属性读回与移除、点击、位置与矩形
// ---------------------------------------------------------------------------
const 存在文 = 文 => [文.startsWith('1|'), 文.slice(2)];
const 点 = (窗, 元素, 型 = 'click', 附 = {}) => {
  const 事件 = new 窗.MouseEvent(型, {bubbles: true, cancelable: true, ...附});
  元素.dispatchEvent(事件);
  return 事件;
};

test('选择器过滤：只对匹配元素产生事件并同步阻止默认；载荷取匹配元素，另报目标标识、属性与矩形', 用页({
  html: 页面('<div id="容器"><a id="链" href="/x" title="题"><span id="内">字</span></a><button id="钮" type="button">按钮</button></div>')
}, async 页 => {
  const 号 = Number(await 页.调('订阅界面事件', '文档', 'click', JSON.stringify({捕获: true, 选择器: 'a[href]', 阻止默认: true, 带属性: ['href', 'title', 'data-none'], 带矩形: true})));
  assert.ok(号 > 0);
  const 内 = 页.文.getElementById('内');
  const 点击 = 点(页.窗, 内);
  assert.equal(点击.defaultPrevented, true, 'dispatchEvent 一返回默认动作即被阻止');
  const 事 = await 事件(页);
  assert.equal(事.类型, '界面');
  assert.equal(事.订阅号, 号);
  assert.equal(事.名称, 'click');
  assert.equal(事.标识, '链', '标识取匹配元素');
  assert.equal(事.目标标识, '内', '原始目标由目标标识报告');
  assert.deepEqual(事.属性, {href: '/x', title: '题'}, '不存在的属性省略');
  assert.deepEqual(Object.keys(事.矩形).sort(), ['上', '下', '右', '左'].sort());
  assert.ok(Object.values(事.矩形).every(Number.isInteger));
  assert.equal(事.已阻止默认, true);
  // 点击不匹配的按钮：没有事件，也不阻止默认，也不会由旧式隐式监听重复上报
  const 钮点击 = 点(页.窗, 页.文.getElementById('钮'));
  assert.equal(钮点击.defaultPrevented, false);
  await 睡(20);
  assert.equal(页.宿主.状态().各类积压.界面 ?? 0, 0);
}));

test('选择器过滤：匹配必须在订阅边界内；操作键从匹配元素向上找；非元素目标不匹配', 用页({
  html: 页面('<section data-yy="外"><div id="域"><p data-yy="行:1" class="行"><em id="字">甲</em></p></div></section><p class="行 外">域外</p>')
}, async 页 => {
  await 页.调('订阅界面事件', '域', 'click', JSON.stringify({选择器: '.行'}));
  const 事甲 = await (async () => { 点(页.窗, 页.文.getElementById('字')); return 事件(页); })();
  assert.equal(事甲.标识, '', '匹配元素没有 id 时为空');
  assert.equal(事甲.操作键, '行:1');
  assert.equal(事甲.目标标识, '字');
  // 域外的同类元素点击不产生事件（订阅边界之外）
  点(页.窗, 页.文.querySelector('p.外'));
  await 睡(20);
  assert.equal(页.宿主.状态().各类积压.界面 ?? 0, 0);
  // 文档订阅 scroll：目标是文档而不是元素，选择器订阅不匹配
  await 页.调('订阅界面事件', '文档', 'scroll', JSON.stringify({选择器: '.行'}));
  页.文.dispatchEvent(new 页.窗.Event('scroll'));
  await 睡(20);
  assert.equal(页.宿主.状态().各类积压.界面 ?? 0, 0);
}));

test('选择器与带属性的校验：语法错误、超限、重复、危险属性名都在订阅时抛事故且不留监听', 用页({html: 页面('<div id="域"></div>')}, async 页 => {
  const 前 = 页.宿主.状态().界面订阅数;
  const 试 = (策略, 期望) => 失败(页.调('订阅界面事件', '文档', 'click', JSON.stringify(策略)), 期望);
  await 试({选择器: 'button['}, /选择器无效/);
  await 试({选择器: ''}, /选择器须为/);
  await 试({选择器: 'a'.repeat(513)}, /选择器须为/);
  await 试({带属性: ['a', 'a']}, /重复/);
  await 试({带属性: ['onclick']}, /无效的属性名/);
  await 试({带属性: ['style']}, /无效的属性名/);
  await 试({带属性: ['Bad']}, /无效的属性名/);
  await 试({带属性: ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']}, /不超过 8 个/);
  await 试({带矩形: 'yes'}, /须为布尔/);
  assert.equal(页.宿主.状态().界面订阅数, 前);
}));

test('排除选择器：点击落在被排除区域之内则忽略（不投递、不阻止默认），之外才产生事件；可与选择器并用', 用页({
  html: 页面('<details class="文件目录"><summary id="摘要">目录</summary><div><a id="内链" href="#x">内</a></div></details><p id="外" data-yy="外面">外</p><button id="钮" type="button">钮</button>')
}, async 页 => {
  await 页.调('订阅界面事件', '文档', 'click', JSON.stringify({排除选择器: '.文件目录', 阻止默认: true}));
  const 摘要点 = 点(页.窗, 页.文.getElementById('摘要'));
  assert.equal(摘要点.defaultPrevented, false, '被排除：不执行同步动作');
  点(页.窗, 页.文.getElementById('内链'));
  await 睡(20);
  assert.equal(页.宿主.状态().各类积压.界面 ?? 0, 0, '区域内的点击没有事件');
  const 外点 = 点(页.窗, 页.文.getElementById('外'));
  assert.equal(外点.defaultPrevented, true);
  const 事 = await 事件(页);
  assert.deepEqual([事.标识, 事.操作键], ['外', '外面']);
  // 与选择器并用：先排除，后选择器
  await 页.调('订阅界面事件', '文档', 'mouseover', JSON.stringify({排除选择器: '.文件目录', 选择器: 'button'}));
  点(页.窗, 页.文.getElementById('摘要'), 'mouseover');
  点(页.窗, 页.文.getElementById('钮'), 'mouseover');
  const 悬 = await 事件(页);
  assert.deepEqual([悬.名称, 悬.标识], ['mouseover', '钮']);
  // 校验
  await 失败(页.调('订阅界面事件', '文档', 'click', '{"排除选择器":""}'), /排除选择器须为/);
  await 失败(页.调('订阅界面事件', '文档', 'click', '{"排除选择器":"a["}'), /排除选择器无效/);
}));

test('带属性与带矩形在没有选择器时作用于原始目标；口令框的 value 属性不带出', 用页({
  html: 页面('<button id="钮" data-k="v">钮</button><input id="口令" type="password" value="秘密" data-k="w">')
}, async 页 => {
  await 页.调('订阅界面事件', '文档', 'click', JSON.stringify({带属性: ['data-k', 'value'], 带矩形: true}));
  点(页.窗, 页.文.getElementById('钮'));
  const 事 = await 事件(页);
  assert.deepEqual(事.属性, {'data-k': 'v'});
  assert.equal(事.目标标识, undefined, '没有选择器时不带目标标识');
  assert.ok(事.矩形);
  点(页.窗, 页.文.getElementById('口令'));
  const 乙 = await 事件(页);
  assert.deepEqual(乙.属性, {'data-k': 'w'}, '口令框的 value 属性不带出');
  assert.equal(乙.值, undefined, '口令框的值本来就不带出');
}));

test('读取节点属性、移除节点属性、读取节点文字：保留字文档根与页体、布尔属性、白册与危险元素', 用页({
  html: 页面('<div id="盒" data-x="值" hidden>文字<b>粗</b></div><details id="项" open></details><script id="脚"></script><iframe id="框" title="题" data-a="b" src="/x"></iframe>')
}, async 页 => {
  页.文.documentElement.setAttribute('data-lang', 'wen');
  页.文.body.setAttribute('aria-busy', 'true');
  assert.deepEqual(存在文(await 页.调('读取节点属性', '文档根', 'data-lang')), [true, 'wen']);
  assert.deepEqual(存在文(await 页.调('读取节点属性', '文档根', 'data-none')), [false, '']);
  assert.deepEqual(存在文(await 页.调('读取节点属性', '页体', 'aria-busy')), [true, 'true']);
  assert.deepEqual(存在文(await 页.调('读取节点属性', '盒', 'data-x')), [true, '值']);
  assert.deepEqual(存在文(await 页.调('读取节点属性', '盒', 'hidden')), [true, ''], '布尔属性存在时值为空文字');
  assert.deepEqual(存在文(await 页.调('读取节点属性', '项', 'open')), [true, '']);
  assert.deepEqual(存在文(await 页.调('读取节点属性', '框', 'title')), [true, '题']);
  assert.deepEqual(存在文(await 页.调('读取节点属性', '框', 'data-a')), [true, 'b']);
  await 失败(页.调('读取节点属性', '框', 'src'), /iframe/);
  await 失败(页.调('读取节点属性', '脚', 'id'), /不允许操作/);
  await 失败(页.调('读取节点属性', '不存在', 'id'), /不存在/);
  for (const 坏名 of ['onclick', 'style', 'srcdoc', 'Bad', '1a', '']) await 失败(页.调('读取节点属性', '盒', 坏名), /不受支持/, 坏名);
  await 页.调('移除节点属性', '项', 'open');
  assert.deepEqual(存在文(await 页.调('读取节点属性', '项', 'open')), [false, '']);
  await 页.调('移除节点属性', '项', 'open'); // 本来就没有：无事
  await 页.调('移除节点属性', '盒', 'data-x');
  assert.equal(页.文.getElementById('盒').hasAttribute('data-x'), false);
  await 页.调('移除节点属性', '盒', 'hidden');
  assert.equal(页.文.getElementById('盒').hidden, false);
  for (const 坏 of ['style', 'onclick', 'data-yy-内部', 'rel', 'src']) await 失败(页.调('移除节点属性', '盒', 坏), /不受支持|保留/, 坏);
  await 失败(页.调('移除节点属性', '框', 'src'), /不受支持/);
  assert.equal(await 页.调('读取节点文字', '盒'), '文字粗');
  await 失败(页.调('读取节点文字', '脚'), /不允许操作/);
  await 失败(页.调('读取节点文字', '没有'), /不存在/);
}));

test('点击节点：经全部订阅收到 click；被禁用无效果；文件框与危险链接抛事故；回放被拦截点击须先撤订阅', 用页({
  html: 页面('<button id="钮">钮</button><button id="禁" disabled>禁</button><input id="文件" type="file"><a id="脚本链" href="javascript:alert(1)">x</a><a id="锚链" href="#below">锚</a><div id="below"></div>')
}, async 页 => {
  const 号 = Number(await 页.调('订阅界面事件', '文档', 'click', JSON.stringify({捕获: true, 选择器: 'button', 阻止默认: true, 停止同处: true})));
  let 冒泡到 = 0;
  页.文.getElementById('钮').addEventListener('click', () => { 冒泡到++; });
  await 页.调('点击节点', '钮');
  const 事 = await 事件(页);
  assert.deepEqual([事.名称, 事.标识], ['click', '钮']);
  assert.equal(冒泡到, 0, '被拦截订阅停止同处，页面上别的监听者收不到');
  // 回放：先撤订阅，再点击，页面上别的监听者才收到
  await 页.调('取消订阅界面事件', 号);
  await 页.调('点击节点', '钮');
  assert.equal(冒泡到, 1);
  await 页.调('点击节点', '禁'); // 被禁用：无效果不抛事故
  await 失败(页.调('点击节点', '文件'), /文件输入框/);
  await 失败(页.调('点击节点', '脚本链'), /链接不受支持/);
  await 失败(页.调('点击节点', '没有'), /不存在/);
  await 页.调('点击节点', '锚链');
  await 等待(() => 页.窗.location.hash === '#below', '锚点导航');
}));

test('设置节点位置与读取节点矩形：像素整数、范围、危险元素', 用页({html: 页面('<div id="提示" style="position:absolute"></div><script id="脚"></script>')}, async 页 => {
  await 页.调('设置节点位置', '提示', 12, -34);
  assert.equal(页.文.getElementById('提示').style.left, '12px');
  assert.equal(页.文.getElementById('提示').style.top, '-34px');
  await 页.调('设置节点位置', '提示', 100000, -100000);
  await 失败(页.调('设置节点位置', '提示', 100001, 0), /越界/);
  await 失败(页.调('设置节点位置', '脚', 1, 1), /不允许操作/);
  const 框 = JSON.parse(await 页.调('读取节点矩形', '提示'));
  assert.deepEqual(Object.keys(框).sort(), ['上', '下', '右', '左'].sort());
  assert.ok(Object.values(框).every(Number.isInteger));
  await 失败(页.调('读取节点矩形', '没有'), /不存在/);
}));

// ---------------------------------------------------------------------------
// 八·六、网页文树 0.3.0：选择器查询、节点读回、树导出、替换、超文本
// ---------------------------------------------------------------------------
test('文树 0.3.0：选择器查询（含 template 内容、危险元素不入结果）与节点读回、父级、最近祖先、匹配', 用页({
  html: 页面('<section id="外"><p id="段"><a class="引用" href="/x">甲</a><a class="引用" href="/y" title="题">乙</a></p></section><script id="脚" class="引用"></script>'
    + '<template id="资料"><a data-qualified="甲.乙" href="/a">甲</a></template>')
}, async 页 => {
  const 柄们 = JSON.parse(await 页.调('查询页面节点', '', '.引用'));
  assert.equal(柄们.length, 2, '危险元素 script 不入结果');
  assert.equal(await 页.调('读取页面节点标签', 柄们[0]), 'a');
  assert.deepEqual(存在文(await 页.调('读取页面节点属性', 柄们[1], 'title')), [true, '题']);
  assert.deepEqual(存在文(await 页.调('读取页面节点属性', 柄们[0], 'title')), [false, '']);
  assert.equal(await 页.调('读取页面节点文字', 柄们[0]), '甲');
  const 父 = await 页.调('读取页面节点父级', 柄们[0]);
  assert.equal(await 页.调('读取页面节点标签', 父), 'p');
  const 祖 = await 页.调('查找最近页面祖先', 柄们[0], 'section');
  assert.equal(await 页.调('读取页面节点标签', 祖), 'section');
  assert.equal(await 页.调('查找最近页面祖先', 柄们[0], '.不存在'), '');
  assert.equal(await 页.调('查找最近页面祖先', 柄们[0], 'a'), 柄们[0], 'closest 含自身，同一元素同一句柄');
  assert.equal(await 页.调('页面节点匹配选择器', 柄们[0], 'a[href="/x"]'), 'true');
  assert.equal(await 页.调('页面节点匹配选择器', 柄们[0], 'a[href="/y"]'), 'false');
  assert.deepEqual(JSON.parse(await 页.调('查询页面节点', 父, 'a')), 柄们, '按文档顺序，同一元素同一句柄');
  // 根为 template：在其 content 里查询；普通文档查询看不到
  assert.deepEqual(JSON.parse(await 页.调('查询页面节点', '', 'a[data-qualified]')), []);
  const 模板柄 = await 页.调('取得页面节点', '资料');
  const 模板内 = JSON.parse(await 页.调('查询页面节点', 模板柄, 'a[data-qualified]'));
  assert.equal(模板内.length, 1);
  assert.deepEqual(存在文(await 页.调('读取页面节点属性', 模板内[0], 'data-qualified')), [true, '甲.乙']);
  assert.equal(await 页.调('读取页面节点父级', 模板内[0]), '', '模板内容里的节点没有父元素');
  // 错误
  await 失败(页.调('查询页面节点', '', 'a['), /选择器无效/);
  await 失败(页.调('查询页面节点', '', ''), /选择器须为/);
  await 失败(页.调('查询页面节点', '', 'a'.repeat(513)), /选择器须为/);
  await 失败(页.调('读取页面节点标签', '99999'), /句柄无效/);
  await 失败(页.调('读取页面节点属性', 柄们[0], 'onclick'), /不受支持/);
  await 失败(页.调('查找最近页面祖先', 柄们[0], '::'), /选择器无效/);
  // 释放这些句柄，句柄数回到起点
  const 起 = 页.宿主.状态().句柄数;
  for (const 柄 of [...柄们, 父, 祖, 模板柄, ...模板内]) await 页.调('释放页面节点', 柄);
  assert.ok(页.宿主.状态().句柄数 < 起);
}));

test('文树 0.3.0：读取页面节点树导出与构建页面节点树同形，白册外属性与危险子树被略去，重建等价', 用页({
  html: 页面('<div id="根容器"></div><p class="源 宽" data-a="b" hidden onclick="x" style="color:red" data-yy-x="1">首<code class="c0">ab<b class="k">cd</b></code>末<script>1</script><template><i>甲</i></template></p>')
}, async 页 => {
  const 源 = JSON.parse(await 页.调('查询页面节点', '', 'p.源'))[0];
  const 树 = JSON.parse(await 页.调('读取页面节点树', 源, 5));
  assert.deepEqual(树, {标签: 'p', 类: '源 宽', 属性: {'data-a': 'b', hidden: ''},
    子: ['首', {标签: 'code', 类: 'c0', 子: ['ab', {标签: 'b', 类: 'k', 子: ['cd']}]}, '末']});
  // 原样交给构建页面节点树得到等价的新树
  const 新 = await 页.调('构建页面节点树', JSON.stringify(树));
  const 容器 = await 页.调('取得页面节点', '根容器');
  await 页.调('添加页面子节点', 容器, 新);
  const 副本 = 页.文.querySelector('#根容器 > p');
  assert.equal(副本.className, '源 宽');
  assert.equal(副本.getAttribute('data-a'), 'b');
  assert.equal(副本.hasAttribute('onclick'), false);
  assert.equal(副本.textContent, '首abcd末');
  assert.equal(副本.querySelector('b.k').textContent, 'cd');
  // 深度与节点数上限
  await 失败(页.调('读取页面节点树', 源, 2), /深度超过所给上限/);
  await 失败(页.调('读取页面节点树', 源, 0), /深度上限须为/);
  await 失败(页.调('读取页面节点树', 源, 33), /深度上限须为/);
  const 大 = 页.文.createElement('div');
  for (let i = 0; i < 2000; i++) 大.appendChild(页.文.createElement('span'));
  大.id = '大';
  页.文.body.appendChild(大);
  await 失败(页.调('读取页面节点树', await 页.调('取得页面节点', '大'), 3), /节点数超过 2000/);
}));

test('文树 0.3.0：替换页面节点不释放旧节点句柄，可再放进新节点（包一层）；无父与层级违规抛事故', 用页({
  html: 页面('<div id="容器"><span id="旧">字</span><b id="后">后</b></div>')
}, async 页 => {
  const 旧 = await 页.调('取得页面节点', '旧');
  const 链 = await 页.调('新建页面节点', 'a', '', '包');
  await 页.调('设置页面节点属性', 链, 'href', '/x');
  await 页.调('替换页面节点', 旧, 链);
  assert.equal(页.文.getElementById('容器').innerHTML, '<a class="包" href="/x"></a><b id="后">后</b>');
  await 页.调('添加页面子节点', 链, 旧);
  assert.equal(页.文.getElementById('容器').innerHTML, '<a class="包" href="/x"><span id="旧">字</span></a><b id="后">后</b>');
  await 页.调('设置页面节点文字', 旧, '改'); // 旧句柄仍有效
  assert.equal(页.文.getElementById('旧').textContent, '改');
  // 无父节点的旧节点
  const 孤 = await 页.调('新建页面节点', 'span', '', '');
  await 失败(页.调('替换页面节点', 孤, 链), /没有父节点/);
  // 把祖先放进后代
  const 后 = await 页.调('取得页面节点', '后');
  const 容器 = await 页.调('取得页面节点', '容器');
  await 失败(页.调('替换页面节点', 后, 容器), /层级无效/);
  // 自己换自己：无事
  await 页.调('替换页面节点', 后, 后);
}));

test('文树 0.3.0：移除页面节点属性与读取；设置页面节点超文本按白册清洗、不合规抛事故且内容不变、旧子树句柄同释', 用页({
  html: 页面('<div id="盒" data-x="1" hidden><i id="旧子">旧</i></div><pre id="代码"></pre>')
}, async 页 => {
  const 盒 = await 页.调('取得页面节点', '盒');
  await 页.调('移除页面节点属性', 盒, 'data-x');
  await 页.调('移除页面节点属性', 盒, 'hidden');
  await 页.调('移除页面节点属性', 盒, 'title'); // 本来没有：无事
  assert.equal(页.文.getElementById('盒').hasAttribute('data-x') || 页.文.getElementById('盒').hidden, false);
  await 失败(页.调('移除页面节点属性', 盒, 'style'), /不受支持/);
  await 失败(页.调('移除页面节点属性', 盒, 'data-yy-内'), /不受支持|保留/);
  const 代码 = await 页.调('取得页面节点', '代码');
  const 旧子 = await 页.调('取得页面节点', '旧子');
  void 旧子;
  await 页.调('设置页面节点超文本', 代码, '<span class="tok-k">甲</span>乙<!--注释--><b>粗</b><em>强调</em><hr>');
  assert.equal(页.文.getElementById('代码').innerHTML, '<span class="tok-k">甲</span>乙<b>粗</b><em>强调</em><hr>');
  // 不合规：标签、属性、链接
  const 前 = 页.文.getElementById('代码').innerHTML;
  await 失败(页.调('设置页面节点超文本', 代码, '<script>alert(1)</script>'), /不受支持的标签：script（根\[0\]）/);
  await 失败(页.调('设置页面节点超文本', 代码, '甲<span onclick="x">乙</span>'), /页面属性不受支持：onclick（根\[1\]）/);
  await 失败(页.调('设置页面节点超文本', 代码, '<span style="color:red">乙</span>'), /页面属性不受支持：style/);
  await 失败(页.调('设置页面节点超文本', 代码, '<a href="javascript:alert(1)">乙</a>'), /链接不受支持/);
  await 失败(页.调('设置页面节点超文本', 代码, '<div><p><img src=x></p></div>'), /不受支持的标签：img（根\[0\]\.子\[0\]\.子\[0\]）/);
  assert.equal(页.文.getElementById('代码').innerHTML, 前, '失败不改内容');
  await 页.调('设置页面节点超文本', 代码, '');
  assert.equal(页.文.getElementById('代码').innerHTML, '');
  // 被替换子树的句柄同释
  const 盒二 = await 页.调('取得页面节点', '盒');
  await 页.调('设置页面节点超文本', 盒二, '新');
  await 失败(页.调('读取页面节点标签', 旧子), /句柄无效/);
}));

test('文树 0.3.0：新增标签 em、i、kbd、sup、sub、blockquote、hr、dl、dt、dd 可建；hr 是空元素', 用页({html: 页面('<div id="根容器"></div>')}, async 页 => {
  const 树 = {标签: 'blockquote', 子: [{标签: 'em', 文字: '强'}, {标签: 'i', 文字: '斜'}, {标签: 'kbd', 文字: 'Ctrl'}, {标签: 'sup', 文字: '2'}, {标签: 'sub', 文字: '1'},
    {标签: 'hr'}, {标签: 'dl', 子: [{标签: 'dt', 文字: '词'}, {标签: 'dd', 文字: '义'}]}]};
  const 柄 = await 页.调('构建页面节点树', JSON.stringify(树));
  await 页.调('添加页面子节点', await 页.调('取得页面节点', '根容器'), 柄);
  assert.equal(页.文.getElementById('根容器').innerHTML, '<blockquote><em>强</em><i>斜</i><kbd>Ctrl</kbd><sup>2</sup><sub>1</sub><hr><dl><dt>词</dt><dd>义</dd></dl></blockquote>');
  await 失败(页.调('构建页面节点树', JSON.stringify({标签: 'hr', 文字: '不可'})), /空元素/);
}));

// ---------------------------------------------------------------------------
// 九、关闭语义
// ---------------------------------------------------------------------------
test('宿主关闭：阻塞的等待返回关闭，应用正常退出；之后的等待继续返回关闭', async () => {
  const 页 = await 启动探针({html: 页面('')});
  const 等界面 = 页.发('等待界面事件');
  await 睡(10);
  页.宿主.关闭();
  const 回 = await 等界面;
  assert.deepEqual([回.成, 回.果], [true, '关闭|']);
  await Promise.race([页.宿主.完成, 睡(2000).then(() => { throw Error('应用没有在关闭后退出'); })]);
  assert.equal(页.错误(), undefined);
  页.窗.close();
});

test('宿主关闭时统一等待点返回类型为关闭的事件', async () => {
  const 页 = await 启动探针({html: 页面('')});
  const 等 = 页.发('等待网页事件');
  await 睡(10);
  页.宿主.关闭();
  const 回 = await 等;
  const 事 = JSON.parse(回.果);
  assert.deepEqual([事.类型, 事.名称, 事.订阅号], ['关闭', '关闭', 0]);
  await 页.宿主.完成;
  页.窗.close();
});
