// 文言：验宿主三工厂：事件列、界面订阅器、页面控制；不涉 Wasm，只以 JSDOM 为页。
// 汉语：宿主单元测试：直接调用 创建事件队列、创建界面订阅器、创建页面控制，覆盖背压、按类型取事件、委托、同步键规则、白册与句柄回收。
import test from 'node:test';
import assert from 'node:assert/strict';
import {JSDOM路径, 宿主模块, 产物目录} from './夹具.mjs';
import {pathToFileURL} from 'node:url';

const {JSDOM} = await import(pathToFileURL(JSDOM路径));
const {创建事件队列, 创建界面订阅器, 创建页面控制, 分类网页事件, 统一事件文, 统一事件体, 原始事件文, 界面事件元组, 消息事件元组,
  解析界面策略, 网页事件类型} = 宿主模块;
const {创建句柄表} = await import(pathToFileURL(产物目录 + '句柄.mjs'));

const 造页 = (正文 = '', {url = 'https://yuyan-lang.org/cloud/'} = {}) => {
  const 窗 = new JSDOM('<!doctype html><html lang="zh-CN"><body>' + 正文 + '</body></html>', {url, pretendToBeVisual: true}).window;
  return {窗, 文: 窗.document};
};
const 造项 = (类型, 名称, 额外 = {}) => ({类型, 事件: {名称, ...额外}});
const 取 = (队, 类) => 队.等待(x => x === 类, 项 => 项.事件.名称, () => '关闭', true);

// ---------------------------------------------------------------------------
// 一、统一事件队列
// ---------------------------------------------------------------------------
test('按类型取事件不吞其他类型，同类保持先进先出', () => {
  const 队 = 创建事件队列();
  队.投递(造项('界面', 'click'));
  队.投递(造项('消息', '豫言甲'));
  队.投递(造项('界面', 'input'));
  队.投递(造项('定时', '定时'));
  assert.equal(取(队, '消息'), '豫言甲');
  assert.equal(取(队, '界面'), 'click');
  assert.equal(取(队, '界面'), 'input');
  assert.equal(取(队, '定时'), '定时');
  assert.equal(队.状态().积压, 0);
});

test('任意类型的等待按全局到达顺序取，且各类型互不吞', () => {
  const 队 = 创建事件队列();
  for (const [类, 名] of [['消息', 'a'], ['界面', 'b'], ['定时', 'c'], ['界面', 'd']]) 队.投递(造项(类, 名));
  const 任 = () => 队.等待(() => true, 项 => 项.事件.名称, () => '关闭', true);
  assert.deepEqual([任(), 任(), 任(), 任()], ['a', 'b', 'c', 'd']);
});

test('等待者按类型接收：不接受的类型入队，接受的直接交付', async () => {
  const 队 = 创建事件队列();
  const 等消息 = 队.等待(x => x === '消息', 项 => 项.事件.名称, () => '关闭', true);
  assert.ok(等消息 instanceof Promise);
  队.投递(造项('界面', 'click'));
  assert.equal(队.状态().积压, 1, '界面事件留在队列里');
  队.投递(造项('消息', '豫言乙'));
  assert.equal(await 等消息, '豫言乙');
  assert.equal(队.状态().积压, 1);
  assert.equal(取(队, '界面'), 'click');
});

test('多个等待者各取所需，先到先得', async () => {
  const 队 = 创建事件队列();
  const 甲 = 队.等待(x => x === '界面', 项 => 'A:' + 项.事件.名称, () => '关', true);
  const 乙 = 队.等待(() => true, 项 => 'B:' + 项.事件.名称, () => '关', true);
  队.投递(造项('消息', 'm1'));
  队.投递(造项('界面', 'c1'));
  assert.equal(await 乙, 'B:m1');
  assert.equal(await 甲, 'A:c1');
});

test('每类型上限：超限丢弃该类型最旧事件并计数，序号出现缺口，别的类型不受影响', () => {
  const 队 = 创建事件队列({上限: {界面: 3}});
  队.投递(造项('消息', 'm'));
  for (let i = 1; i <= 5; i++) 队.投递(造项('界面', 'e' + i));
  assert.equal(队.丢弃数(), 2);
  assert.equal(队.丢弃数('界面'), 2);
  assert.equal(队.丢弃数('消息'), 0);
  assert.equal(队.状态().各类积压.界面, 3);
  const 序们 = [];
  for (let i = 0; i < 3; i++) 序们.push(队.等待(x => x === '界面', 项 => 项.事件.名称 + '#' + 项.序, () => '关', true));
  assert.deepEqual(序们, ['e3#4', 'e4#5', 'e5#6'], '留下最新三个，序号 4 5 6 表明 e1 e2 已丢');
  assert.equal(取(队, '消息'), 'm');
});

test('默认上限为 1024，可按类型配置；配置越界或类型无效则报错', () => {
  const 队 = 创建事件队列();
  assert.equal(队.取上限('界面'), 1024);
  队.设上限('消息', 7);
  assert.equal(队.取上限('消息'), 7);
  assert.throws(() => 队.设上限('界面', 0), /1 至 65536/);
  assert.throws(() => 队.设上限('界面', 65537), /1 至 65536/);
  assert.throws(() => 队.设上限('界面', 1.5), /1 至 65536/);
  assert.throws(() => 队.设上限('乱写', 5), /类型无效/);
  assert.throws(() => 创建事件队列({上限: {乱写: 5}}), /类型无效/);
  assert.throws(() => 创建事件队列({上限: []}), /须为对象/);
});

test('上限调低后，下一次投递把该类型压到新上限', () => {
  const 队 = 创建事件队列();
  for (let i = 0; i < 10; i++) 队.投递(造项('界面', 'e' + i));
  队.设上限('界面', 4);
  队.投递(造项('界面', 'e10'));
  assert.equal(队.状态().各类积压.界面, 4);
  assert.equal(队.丢弃数(), 7);
});

test('宿主类满则拒新事件报错，不丢弃已有事件（保持旧行为）', () => {
  const 队 = 创建事件队列({上限: {宿主: 2}});
  队.投递(造项('宿主', 'x1'));
  队.投递(造项('宿主', 'x2'));
  assert.throws(() => 队.投递(造项('宿主', 'x3')), /浏览器事件队列已满/);
  assert.equal(队.丢弃数(), 0);
  assert.equal(取(队, '宿主'), 'x1');
});

test('合并键：尾项同类型同键则就地替换；夹了别的事件则不合并', () => {
  const 队 = 创建事件队列();
  const 滚 = (顶) => ({类型: '界面', 事件: {名称: 'scroll', 顶}, 合并键: '1:面板'});
  队.投递(滚(1)); 队.投递(滚(2)); 队.投递(滚(3));
  assert.equal(队.状态().积压, 1);
  队.投递(造项('界面', 'click'));
  队.投递(滚(4));
  assert.equal(队.状态().积压, 3);
  const 读 = () => 队.等待(x => x === '界面', 项 => 项.事件.名称 + (项.事件.顶 ?? ''), () => '关', true);
  assert.deepEqual([读(), 读(), 读()], ['scroll3', 'click', 'scroll4']);
  assert.equal(队.丢弃数(), 0, '合并不算丢弃');
});

test('离队钩子：直递、取走、丢弃、删除四种离队都会调用', async () => {
  const 见 = [];
  const 队 = 创建事件队列({上限: {界面: 1}, 离队钩子: 项 => 见.push(项.事件.名称)});
  const 等 = 队.等待(x => x === '消息', () => 1, () => 0, true);
  队.投递(造项('消息', '直递'));
  await 等;
  队.投递(造项('界面', '将被丢'));
  队.投递(造项('界面', '将被取'));
  取(队, '界面');
  队.投递(造项('定时', '将被删'));
  队.删除若(项 => 项.事件.名称 === '将被删');
  assert.deepEqual(见, ['直递', '将被丢', '将被取', '将被删']);
});

test('关闭：等待者收到各自的关闭值；粘滞等待之后仍得关闭标记，非粘滞则永不返回', async () => {
  const 队 = 创建事件队列();
  const 甲 = 队.等待(x => x === '界面', () => '事', () => '关闭甲', true);
  const 乙 = 队.等待(() => true, () => '事', () => '关闭乙', true);
  队.关闭();
  assert.equal(await 甲, '关闭甲');
  assert.equal(await 乙, '关闭乙');
  assert.equal(await 队.等待(() => true, () => '事', () => '再关闭', true), '再关闭');
  assert.equal(队.投递(造项('界面', 'x')), false, '关闭后不再收事件');
  const 非粘 = 队.等待(() => true, () => '事', () => '不该返回', false);
  const 竞 = await Promise.race([非粘, new Promise(完成 => setTimeout(() => 完成('超时'), 30))]);
  assert.equal(竞, '超时');
});

test('关闭后，已入队的事件仍可先取走', async () => {
  const 队 = 创建事件队列();
  队.投递(造项('界面', '遗留'));
  队.关闭();
  assert.equal(取(队, '界面'), '遗留');
  assert.equal(await 取(队, '界面'), '关闭');
});

test('旧式原始事件的分类与统一形状', () => {
  assert.equal(分类网页事件({名称: 'click', 标识: 'a', 值: '', 选中: false}), '界面');
  assert.equal(分类网页事件({名称: 'click', 来源句柄: '3', 标识: 'a'}), '宿主', '通用桥事件保持宿主类');
  assert.equal(分类网页事件({名称: '定时', 定时号: '1'}), '定时');
  assert.equal(分类网页事件({名称: '关闭'}), '关闭');
  assert.equal(分类网页事件({名称: '动画帧'}), '宿主');
  assert.equal(分类网页事件({类型: '事件流', 订阅号: 1, 名称: 'message'}), '事件流', '新式事件自带类型');
  assert.equal(分类网页事件({类型: '乱写', 名称: 'x'}), '宿主');
  assert.deepEqual(网页事件类型.slice(), ['界面', '消息', '定时', '事件流', '请求', '编译', '可见性', '联机', '历史', '关闭', '宿主']);
  const 体 = 项 => JSON.parse(统一事件文({序: 5, ...项}));
  assert.deepEqual(体({类型: '界面', 事件: {名称: 'click', 标识: 'b', 值: 'v', 选中: true}, 附加: {操作键: '删'}}),
    {类型: '界面', 序: 5, 订阅号: 0, 标识: 'b', 名称: 'click', 操作键: '删', 值: 'v', 选中: true});
  assert.deepEqual(体({类型: '定时', 事件: {名称: '定时', 定时号: '7', 种类: '重复', 标记: '刷新', 时刻: 99}}),
    {类型: '定时', 序: 5, 订阅号: 7, 名称: '定时', 详情: {种类: '重复', 标记: '刷新', 时刻: 99}});
  assert.deepEqual(体({类型: '宿主', 事件: {名称: '广播频道', 频道号: '2'}}),
    {类型: '宿主', 序: 5, 订阅号: 0, 名称: '广播频道', 详情: {名称: '广播频道', 频道号: '2'}});
  assert.deepEqual(体({类型: '关闭', 事件: {名称: '关闭'}}), {类型: '关闭', 序: 5, 订阅号: 0, 名称: '关闭'});
  assert.deepEqual(体({类型: '事件流', 新式: true, 事件: {订阅号: 3, 名称: 'message', 详情: {数据: '甲'}}}),
    {类型: '事件流', 序: 5, 订阅号: 3, 名称: 'message', 详情: {数据: '甲'}});
  assert.equal(原始事件文({类型: '界面', 事件: {名称: 'click', 标识: 'b', 值: '', 选中: false}}), '{"名称":"click","标识":"b","值":"","选中":false}', '旧式事件的原始 JSON 不变');
});

test('消息事件：详情文直接拼入；错误标记与元组形状', () => {
  const 项 = {类型: '消息', 序: 2, 事件: {订阅号: 1, 名称: '豫言甲'}, 新式: true, 详情文: '{"a":[1,2]}'};
  assert.deepEqual(JSON.parse(统一事件文(项)), {类型: '消息', 序: 2, 订阅号: 1, 名称: '豫言甲', 详情: {a: [1, 2]}});
  assert.deepEqual(消息事件元组(项), ['豫言甲', '{"a":[1,2]}', '']);
  const 错项 = {类型: '消息', 序: 3, 事件: {订阅号: 1, 名称: '豫言甲'}, 新式: true, 详情错误: '超过一 MiB'};
  assert.deepEqual(JSON.parse(统一事件文(错项)), {类型: '消息', 序: 3, 订阅号: 1, 名称: '豫言甲', 详情: null, 详情错误: '超过一 MiB'});
  assert.deepEqual(消息事件元组(错项), ['豫言甲', 'null', '网页消息详情超过一 MiB']);
  assert.deepEqual(界面事件元组({类型: '界面', 事件: {名称: 'click', 标识: 'x'}}), ['click', 'x']);
});

// ---------------------------------------------------------------------------
// 二、界面订阅器
// ---------------------------------------------------------------------------
const 造订阅器 = (正文, 选项) => {
  const {窗, 文} = 造页(正文, 选项);
  const 事件们 = [];
  let 关了 = false;
  const 订阅器 = 创建界面订阅器({根: 文, 全局: 窗, 投递: 项 => { 事件们.push(项); return true; }, 已关闭: () => 关了,
    删除若: 谓词 => { for (let i = 事件们.length - 1; i >= 0; i--) if (谓词(事件们[i])) 事件们.splice(i, 1); }});
  return {窗, 文, 订阅器, 事件们, 关: () => { 关了 = true; }};
};
const 键盘 = (窗, 型, 键, 附 = {}) => new 窗.KeyboardEvent(型, {key: 键, code: 附.code ?? 键, bubbles: true, cancelable: true, ...附});

test('委托：容器订阅 click，无 id 的后代触发时标识为空，操作键取最近的 data-yy', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<div id="容器"><button data-yy="删除"><span id="内">x</span><b>y</b></button><p id="段">文</p></div><i id="外" data-yy="不算">z</i>');
  const 号 = 订阅器.订阅('容器', 'click', '{}');
  assert.equal(typeof 号, 'number');
  文.querySelector('b').click();
  文.getElementById('内').click();
  文.getElementById('段').click();
  文.getElementById('外').click();
  assert.equal(事件们.length, 3, '容器之外的点击不发');
  assert.deepEqual(事件们.map(项 => [项.类型, 项.事件.标识, 项.事件.操作键, 项.事件.名称, 项.事件.订阅号]),
    [['界面', '', '删除', 'click', 号], ['界面', '内', '删除', 'click', 号], ['界面', '段', undefined, 'click', 号]]);
  assert.ok(事件们.every(项 => 项.新式 === true && 项.来源键 === '界面:' + 号));
});

test('操作键搜索止于订阅边界（含边界自身），文档订阅不设边界', () => {
  const {文, 订阅器, 事件们} = 造订阅器('<section data-yy="祖"><div id="容器" data-yy="界内"><a id="叶">a</a></div></section>');
  订阅器.订阅('容器', 'click', '{}');
  文.getElementById('叶').click();
  assert.equal(事件们.at(-1).事件.操作键, '界内');
  const {文: 文二, 订阅器: 订二, 事件们: 事二} = 造订阅器('<section data-yy="祖"><div id="容器"><a id="叶">a</a></div></section>');
  订二.订阅('容器', 'click', '{}');
  文二.getElementById('叶').click();
  assert.equal(事二.at(-1).事件.操作键, undefined, '祖先在边界外，不算');
  const {文: 文三, 订阅器: 订三, 事件们: 事三} = 造订阅器('<section data-yy="祖"><div id="容器"><a id="叶">a</a></div></section>');
  订三.订阅('文档', 'click', '{}');
  文三.getElementById('叶').click();
  assert.equal(事三.at(-1).事件.操作键, '祖', '文档订阅的边界是文档');
});

test('控件载荷：值、选中、键盘与鼠标修饰键、已阻止默认', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<input id="文本" value="abc"><input id="选" type="checkbox" checked><select id="下拉"><option value="1">a</option><option value="2" selected>b</option></select><textarea id="域">xyz</textarea><div id="盒">d</div>');
  订阅器.订阅('文档', 'click', '{}');
  订阅器.订阅('文档', 'change', '{}');
  订阅器.订阅('文档', 'keydown', '{}');
  文.getElementById('文本').click();
  文.getElementById('选').click();
  文.getElementById('下拉').dispatchEvent(new 窗.Event('change', {bubbles: true}));
  文.getElementById('域').dispatchEvent(键盘(窗, 'keydown', 'a', {ctrlKey: true, shiftKey: true, repeat: true}));
  文.getElementById('盒').dispatchEvent(new 窗.MouseEvent('click', {bubbles: true, ctrlKey: true, metaKey: true, button: 1}));
  assert.deepEqual(事件们.map(项 => [项.事件.名称, 项.事件.标识]),
    [['click', '文本'], ['click', '选'], ['change', '选'], ['change', '下拉'], ['keydown', '域'], ['click', '盒']]);
  const [文本, 选, 选变, 下拉, 域, 盒] = 事件们.map(项 => 项.事件);
  assert.equal(文本.值, 'abc'); assert.equal(文本.选中, undefined);
  assert.equal(选.选中, false, '点击后 checked 被翻转为假'); assert.equal(选.值, 'on'); assert.equal(选变.选中, false);
  assert.equal(下拉.值, '2');
  assert.equal(域.值, 'xyz'); assert.equal(域.键, 'a'); assert.equal(域.代码, 'a'); assert.equal(域.ctrl, true); assert.equal(域.shift, true);
  assert.equal(域.meta, false); assert.equal(域.alt, false); assert.equal(域.重复, true); assert.equal(域.组字中, false);
  assert.equal(盒.ctrl, true); assert.equal(盒.meta, true); assert.equal(盒.按钮, 1); assert.equal(盒.值, undefined);
  assert.equal(盒.已阻止默认, false);
});

test('口令输入框的值不进入事件载荷', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<input id="口令" type="password" value="秘密"><input id="明文" value="公开">');
  订阅器.订阅('文档', 'input', '{}');
  文.getElementById('口令').dispatchEvent(new 窗.Event('input', {bubbles: true}));
  文.getElementById('明文').dispatchEvent(new 窗.Event('input', {bubbles: true}));
  assert.equal(事件们[0].事件.值, undefined);
  assert.equal(事件们[0].事件.值过长, undefined);
  assert.equal(事件们[1].事件.值, '公开');
});

test('值过长时省略并标记，避免每个按键复制巨文', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<textarea id="域"></textarea>');
  文.getElementById('域').value = '甲'.repeat(70000);
  订阅器.订阅('文档', 'input', '{}');
  文.getElementById('域').dispatchEvent(new 窗.Event('input', {bubbles: true}));
  assert.equal(事件们[0].事件.值, undefined);
  assert.equal(事件们[0].事件.值过长, true);
});

test('键规则命中在原生回调里同步 preventDefault，不命中则不动', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<input id="输入">');
  订阅器.订阅('文档', 'keydown', JSON.stringify({键规则: [{键: 'Enter', shift: false}, {键: 'Tab', ctrl: false, meta: false}]}));
  const 派 = (键, 附) => { const 事 = 键盘(窗, 'keydown', 键, 附); 文.getElementById('输入').dispatchEvent(事); return 事.defaultPrevented; };
  assert.equal(派('Enter'), true);
  assert.equal(派('Enter', {shiftKey: true}), false);
  assert.equal(派('Tab'), true);
  assert.equal(派('Tab', {ctrlKey: true}), false);
  assert.equal(派('a'), false);
  assert.deepEqual(事件们.map(项 => 项.事件.已阻止默认), [true, false, true, false, false], '所有键盘事件都送达，载荷如实记录是否已阻止');
});

test('键规则：字母不分大小写，代码可作条件，组字期间默认不命中（含组字可开启）', () => {
  const {窗, 文, 订阅器} = 造订阅器('<input id="输入">');
  订阅器.订阅('文档', 'keydown', JSON.stringify({键规则: [{键: 'l', ctrl: true}, {代码: 'F5'}]}));
  订阅器.订阅('文档', 'keyup', JSON.stringify({键规则: [{键: 'Enter', 含组字: true}]}));
  const 派 = (型, 键, 附) => { const 事 = 键盘(窗, 型, 键, 附); 文.getElementById('输入').dispatchEvent(事); return 事.defaultPrevented; };
  assert.equal(派('keydown', 'l', {ctrlKey: true}), true);
  assert.equal(派('keydown', 'L', {ctrlKey: true, shiftKey: true}), true, 'Caps 或 Shift 下的大写字母亦合');
  assert.equal(派('keydown', 'x', {code: 'F5'}), true);
  assert.equal(派('keydown', 'l', {ctrlKey: true, isComposing: true}), false, '组字期间的按键不命中');
  assert.equal(派('keydown', 'Process', {code: 'F5', keyCode: 229}), false, 'keyCode 229 亦视为组字');
  assert.equal(派('keyup', 'Enter', {isComposing: true}), true, '含组字=真时命中');
});

test('键规则：仅命中时只送达命中的事件；停止传播与停止同处照策略执行', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<div id="祖"><input id="输入"></div>');
  订阅器.订阅('文档', 'keydown', JSON.stringify({键规则: [{键: 'Enter'}], 仅命中: true, 阻止默认: true, 停止同处: true}));
  let 后续 = 0;
  文.addEventListener('keydown', () => { 后续++; });
  const 派 = 键 => { const 事 = 键盘(窗, 'keydown', 键); 文.getElementById('输入').dispatchEvent(事); return 事; };
  const 命中 = 派('Enter'); const 未中 = 派('x');
  assert.equal(命中.defaultPrevented, true);
  assert.equal(未中.defaultPrevented, false);
  assert.equal(事件们.length, 1);
  assert.equal(事件们[0].事件.键, 'Enter');
  assert.equal(后续, 1, '命中的事件被 stopImmediatePropagation 截断，未命中的照常传到后续监听器');
  const {窗: 窗二, 文: 文二, 订阅器: 订二} = 造订阅器('<div id="祖"><input id="输入"></div>');
  订二.订阅('祖', 'keydown', JSON.stringify({键规则: [{键: 'Enter'}], 停止传播: true, 阻止默认: false}));
  let 冒到文档 = 0; 文二.addEventListener('keydown', () => { 冒到文档++; });
  const 事二 = 键盘(窗二, 'keydown', 'Enter'); 文二.getElementById('输入').dispatchEvent(事二);
  assert.equal(事二.defaultPrevented, false, '显式给了动作项，则只按所给动作');
  assert.equal(冒到文档, 0, '停止传播使事件不再冒到文档');
});

test('键规则未显式给动作项时，命中即阻止默认', () => {
  const {窗, 文, 订阅器} = 造订阅器('<input id="输入">');
  订阅器.订阅('文档', 'keydown', JSON.stringify({键规则: [{键: 'Escape'}]}));
  const 事 = 键盘(窗, 'keydown', 'Escape'); 文.getElementById('输入').dispatchEvent(事);
  assert.equal(事.defaultPrevented, true);
});

test('无键规则时，阻止默认对该订阅的每个事件生效（如 submit）', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<form id="表"><input id="值"><button id="交" type="submit">交</button></form>');
  订阅器.订阅('表', 'submit', '{"阻止默认":true}');
  const 事 = new 窗.Event('submit', {bubbles: true, cancelable: true});
  文.getElementById('表').dispatchEvent(事);
  assert.equal(事.defaultPrevented, true);
  assert.equal(事件们[0].事件.标识, '表');
  assert.equal(事件们[0].事件.已阻止默认, true);
});

test('仅选区为空：输入框或文档有选区时不命中', () => {
  const {窗, 文, 订阅器} = 造订阅器('<input id="输入" value="abcdef"><p id="段">正文文字</p>');
  订阅器.订阅('文档', 'keydown', JSON.stringify({键规则: [{键: 'c', ctrl: true, 仅选区为空: true}]}));
  const 输入 = 文.getElementById('输入');
  const 派 = () => { const 事 = 键盘(窗, 'keydown', 'c', {ctrlKey: true}); 输入.dispatchEvent(事); return 事.defaultPrevented; };
  输入.focus();
  输入.setSelectionRange(1, 3);
  assert.equal(派(), false, '输入框内有选区');
  输入.setSelectionRange(2, 2);
  assert.equal(派(), true, '折叠选区');
  const 段 = 文.getElementById('段');
  const 选 = 窗.getSelection(); 选.selectAllChildren(段);
  assert.equal(派(), false, '文档里有选中的文字');
  选.removeAllRanges();
  assert.equal(派(), true);
});

test('仅目标标识：目标须在该元素内；仅当属性：按属性现值判定（布尔属性用 true/false）', () => {
  const {窗, 文, 订阅器} = 造订阅器('<div id="域"><input id="内"></div><input id="外"><ul id="候选" hidden></ul><div id="状态" data-s="开"></div>');
  订阅器.订阅('文档', 'keydown', JSON.stringify({键规则: [{键: 'ArrowDown', 仅目标标识: '域'}, {键: 'Tab', 仅当属性: {标识: '候选', 属性: 'hidden', 值: 'false'}}, {键: 'F1', 仅当属性: {标识: '状态', 属性: 'data-s', 值: '开'}}, {键: 'F2', 仅当属性: {标识: '状态', 属性: 'data-x', 值: null}}]}));
  const 派 = (标识, 键) => { const 事 = 键盘(窗, 'keydown', 键); 文.getElementById(标识).dispatchEvent(事); return 事.defaultPrevented; };
  assert.equal(派('内', 'ArrowDown'), true);
  assert.equal(派('域', 'ArrowDown'), true, '目标为该元素本身亦可');
  assert.equal(派('外', 'ArrowDown'), false);
  assert.equal(派('外', 'Tab'), false, '候选框隐藏时不拦截');
  文.getElementById('候选').removeAttribute('hidden');
  assert.equal(派('外', 'Tab'), true, '候选框显示后拦截');
  assert.equal(派('外', 'F1'), true);
  文.getElementById('状态').setAttribute('data-s', '关');
  assert.equal(派('外', 'F1'), false);
  assert.equal(派('外', 'F2'), true, '值为 null 表示该属性不存在');
});

test('组字与输入事件的载荷', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<input id="命令">');
  for (const 名 of ['compositionstart', 'compositionupdate', 'compositionend', 'input']) 订阅器.订阅('文档', 名, '{}');
  const 命令 = 文.getElementById('命令');
  命令.dispatchEvent(new 窗.CompositionEvent('compositionstart', {bubbles: true, data: ''}));
  命令.dispatchEvent(new 窗.CompositionEvent('compositionupdate', {bubbles: true, data: 'ni'}));
  命令.dispatchEvent(new 窗.InputEvent('input', {bubbles: true, data: 'n', inputType: 'insertCompositionText', isComposing: true}));
  命令.dispatchEvent(new 窗.CompositionEvent('compositionend', {bubbles: true, data: '你'}));
  命令.dispatchEvent(new 窗.InputEvent('input', {bubbles: true, data: '你', inputType: 'insertText', isComposing: false}));
  assert.deepEqual(事件们.map(项 => [项.事件.名称, 项.事件.组字中, 项.事件.详情]), [
    ['compositionstart', true, {数据: ''}], ['compositionupdate', true, {数据: 'ni'}],
    ['input', true, {输入类型: 'insertCompositionText', 数据: 'n'}], ['compositionend', false, {数据: '你'}],
    ['input', false, {输入类型: 'insertText', 数据: '你'}]]);
});

test('不冒泡事件（focus、blur、close、cancel）也能在祖先上委托，并带关联标识/返回值', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<div id="根"><input id="甲"><input id="乙"><dialog id="窗框"></dialog></div>');
  for (const 名 of ['focus', 'blur', 'close', 'cancel']) 订阅器.订阅('根', 名, '{}');
  文.getElementById('甲').focus();
  文.getElementById('乙').focus();
  const 窗框 = 文.getElementById('窗框'); 窗框.returnValue = '好';
  窗框.dispatchEvent(new 窗.Event('cancel', {cancelable: true}));
  窗框.dispatchEvent(new 窗.Event('close'));
  assert.deepEqual(事件们.map(项 => [项.事件.名称, 项.事件.标识, 项.事件.详情]), [
    ['focus', '甲', {关联标识: ''}], ['blur', '甲', {关联标识: '乙'}], ['focus', '乙', {关联标识: '甲'}],
    ['cancel', '窗框', {返回值: '好'}], ['close', '窗框', {返回值: '好'}]]);
});

test('事件不可取消时，要求阻止默认的订阅在载荷里带 可取消:false', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<dialog id="弹窗"></dialog>');
  订阅器.订阅('弹窗', 'cancel', '{"阻止默认":true}');
  const 事 = new 窗.Event('cancel', {cancelable: false});
  文.getElementById('弹窗').dispatchEvent(事);
  assert.equal(事.defaultPrevented, false);
  assert.equal(事件们[0].事件.已阻止默认, false);
  assert.equal(事件们[0].事件.可取消, false);
  const 可取 = new 窗.Event('cancel', {cancelable: true});
  文.getElementById('弹窗').dispatchEvent(可取);
  assert.equal(事件们[1].事件.可取消, undefined);
});

test('dialog 的 cancel 可由策略同步阻止（Esc 不关闭）', () => {
  const {窗, 文, 订阅器} = 造订阅器('<dialog id="弹窗"></dialog>');
  订阅器.订阅('弹窗', 'cancel', '{"阻止默认":true}');
  const 事 = new 窗.Event('cancel', {cancelable: true});
  文.getElementById('弹窗').dispatchEvent(事);
  assert.equal(事.defaultPrevented, true);
});

test('scroll：详情带滚动度量；相邻同源滚动默认合并键相同', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('<div id="面板"><p id="内">x</p></div>');
  const 号 = 订阅器.订阅('面板', 'scroll', '{}');
  const 面板 = 文.getElementById('面板');
  Object.defineProperty(面板, 'scrollTop', {value: 12.4, configurable: true});
  Object.defineProperty(面板, 'scrollHeight', {value: 500, configurable: true});
  Object.defineProperty(面板, 'clientHeight', {value: 200, configurable: true});
  面板.dispatchEvent(new 窗.Event('scroll'));
  assert.deepEqual(事件们[0].事件.详情, {顶: 12, 总高: 500, 可视高: 200});
  assert.equal(事件们[0].合并键, 号 + ':面板');
  const 号二 = 订阅器.订阅('面板', 'click', '{}');
  面板.click();
  assert.equal(事件们.at(-1).合并键, undefined, '点击默认不合并');
  订阅器.订阅('面板', 'input', '{"合并":true}');
  面板.dispatchEvent(new 窗.Event('input', {bubbles: true}));
  assert.ok(事件们.at(-1).合并键);
});

test('一次订阅在首个事件后自动撤销；取消会移除该订阅未取走的事件', () => {
  const {文, 订阅器, 事件们} = 造订阅器('<button id="钮">钮</button><button id="乙">乙</button>');
  订阅器.订阅('钮', 'click', '{"一次":true}');
  文.getElementById('钮').click(); 文.getElementById('钮').click();
  assert.equal(事件们.length, 1);
  assert.equal(订阅器.订阅数(), 0);
  const 号 = 订阅器.订阅('乙', 'click', '{}');
  文.getElementById('乙').click(); 文.getElementById('乙').click();
  assert.equal(事件们.length, 3);
  assert.equal(订阅器.取消(号), true);
  assert.equal(事件们.length, 1, '未取走的事件随订阅撤销');
  文.getElementById('乙').click();
  assert.equal(事件们.length, 1);
  assert.equal(订阅器.取消(号), false, '重复取消无事');
  assert.equal(订阅器.取消(9999), false);
});

test('订阅错误一律在订阅时报出', () => {
  const {订阅器} = 造订阅器('<div id="域"></div>');
  const 错 = (目标, 名, 策略, 期望) => assert.throws(() => 订阅器.订阅(目标, 名, 策略), 期望);
  错('无此元素', 'click', '{}', /网页元素不存在/);
  错('', 'click', '{}', /网页元素不存在/);
  错('域', '乱写', '{}', /界面事件名无效/);
  错('域', '豫言甲', '{}', /界面事件名无效/);
  错('域', 'click', '不是 JSON', /不是有效 JSON/);
  错('域', 'click', '[]', /须为 JSON 对象/);
  错('域', 'click', '{"阻止默认":1}', /须为布尔/);
  错('域', 'click', '{"未知":true}', /未知字段：未知/);
  错('域', 'click', '{"键规则":[{"键":"a"}]}', /只适用于 keydown/);
  错('域', 'keydown', '{"键规则":[{}]}', /须含 键 或 代码/);
  错('域', 'keydown', '{"键规则":[{"键":"a","ctrl":"是"}]}', /须为布尔：ctrl/);
  错('域', 'keydown', '{"键规则":[{"键":"a","乱":1}]}', /未知字段：乱/);
  错('域', 'keydown', '{"键规则":[{"键":"a","仅当属性":{"标识":"x","属性":"Hidden","值":"1"}}]}', /仅当属性.属性 无效/);
  错('域', 'keydown', '{"键规则":[{"键":"a","仅当属性":{"标识":"x","属性":"hidden"}}]}', /仅当属性.值/);
  错('域', 'keydown', '{"仅命中":true}', /仅命中 须配合非空的键规则/);
  错('域', 'scroll', '{"被动":true,"阻止默认":true}', /被动订阅不能阻止默认/);
  错('域', 'keydown', JSON.stringify({键规则: Array.from({length: 33}, () => ({键: 'a'}))}), /不超过 32 条/);
  assert.equal(订阅器.订阅数(), 0, '失败的订阅不留痕');
});

test('订阅数量上限 256', () => {
  const {订阅器} = 造订阅器('<div id="域"></div>');
  for (let i = 0; i < 256; i++) 订阅器.订阅('域', 'click', '{}');
  assert.throws(() => 订阅器.订阅('域', 'click', '{}'), /订阅数量达到上限/);
});

test('窗口与文档目标：online/offline 归联机类，visibilitychange 归可见性类，popstate/hashchange 归历史类', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('');
  订阅器.订阅('窗口', 'online', '{}'); 订阅器.订阅('窗口', 'offline', '{}');
  订阅器.订阅('文档', 'visibilitychange', '{}');
  订阅器.订阅('窗口', 'popstate', '{}'); 订阅器.订阅('窗口', 'hashchange', '{}');
  窗.dispatchEvent(new 窗.Event('offline')); 窗.dispatchEvent(new 窗.Event('online'));
  文.dispatchEvent(new 窗.Event('visibilitychange'));
  窗.dispatchEvent(new 窗.PopStateEvent('popstate', {state: {页: 2}}));
  窗.dispatchEvent(new 窗.HashChangeEvent('hashchange', {oldURL: 'https://yuyan-lang.org/cloud/#a', newURL: 'https://yuyan-lang.org/cloud/#b'}));
  assert.deepEqual(事件们.map(项 => [项.类型, 项.事件.名称, 项.事件.详情]), [
    ['联机', 'offline', {联机: false}], ['联机', 'online', {联机: true}], ['可见性', 'visibilitychange', {可见: true}],
    ['历史', 'popstate', {网址: 'https://yuyan-lang.org/cloud/', 状态: {页: 2}}],
    ['历史', 'hashchange', {旧网址: 'https://yuyan-lang.org/cloud/#a', 新网址: 'https://yuyan-lang.org/cloud/#b'}]]);
  assert.ok(事件们.every(项 => 项.事件.标识 === undefined));
  assert.equal(订阅器.覆盖({type: 'click', target: 文.body}), false);
});

test('覆盖：新订阅覆盖同名事件时，旧式隐式监听让路（按目标边界判断）', () => {
  const {文, 订阅器} = 造订阅器('<div id="域"><a id="内">a</a></div><a id="外">b</a>');
  订阅器.订阅('域', 'click', '{}');
  assert.equal(订阅器.覆盖({type: 'click', target: 文.getElementById('内')}), true);
  assert.equal(订阅器.覆盖({type: 'click', target: 文.getElementById('外')}), false);
  assert.equal(订阅器.覆盖({type: 'input', target: 文.getElementById('内')}), false);
  订阅器.订阅('文档', 'click', '{}');
  assert.equal(订阅器.覆盖({type: 'click', target: 文.getElementById('外')}), true);
});

test('消息订阅：详情为 JSON 文，超限或不可序列化时给错误标记；重复订阅幂等；取消移除未取消息', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('');
  订阅器.订阅消息('豫言甲'); 订阅器.订阅消息('豫言甲');
  assert.equal(订阅器.消息订阅数(), 1);
  文.dispatchEvent(new 窗.CustomEvent('豫言甲', {detail: {a: [1, '二'], b: null}}));
  文.dispatchEvent(new 窗.CustomEvent('豫言甲'));
  文.dispatchEvent(new 窗.CustomEvent('豫言甲', {detail: '字'.repeat(400000)}));
  const 圈 = {}; 圈.自 = 圈;
  文.dispatchEvent(new 窗.CustomEvent('豫言甲', {detail: 圈}));
  assert.equal(事件们.length, 4);
  assert.equal(事件们[0].详情文, '{"a":[1,"二"],"b":null}');
  assert.equal(事件们[1].详情文, 'null');
  assert.equal(事件们[2].详情错误, '超过一 MiB', '40 万汉字约 1.2 MB');
  assert.equal(事件们[3].详情错误, '不是有效 JSON');
  assert.ok(事件们.every(项 => 项.类型 === '消息' && 项.事件.名称 === '豫言甲'));
  assert.equal(订阅器.取消消息('豫言甲'), true);
  assert.equal(事件们.length, 0);
  文.dispatchEvent(new 窗.CustomEvent('豫言甲', {detail: 1}));
  assert.equal(事件们.length, 0);
  assert.equal(订阅器.取消消息('豫言甲'), false);
  for (const 名 of ['甲', '', '豫甲', '豫言' + 'x'.repeat(125)]) assert.throws(() => 订阅器.订阅消息(名), /名称无效/);
  订阅器.订阅消息('豫言' + 'x'.repeat(122));
});

test('恰好一 MiB 的详情可通过，多一字节则错误', () => {
  const {窗, 文, 订阅器, 事件们} = 造订阅器('');
  订阅器.订阅消息('豫言大');
  文.dispatchEvent(new 窗.CustomEvent('豫言大', {detail: 'a'.repeat(1048576 - 2)}));
  文.dispatchEvent(new 窗.CustomEvent('豫言大', {detail: 'a'.repeat(1048576 - 1)}));
  assert.equal(事件们[0].详情错误, undefined);
  assert.equal(事件们[1].详情错误, '超过一 MiB');
});

test('清空后不再投递；关闭标记生效后处理函数直接忽略', () => {
  const {文, 订阅器, 事件们, 关} = 造订阅器('<button id="钮">钮</button>');
  订阅器.订阅('钮', 'click', '{}');
  关();
  文.getElementById('钮').click();
  assert.equal(事件们.length, 0);
  assert.throws(() => 订阅器.订阅('钮', 'click', '{}'), /已关闭/);
  订阅器.清空();
  assert.equal(订阅器.订阅数(), 0);
});

test('解析界面策略：默认值与合并推断', () => {
  const 甲 = 解析界面策略('', 'click');
  assert.deepEqual([甲.阻止默认, 甲.停止传播, 甲.捕获, 甲.合并, 甲.一次, 甲.键规则.length], [false, false, false, false, false, 0]);
  assert.equal(解析界面策略('{}', 'scroll').合并, true);
  assert.equal(解析界面策略('{"合并":false}', 'scroll').合并, false);
  assert.equal(解析界面策略('{}', 'focus').捕获, true, '不冒泡事件自动捕获');
  assert.equal(解析界面策略('{"键规则":[{"键":"a"}]}', 'keydown').阻止默认, true);
  assert.equal(解析界面策略('{"键规则":[{"键":"a"}],"停止传播":true}', 'keydown').阻止默认, false);
});

// ---------------------------------------------------------------------------
// 三、页面控制
// ---------------------------------------------------------------------------
const 造控制 = (正文, {网络 = async () => new Response('', {status: 404}), url = 'https://yuyan-lang.org/cloud/', 补窗 = () => {}} = {}) => {
  const {窗, 文} = 造页(正文, {url});
  补窗(窗);
  const 表 = 创建句柄表();
  const 观察器 = new Map();
  const 释放句柄全部 = 标识 => { 表.释放(标识); };
  const 控制 = 创建页面控制({根: 文, 全局: 窗, 路径: 窗.document.baseURI, 网络, 句柄: 表, 释放句柄全部});
  const 界面 = (操作, 标识 = '', 一 = '', 二 = '') => 控制.运行表(控制.界面操作表, 操作, [标识, 一, 二]);
  const 文树 = (操作, 一 = '', 二 = '', 三 = '') => 控制.运行表(控制.文树操作表, 操作, [一, 二, 三]);
  return {窗, 文, 表, 控制, 界面, 文树, 装入: (...参) => 控制.装入模板(...参)};
};
const 补模态 = 窗 => {
  const P = 窗.HTMLDialogElement.prototype;
  P.showModal = function () { if (this.hasAttribute('open')) throw new 窗.DOMException('已打开', 'InvalidStateError'); this.setAttribute('open', ''); };
  P.close = function (返回值) { if (!this.hasAttribute('open')) return; this.removeAttribute('open'); if (返回值 !== undefined) this.returnValue = 返回值; this.dispatchEvent(new 窗.Event('close')); };
};

test('界面读回：值、显示、禁用、只读、类名', () => {
  const {文, 界面} = 造控制('<input id="文本" value="abc"><select id="下拉"><option value="1">a</option><option value="2">b</option></select><textarea id="域">tt</textarea><div id="盒"></div><span id="行" class="旧 类">x</span>');
  assert.equal(界面('读取值', '文本'), 'abc');
  assert.equal(界面('读取值', '域'), 'tt');
  界面('设置值', '文本', '新值😀');
  assert.equal(文.getElementById('文本').value, '新值😀');
  界面('设置值', '下拉', '2');
  assert.equal(界面('读取值', '下拉'), '2');
  assert.throws(() => 界面('设置值', '下拉', '9'), /没有此选项值/);
  assert.throws(() => 界面('读取值', '盒'), /不是输入控件/);
  assert.throws(() => 界面('设置值', '盒', 'x'), /不是输入控件/);
  assert.throws(() => 界面('读取值', '无此'), /网页元素不存在：无此/);
  界面('设置显示', '盒', 'false'); assert.equal(文.getElementById('盒').hidden, true);
  界面('设置显示', '盒', 'true'); assert.equal(文.getElementById('盒').hidden, false);
  界面('设置禁用', '文本', 'true'); assert.equal(文.getElementById('文本').disabled, true);
  界面('设置禁用', '文本', 'false'); assert.equal(文.getElementById('文本').disabled, false);
  界面('设置只读', '文本', 'true'); assert.equal(文.getElementById('文本').readOnly, true);
  界面('设置只读', '文本', 'false'); assert.equal(文.getElementById('文本').readOnly, false);
  assert.throws(() => 界面('设置显示', '盒', '真'), /布尔参数/);
  界面('设置类名', '行', '甲 乙-丙_1  丁'); assert.equal(文.getElementById('行').className, '甲 乙-丙_1 丁');
  界面('设置类名', '行', ''); assert.equal(文.getElementById('行').hasAttribute('class'), false);
  for (const 坏 of ['a"b', 'a<b', 'a.b', 'a\tb', 'a\nb', 'a#b', 'a;b', 'a,b']) assert.throws(() => 界面('设置类名', '行', 坏), /类名含不允许的字符/, JSON.stringify(坏));
  assert.throws(() => 界面('设置类名', '行', 'x'.repeat(65)), /类名过长/);
  assert.throws(() => 界面('设置类名', '行', Array.from({length: 33}, (_, i) => 'c' + i).join(' ')), /类名过多/);
  assert.throws(() => 界面('不存在的操作'), /网页操作不受支持/);
  assert.throws(() => 界面('constructor'), /网页操作不受支持/);
});

test('设置节点类标记：单个类名的有无，其余类名不动；非法类名拒绝', () => {
  const {文, 界面} = 造控制('<div id="盒" class="甲 乙"></div><iframe id="框" sandbox></iframe>');
  界面('设置类标记', '盒', '丙', 'true'); assert.equal(文.getElementById('盒').className, '甲 乙 丙');
  界面('设置类标记', '盒', '乙', 'false'); assert.equal(文.getElementById('盒').className, '甲 丙');
  界面('设置类标记', '盒', '丙', 'true'); assert.equal(文.getElementById('盒').className, '甲 丙', '已有则不重复');
  界面('设置类标记', '框', '现', 'true'); assert.equal(文.getElementById('框').className, '现');
  for (const 坏 of ['', 'a b', 'a"b', 'x'.repeat(65), 'a.b']) assert.throws(() => 界面('设置类标记', '盒', 坏, 'true'), /类标记须为单个类名/, JSON.stringify(坏));
  assert.throws(() => 界面('设置类标记', '盒', '丁', '真'), /布尔参数/);
  assert.throws(() => 界面('设置类标记', '无此', '丁', 'true'), /网页元素不存在/);
});

test('读回超过八 MiB 的控件值或选区时报错，避免越过值桥上限', () => {
  const {文, 界面} = 造控制('<textarea id="域"></textarea>');
  文.getElementById('域').value = '甲'.repeat(3 * 1024 * 1024);
  assert.throws(() => 界面('读取值', '域'), /超过八 MiB，不能读回/);
  文.getElementById('域').value = 'a'.repeat(8 * 1024 * 1024);
  assert.equal(界面('读取值', '域').length, 8 * 1024 * 1024);
  文.getElementById('域').value = 'a'.repeat(8 * 1024 * 1024 + 1);
  assert.throws(() => 界面('读取值', '域'), /超过八 MiB，不能读回/);
});

test('界面读回：文件输入框不可设值；危险元素不可操作', () => {
  const {界面} = 造控制('<input id="传" type="file"><script id="脚本">1</script><style id="样式"></style><iframe id="框" sandbox></iframe>');
  assert.throws(() => 界面('设置值', '传', 'x'), /文件输入框/);
  assert.throws(() => 界面('设置文字', '脚本', 'alert(1)'), /类型不允许操作：script/);
  assert.throws(() => 界面('设置文字', '样式', 'a{}'), /类型不允许操作：style/);
  assert.throws(() => 界面('设置文字', '框', 'x'), /类型不允许操作：iframe/);
  assert.throws(() => 界面('设置属性', '脚本', 'data-x', '1'), /类型不允许操作：script/);
  界面('设置属性', '框', 'title', '预览');
  assert.throws(() => 界面('设置属性', '框', 'src', 'https://x.example/'), /iframe 的 src/);
  assert.throws(() => 界面('设置属性', '框', 'data-yy-frame-origins', 'https://x.example'), /宿主保留/);
});

test('聚焦、光标（按码点计）、选区文字', () => {
  const {窗, 文, 界面} = 造控制('<input id="甲" value="a😀bc"><input id="乙"><div id="盒"><input id="内"></div><textarea id="域">行一\n行二</textarea><p id="段">正文文字</p>');
  assert.equal(界面('读取聚焦标识'), '');
  界面('聚焦', '甲');
  assert.equal(界面('读取聚焦标识'), '甲');
  assert.equal(界面('包含焦点', '甲'), 'true');
  assert.equal(界面('包含焦点', '乙'), 'false');
  assert.equal(界面('包含焦点', '盒'), 'false');
  界面('聚焦', '内');
  assert.equal(界面('包含焦点', '盒'), 'true', '后代有焦点即含焦点');
  界面('设置光标', '甲', '1', '3');
  assert.equal(文.getElementById('甲').selectionStart, 1);
  assert.equal(文.getElementById('甲').selectionEnd, 4, '码点 3 对应 UTF-16 位置 4（😀 占两个码元）');
  assert.equal(界面('读取光标起', '甲'), '1');
  assert.equal(界面('读取光标止', '甲'), '3');
  界面('设置光标', '甲', '4', '4');
  assert.equal(文.getElementById('甲').selectionStart, 5);
  assert.throws(() => 界面('设置光标', '甲', '2', '9'), /位置超出文字长度/);
  assert.throws(() => 界面('设置光标', '甲', '3', '2'), /光标位置无效/);
  assert.throws(() => 界面('设置光标', '甲', '-1', '2'), /光标位置无效/);
  assert.throws(() => 界面('设置光标', '甲', 'x', '2'), /须为整数/);
  assert.throws(() => 界面('设置光标', '盒', '0', '0'), /不是文本输入框/);
  界面('聚焦', '甲'); 界面('设置光标', '甲', '1', '3');
  assert.equal(界面('读取选区文字'), '😀b', '输入框内的选区');
  界面('设置光标', '甲', '0', '0');
  const 选 = 窗.getSelection(); 选.selectAllChildren(文.getElementById('段'));
  assert.equal(界面('读取选区文字'), '正文文字');
  选.removeAllRanges();
  assert.equal(界面('读取选区文字'), '');
});

test('滚动度量与设置；页面状态；节点可见', () => {
  const {窗, 文, 界面} = 造控制('<div id="面板" style="display:block"></div><div id="藏" hidden><p id="藏内">x</p></div><div id="样式藏" style="display:none"><b id="样式藏内">x</b></div>', {补窗: 窗 => { 窗.matchMedia = 查 => ({matches: 查 === '(max-width: 700px)'}); }});
  const 面板 = 文.getElementById('面板');
  Object.defineProperty(面板, 'scrollHeight', {value: 900, configurable: true});
  Object.defineProperty(面板, 'clientHeight', {value: 300, configurable: true});
  界面('设置滚动顶', '面板', '250');
  assert.deepEqual(JSON.parse(界面('读取滚动度量', '面板')), {顶: 250, 总高: 900, 可视高: 300});
  assert.throws(() => 界面('设置滚动顶', '面板', '1.5'), /须为整数/);
  assert.deepEqual(JSON.parse(界面('读取页面状态')), {可见: true, 联机: true, 窄屏: true});
  assert.equal(界面('读取节点可见', '面板'), 'true');
  assert.equal(界面('读取节点可见', '藏'), 'false');
  assert.equal(界面('读取节点可见', '藏内'), 'false', '祖先隐藏则后代不可见');
  assert.equal(界面('读取节点可见', '样式藏内'), 'false');
  const 无媒体 = 造控制('');
  assert.deepEqual(JSON.parse(无媒体.界面('读取页面状态')), {可见: true, 联机: true, 窄屏: false});
});

test('模态框：打开、关闭；重复打开由浏览器报错；非 dialog 报错', () => {
  const {文, 界面} = 造控制('<dialog id="弹窗"><p>内</p></dialog><div id="盒"></div>', {补窗: 补模态});
  界面('打开模态框', '弹窗');
  assert.equal(文.getElementById('弹窗').open, true);
  assert.throws(() => 界面('打开模态框', '弹窗'), /当前状态不允许此操作/);
  界面('关闭模态框', '弹窗');
  assert.equal(文.getElementById('弹窗').open, false);
  界面('关闭模态框', '弹窗');
  assert.throws(() => 界面('打开模态框', '盒'), /不是 dialog/);
  assert.throws(() => 界面('关闭模态框', '盒'), /不是 dialog/);
  const 无 = 造控制('<dialog id="弹窗"></dialog>');
  assert.throws(() => 无.界面('打开模态框', '弹窗'), /不支持 dialog 模态框/);
});

// ---- 属性白册 ----
const 属性表 = [
  // [标签, 属性, 值, 应接受]
  ['div', 'class', '甲 乙', true], ['div', 'class', 'a"b', false],
  ['div', 'role', 'listbox', true], ['div', 'role', 'List', false], ['div', 'role', 'a b', false],
  ['button', 'type', 'submit', true], ['button', 'type', 'checkbox', false], ['input', 'type', 'checkbox', true], ['input', 'type', 'file', false], ['div', 'type', 'text', false],
  ['a', 'title', '提示', true], ['div', 'title', 'x'.repeat(4097), false],
  ['div', 'hidden', '', true], ['div', 'hidden', 'true', true], ['div', 'hidden', 'false', true], ['div', 'hidden', 'yes', false],
  ['button', 'disabled', 'true', true], ['input', 'readonly', '', true],
  ['input', 'placeholder', '提示', true], ['div', 'placeholder', 'x', false],
  ['div', 'tabindex', '-1', true], ['div', 'tabindex', '0', true], ['div', 'tabindex', '3', false], ['div', 'tabindex', 'a', false],
  ['time', 'datetime', '2026-09-25T12:00:00Z', true], ['time', 'datetime', 'x y;', false], ['div', 'datetime', '2026', false],
  ['input', 'maxlength', '80', true], ['input', 'maxlength', '-1', false], ['textarea', 'rows', '4', true], ['div', 'rows', '4', false],
  ['td', 'colspan', '2', true], ['div', 'colspan', '2', false],
  ['div', 'aria-label', '标签', true], ['div', 'aria-live', 'polite', true], ['div', 'aria-', 'x', false],
  ['div', 'data-task-id', 'abc-1', true], ['div', 'data-yy', '删除', true], ['div', 'data-yy-frame-origins', 'https://x', false], ['div', 'data-Bad', '1', false], ['div', 'data-', '1', false],
  ['a', 'href', '/cloud/?service=code', true], ['a', 'href', '#锚', true], ['a', 'href', 'https://yuyan-lang.org/x', true], ['a', 'href', 'http://example.com/', true], ['a', 'href', 'HTTPS://Example.com', true],
  ['a', 'href', 'javascript:alert(1)', false], ['a', 'href', ' javascript:alert(1)', false], ['a', 'href', 'JaVaScRiPt:1', false], ['a', 'href', 'data:text/html,x', false], ['a', 'href', '//evil.example/', false],
  ['a', 'href', '/\\evil.example', false], ['a', 'href', 'vbscript:x', false], ['a', 'href', 'mailto:a@b.c', false], ['a', 'href', 'ftp://x', false], ['a', 'href', 'x.html', false], ['a', 'href', '', false], ['a', 'href', 'https://', false],
  ['a', 'href', '/a\nb', false], ['a', 'href', '/a b', false],
  ['div', 'href', '/x', false], ['span', 'href', '#x', false],
  ['a', 'target', '_blank', true], ['a', 'target', '_self', true], ['a', 'target', '_top', false], ['a', 'target', 'win', false], ['div', 'target', '_blank', false],
  ['input', 'value', 'v', true], ['option', 'value', '1', true], ['div', 'value', '1', false],
  ['label', 'for', '输入', true], ['div', 'for', 'x', false], ['label', 'for', 'a b', false],
  ['input', 'checked', 'true', true], ['option', 'selected', '', true], ['details', 'open', 'true', true],
  ['div', 'style', 'color:red', false], ['div', 'onclick', 'alert(1)', false], ['div', 'onload', '1', false], ['img', 'src', '/x.png', false],
  ['a', 'rel', 'noopener', false], ['div', 'srcdoc', '<script>', false], ['div', 'formaction', '/x', false], ['div', 'contenteditable', 'true', false], ['div', 'name', 'x', false],
  ['div', 'CLASS', 'a', false], ['div', '', 'x', false], ['div', 'a b', 'x', false], ['div', 'x'.repeat(65), 'x', false],
  ['div', 'data-x', 'a\u0000b', false], ['div', 'data-x', 'a\tb\nc', true], ['div', 'data-x', 'x'.repeat(65537), false]
];
test('属性白册：设置节点属性与页面节点属性对同一张表一致', () => {
  const {文, 界面, 文树} = 造控制('');
  const 违例 = [];
  属性表.forEach(([标签, 名, 值, 应], 序) => {
    const 标识 = '测' + 序;
    // 界面（按标识）
    const 元 = 文.createElement(标签.replace('img', 'span')); 元.id = 标识; 文.body.append(元);
    if (标签 === 'img') { 元.remove(); const 图 = 文.createElement('img'); 图.id = 标识; 文.body.append(图); }
    let 接受;
    try { 界面('设置属性', 标识, 名, 值); 接受 = true; } catch { 接受 = false; }
    if (接受 !== 应) 违例.push(['界面', 标签, 名, 值.slice(0, 40), 应]);
    // 文树（按句柄）：白册内的标签才能新建
    if (标签 !== 'img') {
      const 号 = 文树('新建', 标签, '', '');
      let 接受二;
      try { 文树('设置属性', 号, 名, 值); 接受二 = true; } catch { 接受二 = false; }
      if (接受二 !== 应) 违例.push(['文树', 标签, 名, 值.slice(0, 40), 应]);
      文树('释放', 号);
    }
  });
  assert.deepEqual(违例, []);
});

test('target=_blank 强制 rel=noopener noreferrer；布尔属性 false 移除；属性写入后的 DOM 形态', () => {
  const {文, 界面, 文树} = 造控制('<a id="链">x</a><button id="钮" disabled>x</button>');
  界面('设置属性', '链', 'href', 'http://example.com/a?b=1');
  界面('设置属性', '链', 'target', '_blank');
  const 链 = 文.getElementById('链');
  assert.equal(链.getAttribute('href'), 'http://example.com/a?b=1');
  assert.equal(链.getAttribute('rel'), 'noopener noreferrer');
  界面('设置属性', '钮', 'disabled', 'false');
  assert.equal(文.getElementById('钮').hasAttribute('disabled'), false);
  界面('设置属性', '钮', 'hidden', '');
  assert.equal(文.getElementById('钮').getAttribute('hidden'), '');
  const 号 = 文树('新建', 'a', '文', '类');
  文树('设置属性', 号, 'target', '_blank');
  文树('设置属性', 号, 'href', 'https://yuyan-lang.org/');
  const 元 = 文树('取得', '钮');
  assert.equal(typeof 元, 'string');
});

test('id 属性：须唯一且无空白与引号', () => {
  const {文, 界面, 文树} = 造控制('<div id="已有"></div><div id="乙"></div>');
  assert.throws(() => 界面('设置属性', '乙', 'id', '已有'), /标识已存在/);
  界面('设置属性', '乙', 'id', '乙二');
  assert.ok(文.getElementById('乙二'));
  assert.throws(() => 界面('设置属性', '乙二', 'id', 'a b'), /标识无效/);
  assert.throws(() => 界面('设置属性', '乙二', 'id', 'a"b'), /标识无效/);
  assert.throws(() => 界面('设置属性', '乙二', 'id', 'x'.repeat(129)), /标识无效/);
  界面('设置属性', '乙二', 'id', '乙二');
});

// ---- 文树：标签与建树 ----
test('标签白册：白册内可造，危险标签与未知标签一律拒绝；空元素不可带文字', () => {
  const {文树} = 造控制('');
  for (const 签 of ['a', 'span', 'small', 'h1', 'h2', 'h3', 'h4', 'h5', 'p', 'section', 'div', 'article', 'b', 'strong', 'br', 'button', 'table', 'thead', 'tbody', 'tr', 'th', 'td', 'pre', 'code', 'time', 'ul', 'ol', 'li', 'label', 'select', 'option', 'nav', 'form', 'input', 'textarea', 'details', 'summary']) {
    const 号 = 文树('新建', 签, '', ''); 文树('释放', 号);
  }
  for (const 签 of ['script', 'style', 'iframe', 'object', 'embed', 'link', 'meta', 'base', 'img', 'svg', 'video', 'dialog', 'template', 'frame', 'applet', 'A', 'Div', '', 'a b', '__proto__', 'constructor'])
    assert.throws(() => 文树('新建', 签, '', ''), /页面标签不受支持/, 签);
  assert.throws(() => 文树('新建', 'br', '文', ''), /空元素不能含文字/);
  assert.throws(() => 文树('新建', 'input', '文', ''), /空元素不能含文字/);
  文树('释放', 文树('新建', 'div', '文', '甲 乙'));
  assert.throws(() => 文树('新建', 'div', '', '坏"类'), /类名含不允许的字符/);
});

test('取得页面节点：危险元素不给句柄；句柄无效与非节点句柄报错', () => {
  const {表, 文树} = 造控制('<div id="容器"></div><script id="脚本"></script><style id="样"></style><iframe id="框" sandbox></iframe>');
  const 号 = 文树('取得', '容器');
  assert.equal(文树('取得', '容器'), 号, '同一元素重复取得复用同一句柄');
  assert.equal(表.数量(), 1);
  for (const 标识 of ['脚本', '样', '框']) assert.throws(() => 文树('取得', 标识), /类型不允许操作/);
  assert.throws(() => 文树('取得', '无此'), /网页元素不存在/);
  assert.throws(() => 文树('添加子', '999', 号), /页面节点句柄无效：999/);
  const 非节点 = 表.登记({});
  assert.throws(() => 文树('添加子', 号, 非节点), /句柄不是页面节点/);
  assert.throws(() => 文树('设置文字', 非节点, 'x'), /句柄不是页面节点/);
});

test('一次建树：结构、类、属性、文字、文本子节点，只占一个句柄', () => {
  const {文, 表, 文树} = 造控制('<div id="容器"></div>');
  const 描述 = {标签: 'article', 类: '事件 事件-命令', 属性: {'data-yy': '开', 'data-task-id': 'a1', id: '行1', hidden: false, tabindex: 0},
    文字: '首', 子: [
      {标签: 'p', 文字: '段落'}, '裸文字',
      {标签: 'ul', 子: [{标签: 'li', 文字: '一'}, {标签: 'li', 属性: {title: '二'}, 子: [{标签: 'b', 文字: '粗'}]}]},
      {标签: 'input', 属性: {type: 'text', value: 'v', maxlength: 5}}, {标签: 'br'}]};
  const 根号 = 文树('构建树', JSON.stringify(描述));
  assert.equal(表.数量(), 1, '整棵树只占一个句柄');
  assert.equal(文.getElementById('行1'), null, '建好但未插入文档');
  文树('添加子', 文树('取得', '容器'), 根号);
  const 行 = 文.getElementById('行1');
  assert.ok(行);
  assert.equal(行.className, '事件 事件-命令');
  assert.equal(行.getAttribute('data-yy'), '开');
  assert.equal(行.hasAttribute('hidden'), false);
  assert.equal(行.getAttribute('tabindex'), '0');
  assert.equal(行.firstChild.nodeType, 3);
  assert.equal(行.firstChild.data, '首');
  assert.equal(行.querySelector('p').textContent, '段落');
  assert.equal(行.childNodes[2].data, '裸文字');
  assert.equal(行.querySelectorAll('li').length, 2);
  assert.equal(行.querySelector('li b').textContent, '粗');
  assert.equal(行.querySelector('input').getAttribute('maxlength'), '5');
  assert.equal(行.querySelector('br').tagName, 'BR');
  assert.equal(行.textContent, '首段落裸文字一粗');
});

test('一次建树的限额：深度 32、节点 2000、文字 1 MiB、描述 2 MiB', () => {
  const {表, 文树} = 造控制('');
  const 链 = 深 => { let 项 = {标签: 'span', 文字: '叶'}; for (let i = 1; i < 深; i++) 项 = {标签: 'div', 子: [项]}; return 项; };
  文树('释放', 文树('构建树', JSON.stringify(链(32))));
  assert.throws(() => 文树('构建树', JSON.stringify(链(33))), /深度超过 32/);
  const 宽 = 数 => ({标签: 'ul', 子: Array.from({length: 数}, () => ({标签: 'li'}))});
  文树('释放', 文树('构建树', JSON.stringify(宽(1999))));
  assert.throws(() => 文树('构建树', JSON.stringify(宽(2000))), /节点数超过 2000/);
  文树('释放', 文树('构建树', JSON.stringify({标签: 'pre', 文字: 'a'.repeat(1048576)})));
  assert.throws(() => 文树('构建树', JSON.stringify({标签: 'pre', 文字: 'a'.repeat(1048577)})), /文字总量超过 1 MiB/);
  assert.throws(() => 文树('构建树', JSON.stringify({标签: 'pre', 文字: '甲'.repeat(400000)})), /文字总量超过 1 MiB/, '按 UTF-8 字节计');
  assert.throws(() => 文树('构建树', JSON.stringify({标签: 'div', 子: ['a'.repeat(700000), 'b'.repeat(400000)]})), /文字总量超过 1 MiB/);
  assert.throws(() => 文树('构建树', JSON.stringify({标签: 'div', 属性: {'data-a': 'x'.repeat(60000), 'data-b': 'x'.repeat(60000)}, 子: Array.from({length: 20}, () => ({标签: 'span', 属性: {'data-c': 'x'.repeat(60000)}}))}).repeat(1)), /属性总量超过 1 MiB|属性值无效或过长/);
  assert.throws(() => 文树('构建树', '{"标签":"div","文字":"' + 'a'.repeat(2 * 1024 * 1024) + '"}'), /描述超过 2 MiB/);
  assert.equal(表.数量(), 0, '失败的建树不留句柄');
});

test('一次建树的错误：未知字段、危险标签、危险属性、重复 id、类型错误，且不留句柄', () => {
  const {表, 文树} = 造控制('<div id="已有"></div>');
  const 坏 = (描述, 期望) => assert.throws(() => 文树('构建树', typeof 描述 === 'string' ? 描述 : JSON.stringify(描述)), 期望, JSON.stringify(描述).slice(0, 80));
  坏('不是JSON', /不是有效 JSON/);
  坏('"字"', /根须为元素描述/);
  坏('[]', /须为对象或文字/);
  坏('null', /须为对象或文字/);
  坏({标签: 'script'}, /页面标签不受支持：script/);
  坏({标签: 'div', 子: [{标签: 'iframe'}]}, /页面标签不受支持：iframe.*根\.子\[0\]/);
  坏({标签: 'div', 子: [{标签: 'p', 子: [{标签: 'object'}]}]}, /根\.子\[0\]\.子\[0\]/);
  坏({}, /页面标签不受支持/);
  坏({标签: 'div', 乱: 1}, /描述含未知字段：乱/);
  坏({标签: 'div', 类: 5}, /页面类名须为文字/);
  坏({标签: 'div', 属性: []}, /属性须为对象/);
  坏({标签: 'div', 属性: {onclick: 'alert(1)'}}, /页面属性不受支持：onclick/);
  坏({标签: 'a', 属性: {href: 'javascript:alert(1)'}}, /页面链接不受支持/);
  坏({标签: 'div', 属性: {style: 'x'}}, /页面属性不受支持：style/);
  坏({标签: 'div', 属性: {'data-x': {}}}, /属性值须为文字、数字或布尔/);
  坏({标签: 'div', 属性: {'data-x': null}}, /属性值须为文字、数字或布尔/);
  坏({标签: 'div', 属性: {id: '已有'}}, /标识已存在/);
  坏({标签: 'div', 子: [{标签: 'p', 属性: {id: '同'}}, {标签: 'p', 属性: {id: '同'}}]}, /树内标识重复/);
  坏({标签: 'div', 文字: 5}, /文字须为文字/);
  坏({标签: 'div', 子: '字'}, /子项须为数组/);
  坏({标签: 'br', 文字: 'x'}, /空元素不能含文字/);
  坏({标签: 'br', 子: [{标签: 'span'}]}, /空元素不能含子节点/);
  坏({标签: 'input', 子: ['x']}, /空元素不能含子节点/);
  assert.equal(表.数量(), 0);
});

test('句柄不泄漏：建树与释放循环一万次不撞句柄上限，句柄数回到零', () => {
  const {表, 文树} = 造控制('<div id="容器"></div>');
  const 容器 = 文树('取得', '容器');
  const 描述 = JSON.stringify({标签: 'article', 类: '行', 子: [{标签: 'p', 文字: '甲'}, {标签: 'p', 文字: '乙'}]});
  let 最大 = 0;
  for (let i = 0; i < 10000; i++) {
    const 根号 = 文树('构建树', 描述);
    文树('添加子', 容器, 根号);
    文树('释放', 根号);
    最大 = Math.max(最大, 表.数量());
    if (i % 100 === 99) 文树('清空子', 容器);
  }
  assert.ok(最大 <= 2, '同时至多容器与一棵新树：' + 最大);
  assert.equal(表.数量(), 1);
});

test('不释放则句柄达到上限时报清晰的错误，释放后即可恢复', () => {
  const {表, 文树} = 造控制('');
  const 号们 = [];
  for (let i = 0; i < 4096; i++) 号们.push(文树('新建', 'span', '', ''));
  assert.throws(() => 文树('新建', 'span', '', ''), /页面节点句柄达到上限（4096）/);
  文树('释放', 号们.pop());
  文树('新建', 'span', '', '');
  assert.equal(表.数量(), 4096);
});

test('移除、清空、替换会同时释放被移除子树内所有节点的句柄', () => {
  const {文, 表, 文树} = 造控制('<div id="容器"></div>');
  const 容器 = 文树('取得', '容器');
  const 造行 = () => {
    const 行 = 文树('新建', 'div', '', ''); const 子 = 文树('新建', 'span', '', ''); const 孙 = 文树('新建', 'b', '', '');
    文树('添加子', 子, 孙); 文树('添加子', 行, 子); 文树('添加子', 容器, 行);
    return {行, 子, 孙};
  };
  const 一 = 造行(); const 二 = 造行(); const 三 = 造行();
  assert.equal(表.数量(), 10);
  文树('移除', 一.行);
  assert.equal(表.数量(), 10 - 3, '移除一行连同它下面的句柄');
  assert.equal(文.getElementById('容器').children.length, 2);
  assert.throws(() => 文树('设置文字', 一.孙, 'x'), /页面节点句柄无效/);
  文树('替换子', 容器, JSON.stringify([二.行]));
  assert.equal(表.数量(), 10 - 3 - 3, '被替换掉的行及其子树句柄同释');
  assert.equal(文.getElementById('容器').children.length, 1);
  assert.doesNotThrow(() => 文树('设置文字', 二.孙, '仍有效'));
  文树('清空子', 容器);
  assert.equal(表.数量(), 1, '清空后只剩容器句柄');
  assert.equal(文.getElementById('容器').children.length, 0);
});

test('插入子节点前、追加、移动、替换的语义与错误', () => {
  const {文, 文树} = 造控制('<ul id="列"></ul>');
  const 列 = 文树('取得', '列');
  const 项 = 文本 => 文树('新建', 'li', 文本, '');
  const 甲 = 项('甲'), 乙 = 项('乙'), 丙 = 项('丙');
  文树('插入子前', 列, 甲, '');
  文树('插入子前', 列, 丙, '');
  文树('插入子前', 列, 乙, 丙);
  const 文本们 = () => Array.from(文.getElementById('列').children, 元 => 元.textContent).join('');
  assert.equal(文本们(), '甲乙丙');
  文树('插入子前', 列, 丙, 甲);
  assert.equal(文本们(), '丙甲乙', '已在文档中的节点是移动');
  文树('添加子', 列, 丙);
  assert.equal(文本们(), '甲乙丙');
  文树('替换子', 列, JSON.stringify([丙, 乙]));
  assert.equal(文本们(), '丙乙');
  assert.throws(() => 文树('插入子前', 列, 项('丁'), 甲), /页面节点句柄无效/, '甲已被替换掉且句柄已释放');
  const 别的 = 文树('新建', 'div', '', '');
  assert.throws(() => 文树('插入子前', 列, 丙, 别的), /参照节点不是父节点的子节点/);
  assert.throws(() => 文树('添加子', 丙, 列), /层级无效/, '祖先不能放进后代');
  assert.throws(() => 文树('添加子', 列, 列), /层级无效/);
  assert.throws(() => 文树('替换子', 列, JSON.stringify([丙, 丙])), /含重复项/);
  assert.throws(() => 文树('替换子', 列, '不是JSON'), /不是有效 JSON/);
  assert.throws(() => 文树('替换子', 列, '[1]'), /文字数组/);
  assert.throws(() => 文树('替换子', 列, JSON.stringify(Array.from({length: 2001}, (_, i) => String(i)))), /2000 项/);
  文树('替换子', 列, '[]');
  assert.equal(文本们(), '');
});

test('追加页面节点文字：并入末尾文本节点，其他情形新建文本节点，有上限', () => {
  const {文, 文树} = 造控制('<pre id="输出"></pre>');
  const 输出 = 文树('取得', '输出');
  文树('追加文字', 输出, '你');
  文树('追加文字', 输出, '好');
  文树('追加文字', 输出, '，豫言');
  const 元 = 文.getElementById('输出');
  assert.equal(元.textContent, '你好，豫言');
  assert.equal(元.childNodes.length, 1, '连续追加不产生碎片文本节点');
  const 强 = 文树('新建', 'b', '粗', '');
  文树('添加子', 输出, 强);
  文树('追加文字', 输出, '尾');
  assert.equal(元.childNodes.length, 3);
  assert.equal(元.lastChild.data, '尾');
  assert.throws(() => 文树('追加文字', 文树('新建', 'br', '', ''), 'x'), /空元素不能含文字/);
  assert.throws(() => 文树('追加文字', 输出, 'a'.repeat(8 * 1024 * 1024 + 1)), /超过八 MiB/);
  文树('追加文字', 输出, '');
});

test('设置页面文字：空元素与巨文的限额', () => {
  const {界面, 文树} = 造控制('<div id="盒"></div>');
  界面('设置文字', '盒', 'a'.repeat(8 * 1024 * 1024));
  assert.throws(() => 界面('设置文字', '盒', 'a'.repeat(8 * 1024 * 1024 + 1)), /超过八 MiB/);
  assert.throws(() => 文树('设置文字', 文树('新建', 'input', '', ''), 'x'), /空元素不能含文字/);
});

// ---- iframe 与模板 ----
const 框页 = '<iframe id="框" sandbox="allow-same-origin allow-forms" referrerpolicy="no-referrer" data-yy-frame-origins="https://usercontent.yuyan-lang.org https://预览.example"></iframe><iframe id="无沙箱"></iframe><div id="盒"></div>';
test('设置页面框架地址：同源路径与页面声明的 https 来源；其余一律拒绝', () => {
  const {文, 文树} = 造控制(框页);
  const 设 = 网址 => 文树('设置框架地址', '框', 网址);
  设('/sites/abc/');
  assert.equal(文.getElementById('框').getAttribute('src'), '/sites/abc/');
  设('https://usercontent.yuyan-lang.org/sites/' + 'a'.repeat(64) + '/?revision=3');
  assert.match(文.getElementById('框').getAttribute('src'), /^https:\/\/usercontent\.yuyan-lang\.org\/sites\/a{64}\/\?revision=3$/);
  设('https://yuyan-lang.org/预览');
  设('');
  assert.equal(文.getElementById('框').hasAttribute('src'), false, '空串清除地址');
  for (const 坏 of ['https://evil.example/', 'http://usercontent.yuyan-lang.org/', '//usercontent.yuyan-lang.org/', 'javascript:alert(1)', 'data:text/html,x', '/\\evil.example', ' /x', '/a b', 'x.html', 'ftp://x', 'https://usercontent.yuyan-lang.org.evil.example/', 'https://usercontent.yuyan-lang.org@evil.example/'])
    assert.throws(() => 设(坏), /框架地址/, 坏);
  assert.throws(() => 文树('设置框架地址', '无沙箱', '/x'), /未声明 sandbox/);
  assert.throws(() => 文树('设置框架地址', '盒', '/x'), /不是 iframe/);
  assert.throws(() => 文树('设置框架地址', '无此', '/x'), /网页元素不存在/);
  assert.throws(() => 设('/' + 'a'.repeat(2050)), /框架地址无效/);
});

const 模板文 = '<!doctype html><html><head><meta charset="utf-8"><link rel="stylesheet" href="/x.css"><script type="module" src="/x.mjs"></script></head><body><main id="市场根" class="市场" onclick="alert(1)"><script>alert(2)</script><style>x{}</style><nav><a id="外链" href="https://yuyan-lang.org/products/" target="_blank" onmouseover="x()">产品</a><a id="坏链" href="  javascript:alert(3)">坏</a></nav><iframe src="/f"></iframe><object data="x"></object><embed src="y"><p id="状态" role="status" srcdoc="x">正在读取</p><form><input name="n" onfocus="x()" formaction="/z"><template><script>t</script></template></form></main></body></html>';
test('装入页面模板：取同源 HTML，按标识选一个元素，剔除脚本与危险属性后装入目标', async () => {
  const 请求们 = [];
  const {文, 文树, 表, 装入} = 造控制('<div id="目标"><p id="旧">旧内容</p></div>', {网络: async (网址, 选项) => { 请求们.push([网址, 选项.redirect, 选项.credentials]); return new Response(模板文, {status: 200}); }});
  const 旧号 = 文树('取得', '旧');
  await 装入('/AI市场模板.html', '市场根', '目标');
  assert.deepEqual(请求们, [['https://yuyan-lang.org/AI%E5%B8%82%E5%9C%BA%E6%A8%A1%E6%9D%BF.html', 'error', 'same-origin']]);
  const 目标 = 文.getElementById('目标');
  assert.equal(目标.children.length, 1);
  const 根 = 目标.firstElementChild;
  assert.equal(根.tagName, 'MAIN');
  assert.equal(根.className, '市场');
  assert.equal(根.hasAttribute('onclick'), false);
  assert.equal(根.querySelector('script, style, iframe, object, embed, template, link, meta'), null);
  assert.equal(文.getElementById('外链').getAttribute('href'), 'https://yuyan-lang.org/products/');
  assert.equal(文.getElementById('外链').getAttribute('rel'), 'noopener noreferrer');
  assert.equal(文.getElementById('外链').hasAttribute('onmouseover'), false);
  assert.equal(文.getElementById('坏链').hasAttribute('href'), false);
  assert.equal(文.getElementById('状态').hasAttribute('srcdoc'), false);
  assert.equal(文.getElementById('状态').getAttribute('role'), 'status');
  assert.equal(根.querySelector('input').hasAttribute('onfocus'), false);
  assert.equal(根.querySelector('input').hasAttribute('formaction'), false);
  assert.equal(根.querySelector('input').getAttribute('name'), 'n');
  assert.equal(文.getElementById('旧'), null, '目标原有子节点被替换');
  assert.throws(() => 文树('设置文字', 旧号, 'x'), /页面节点句柄无效/, '被替换掉的旧子树句柄同时释放');
  assert.equal(表.数量(), 0);
});
