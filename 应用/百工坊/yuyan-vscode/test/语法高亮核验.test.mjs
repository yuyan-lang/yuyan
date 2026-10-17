import {test} from 'node:test';
import assert from 'node:assert/strict';
import {读取TextMate语法, TextMate逐字类别, 对照文件} from './语法高亮核验.mjs';
import {删法们, 删空白, 核验删空白} from './删空白核验.mjs';

test('已解析源码中的导入、典字段和类型应用按语义类别着色', async () => {
  const 语法 = await 读取TextMate语法();
  const 源码 = '寻观「标准库」之「语言核心」之书。\n「类型」者「典」【「种类」者『库』也，】也。\n「值」者「列」也（「零」）也。';
  const 结果 = TextMate逐字类别(源码, 语法);
  const 类别 = (片段, 第几次 = 0) => {
    let 起点 = -1;
    for (let i = 0; i <= 第几次; i++) 起点 = 源码.indexOf(片段, 起点 + 1);
    return 结果.类别[[...源码.slice(0, 起点)].length];
  };
  assert.equal(类别('之「语言核心」'), '操作');
  assert.equal(类别('也，'), '结构');
  assert.equal(类别('也（'), '类型');
});

test('数字形绑定名与点字面量按解析器语境区分', async () => {
  const 语法 = await 读取TextMate语法();
  const 源码 = '虑「三」者「三」而虑「点」者「点」';
  const {类别} = TextMate逐字类别(源码, 语法);
  const 位置 = 片段 => [...源码.slice(0, 源码.indexOf(片段))].length;
  assert.equal(类别[位置('「三」')], '标识');
  assert.equal(类别[位置('「三」而')], '数值');
  assert.equal(类别[位置('「点」')], '标识');
  assert.equal(类别[[...源码.slice(0, 源码.lastIndexOf('「点」'))].length], '数值');
});

// 文言：异码元而同字位，标记之界不可漂移。
// 汉语：补充平面字符占两个 UTF-16 码元，解析器范围仍按一个 Unicode 字符比较。
test('解析器字位与 TextMate 码元正确对齐', async () => {
  const 语法 = await 读取TextMate语法();
  const 结果 = 对照文件('𠮷「甲」', [{开始: 1, 结束: 4, 种类: '引用标识符'}], 语法, '例。豫');
  assert.equal(结果.对照字数, 3);
  assert.deepEqual(结果.差异, []);
});

test('保留有位置和原始 scope 的真实类别差异', async () => {
  const 语法 = await 读取TextMate语法();
  const 结果 = 对照文件('甲\n而', [{开始: 2, 结束: 3, 种类: '类型操作符'}], 语法, '例。豫');
  assert.equal(结果.差异.length, 1);
  assert.equal(结果.差异字数, 1);
  assert.deepEqual({行: 结果.差异[0].行, 列: 结果.差异[0].列}, {行: 2, 列: 1});
  assert.equal(结果.差异[0].解析器, '类型');
  assert.ok(结果.差异[0].scopes.includes('source.yuyan'));
});

test('解析器普通操作符中的括号与 TextMate 标点同类', async () => {
  const 语法 = await 读取TextMate语法();
  const 结果 = 对照文件('（「甲」），】', [
    {开始: 0, 结束: 1, 种类: '普通操作符'},
    {开始: 1, 结束: 4, 种类: '引用标识符'},
    {开始: 4, 结束: 5, 种类: '普通操作符'},
    {开始: 5, 结束: 7, 种类: '普通操作符'}
  ], 语法, '例。豫');
  assert.deepEqual(结果.差异, []);
});

test('跨行注释中的换行不误报为缺色', async () => {
  const 语法 = await 读取TextMate语法();
  const 结果 = 对照文件('「：甲\n乙：」', [{开始: 0, 结束: 7, 种类: '注释'}], 语法, '例。豫');
  assert.deepEqual(结果.差异, []);
  assert.equal(结果.对照字数, 6);
});

// 文言：编器不视空白，删白之后诸字当同色；此篇集多行型签、典与组之逗号行首字段、跨行类型判断。
// 汉语：编译器忽略空格与换行，删去它们后每个非空白字应保持原色；样例覆盖多行类型签名、典与组类组值的逗号行首字段、跨行类型判断。
const 多行样例 = [
  '「：注释：」',
  '寻观「标准库」之书。',
  '「类型」者「典」',
  '  【「种类」者『库』也',
  '  ，「入口」者',
  '    若「甲」则『一』否则『二』',
  '    也',
  '  】',
  '也。',
  '「字形库」即',
  '  「组类」',
  '  【「字体们」乃「列」于「字体」也',
  '  ，「前向」乃化（「列」于「整数」）而',
  '    化「整数」而「整数」也',
  '  】',
  '也。',
  '「新建」者会「字体链」而',
  '  「组值」',
  '  【「字体们」者「字体链」也',
  '  ，「读取」者',
  '    （化「整数」而「字节串」也',
  '    会「偏移」而「偏移」）',
  '    也',
  '  】',
  '也。',
  '「两级变换」乃',
  '  承「甲」而',
  '  承「乙」而',
  '  化（「列」于「甲」）而',
  '  （「列」于「乙」）',
  '也。',
  '「结果」者',
  '  虑「值」者（「整数」合「整数」）也',
  '    （「零」与「一」）',
  '  而「值」',
  '也。'
].join('\n');

test('删去空格与换行不改变任何非空白字的颜色', async () => {
  const 语法 = await 读取TextMate语法();
  assert.deepEqual(核验删空白(多行样例, 语法), []);
});

test('多行写法中的关键字仍按语义类别着色', async () => {
  const 语法 = await 读取TextMate语法();
  const {类别} = TextMate逐字类别(多行样例, 语法);
  const 类 = (片段, 偏移, 第几次 = 0) => {
    let 起点 = -1;
    for (let i = 0; i <= 第几次; i++) 起点 = 多行样例.indexOf(片段, 起点 + 1);
    assert.ok(起点 >= 0, `样例中找不到 ${片段}`);
    return 类别[起点 + 偏移];
  };
  assert.equal(类('「种类」者', 4), '结构');
  assert.equal(类('『库』也', 3), '结构');
  assert.equal(类('「入口」者', 4), '结构');
  assert.equal(类('若「甲」', 0), '控制');
  assert.equal(类('『二』\n    也', 8), '结构');
  assert.equal(类('「字体们」乃', 5), '结构');
  assert.equal(类('「字体」也', 4), '结构');
  assert.equal(类('「前向」乃化', 5), '类型');
  assert.equal(类('）而\n    化', 1), '类型');
  assert.equal(类('）而\n    化', 7), '类型');
  assert.equal(类('「整数」而「整数」也', 9), '结构');
  assert.equal(类('「读取」者', 4), '结构');
  assert.equal(类('「字节串」也', 5), '类型');
  assert.equal(类('会「偏移」而', 5), '控制');
  assert.equal(类('）\n    也', 6), '结构');
  assert.equal(类('承「甲」而', 0), '类型');
  assert.equal(类('承「甲」而', 4), '类型');
  assert.equal(类('虑「值」者', 4), '控制');
  assert.equal(类('（「整数」合「整数」）也', 5), '类型');
  assert.equal(类('（「整数」合「整数」）也', 11), '类型');
});

test('删空白时不拼出注释定界符', () => {
  assert.equal(删空白('「 ：甲：」 乙', 删法们.全删), '「 ：甲：」乙');
  assert.equal(删空白('甲 ：\n」', 删法们.全删), '甲：\n」');
  assert.equal(删空白('甲 乙\n丙', 删法们.只删换行), '甲 乙丙');
  assert.equal(删空白('甲 乙\n丙', 删法们.只删空格), '甲乙\n丙');
});
