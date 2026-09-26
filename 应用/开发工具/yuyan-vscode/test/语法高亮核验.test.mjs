import {test} from 'node:test';
import assert from 'node:assert/strict';
import {读取TextMate语法, TextMate逐字类别, 对照文件} from './语法高亮核验.mjs';

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
