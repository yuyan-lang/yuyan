const assert = require('assert');
const { 加载语法, 高亮源码, 高亮网页 } = require('./生成源码高亮');

async function 主函数() {
  const 语法 = await 加载语法();
  const 源码 = [
    '寻观「标准库」之书。',
    '「函数」乃化「参数」而「结果」也。',
    '「函数」于「参数」',
    '若「条件」则「结果」否则「其他」',
    '包含若否则于也关键字'
  ].join('\n');
  const 结果 = 高亮源码(源码, 语法);

  assert.match(结果, /tok-structure">寻观</);
  assert.match(结果, /tok-structure">乃</);
  assert.match(结果, /tok-type">化</);
  assert.match(结果, /tok-type">而</);
  assert.match(结果, /tok-operator">于</);
  assert.match(结果, /tok-control">若</);
  assert.match(结果, /tok-identifier">包含若否则于也关键字</);

  const 网页结果 = await 高亮网页(`<code data-language="yuyan">${源码}</code>`);
  assert.strictEqual(网页结果.数量, 1);
  assert.match(网页结果.网页, /class="tok-structure"/);
  process.stdout.write('源码树 TextMate 高亮测试通过\n');
}

主函数().catch(错误 => {
  process.stderr.write(`${错误.stack || 错误}\n`);
  process.exitCode = 1;
});
