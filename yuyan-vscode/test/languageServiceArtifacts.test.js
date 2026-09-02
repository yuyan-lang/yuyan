const assert = require('assert');
const {
  languageServiceArtifactPath,
  parseLanguageServiceDocument,
  positionIsInRange,
  selectNarrowestInfo
} = require('../out/languageServiceArtifacts.js');

function runTest(name, callback) {
  callback();
  process.stdout.write(`  ✓ ${name}\n`);
}

const sourceRange = {
  文件: '/项目/例子。豫',
  开始行: 2,
  开始列: 3,
  结束行: 2,
  结束列: 7
};

process.stdout.write('Yuyan language service artifact helpers\n');

runTest('parses the Chinese language service schema', () => {
  const document = parseLanguageServiceDocument(JSON.stringify({
    版本: 1,
    源文件: '/项目/例子。豫',
    信息: [
      {
        种类: '定义',
        范围: sourceRange,
        目标: { ...sourceRange, 开始行: 0, 结束行: 0 }
      },
      {
        种类: '悬停',
        范围: sourceRange,
        内容: '类型：整数'
      }
    ]
  }));

  assert.ok(document);
  assert.strictEqual(document.信息.length, 2);
  assert.strictEqual(document.信息[0].种类, '定义');
  assert.strictEqual(document.信息[1].内容, '类型：整数');
});

runTest('rejects the removed English token schema', () => {
  assert.strictEqual(parseLanguageServiceDocument(JSON.stringify([{
    text: '名称',
    extent: {
      file: '/项目/例子。豫',
      start_line: 2,
      start_col: 3,
      end_line: 2,
      end_col: 7
    },
    detail: { type: 'Hover', content: '类型：整数' }
  }])), undefined);
});

runTest('maps source paths to the Chinese artifact stage', () => {
  assert.strictEqual(
    languageServiceArtifactPath('藏书阁/标准库/例子。豫'),
    '藏书阁/标准库/例子.语言服务.json'
  );
  assert.strictEqual(languageServiceArtifactPath('../例子。豫'), undefined);
});

runTest('uses half-open source ranges and selects the narrowest match', () => {
  assert.strictEqual(positionIsInRange(2, 3, sourceRange), true);
  assert.strictEqual(positionIsInRange(2, 6, sourceRange), true);
  assert.strictEqual(positionIsInRange(2, 7, sourceRange), false);

  const wide = { 种类: '悬停', 范围: { ...sourceRange, 开始列: 1, 结束列: 9 }, 内容: '宽' };
  const narrow = { 种类: '悬停', 范围: sourceRange, 内容: '窄' };
  assert.strictEqual(selectNarrowestInfo([wide, narrow], 2, 4), narrow);
});
