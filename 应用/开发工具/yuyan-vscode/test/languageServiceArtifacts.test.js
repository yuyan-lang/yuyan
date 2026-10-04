const assert = require('assert');
const path = require('path');
const { execFileSync } = require('child_process');
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
  const document = parseLanguageServiceDocument({
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
  });

  assert.ok(document);
  assert.strictEqual(document.信息.length, 2);
  assert.strictEqual(document.信息[0].种类, '定义');
  assert.strictEqual(document.信息[1].内容, '类型：整数');
});

runTest('rejects the removed English token schema', () => {
  assert.strictEqual(parseLanguageServiceDocument([{
    text: '名称',
    extent: {
      file: '/项目/例子。豫',
      start_line: 2,
      start_col: 3,
      end_line: 2,
      end_col: 7
    },
    detail: { type: 'Hover', content: '类型：整数' }
  }]), undefined);
});

runTest('maps source paths to the Chinese artifact stage', () => {
  assert.strictEqual(
    languageServiceArtifactPath('库/标准库/例子。豫'),
    '库/标准库/例子.语言服务.树码'
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

if (process.env.YY_EDITOR_WASM_ROOT) {
  runTest('reads real language and semantic artifacts through Yuyan Wasm', () => {
    const root = process.env.YY_EDITOR_WASM_ROOT;
    const read = kind => JSON.parse(execFileSync(process.execPath, [
      path.join(root, '豫言操作系统/宿主/节点/宿主.cjs'),
      path.join(root, 'yy树码.wasm'),
      '读取编辑器资料',
      path.join(root, `yy编辑器.${kind}.树码`)
    ], { cwd: root, encoding: 'utf8', maxBuffer: 128 * 1024 * 1024 }));
    const language = read('语言服务');
    const document = parseLanguageServiceDocument(language);
    assert.ok(document);
    assert.strictEqual(document.信息[1].内容, '类型：整数\n豫');
    assert.deepStrictEqual(language.附加, [null, true, false, 3.25, -1]);
    const semantic = read('语义标记');
    assert.strictEqual(semantic.版本, 1);
    assert.strictEqual(semantic.标记[0].结束, 2);
  });
}
