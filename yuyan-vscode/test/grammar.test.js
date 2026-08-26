const assert = require('assert');
const fs = require('fs');
const path = require('path');
const oniguruma = require('vscode-oniguruma');
const textmate = require('vscode-textmate');

const grammarPath = path.resolve(__dirname, '..', 'yuyan.tmGrammar.json');

async function loadGrammar() {
  const wasm = fs.readFileSync(require.resolve('vscode-oniguruma/release/onig.wasm'));
  await oniguruma.loadWASM(
    wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength)
  );

  const registry = new textmate.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: patterns => new oniguruma.OnigScanner(patterns),
      createOnigString: value => new oniguruma.OnigString(value)
    }),
    loadGrammar: async scopeName => {
      if (scopeName !== 'source.yuyan') {
        return null;
      }
      return JSON.parse(fs.readFileSync(grammarPath, 'utf8'));
    }
  });

  return registry.loadGrammar('source.yuyan');
}

function tokensWithText(line, result) {
  return result.tokens.map(token => ({
    text: line.slice(token.startIndex, token.endIndex),
    scopes: token.scopes
  }));
}

function findToken(tokens, text) {
  const token = tokens.find(candidate => candidate.text === text);
  assert.ok(token, `Expected a token for ${JSON.stringify(text)} in ${JSON.stringify(tokens)}`);
  return token;
}

function hasScope(token, prefix) {
  return token.scopes.some(scope => scope.startsWith(prefix));
}

async function runTest(name, callback) {
  await callback();
  process.stdout.write(`  ✓ ${name}\n`);
}

async function main() {
  const grammar = await loadGrammar();
  assert.ok(grammar, 'The source.yuyan grammar should load');

  process.stdout.write('Yuyan TextMate grammar\n');

  await runTest('highlights strings and their escape sequences', () => {
    const line = '『第一行「：换行：」第二行』';
    const tokens = tokensWithText(line, grammar.tokenizeLine(line));

    assert.ok(tokens.every(token => hasScope(token, 'string.quoted.other.yuyan')));
    const escapeTokens = tokens.filter(token => hasScope(token, 'constant.character.escape.yuyan'));
    assert.strictEqual(escapeTokens.map(token => token.text).join(''), '「：换行：」');
  });

  await runTest('distinguishes numeric identifiers from ordinary identifiers', () => {
    const line = '「一二点三」「变量1」 123.45';
    const tokens = tokensWithText(line, grammar.tokenizeLine(line));

    assert.ok(hasScope(findToken(tokens, '「一二点三」'), 'constant.numeric.yuyan'));
    assert.ok(tokens.some(token => token.text === '变量1' && hasScope(token, 'variable.other.yuyan')));
    assert.ok(hasScope(findToken(tokens, '123.45'), 'constant.numeric.yuyan'));
  });

  await runTest('tokenizes adjacent syntax in a source statement', () => {
    const line = '寻观「标准库」之书。';
    const tokens = tokensWithText(line, grammar.tokenizeLine(line));

    assert.ok(hasScope(findToken(tokens, '寻观'), 'markup.bold.structure.yuyan'));
    assert.ok(tokens.some(token => token.text === '标准库' && hasScope(token, 'variable.other.yuyan')));
    assert.ok(hasScope(findToken(tokens, '之书'), 'markup.bold.structure.yuyan'));
    assert.ok(hasScope(findToken(tokens, '。'), 'markup.bold.structure.terminator.yuyan'));
  });

  await runTest('keeps keywords inside identifiers unhighlighted', () => {
    const line = '「寻观否则授以也关键字」 包含若否则于也关键字';
    const tokens = tokensWithText(line, grammar.tokenizeLine(line));
    const quotedName = findToken(tokens, '寻观否则授以也关键字');
    const bareName = findToken(tokens, '包含若否则于也关键字');

    for (const token of [quotedName, bareName]) {
      assert.ok(hasScope(token, 'variable.other.yuyan'));
      assert.ok(!token.scopes.some(scope => /^(keyword|markup\.bold|storage)\./.test(scope)));
    }

    const declaration = '包含若否则于也关键字者会「甲」而「甲」也。';
    const declarationTokens = tokensWithText(declaration, grammar.tokenizeLine(declaration));
    const binder = findToken(declarationTokens, '包含若否则于也关键字');
    assert.ok(hasScope(binder, 'variable.other.yuyan'));
    assert.ok(!binder.scopes.some(scope => /^(keyword|markup\.bold|storage)\./.test(scope)));
    assert.ok(hasScope(findToken(declarationTokens, '者'), 'markup.bold.structure.yuyan'));
    assert.ok(hasScope(findToken(declarationTokens, '也'), 'markup.bold.structure.yuyan'));

    const declarationWithMarkerInName = '记者信息者会「甲」而「甲」也。';
    const markerTokens = tokensWithText(
      declarationWithMarkerInName,
      grammar.tokenizeLine(declarationWithMarkerInName)
    );
    const markerBinder = findToken(markerTokens, '记者信息');
    assert.ok(hasScope(markerBinder, 'variable.other.yuyan'));
    assert.ok(!markerBinder.scopes.some(scope => /^(keyword|markup\.bold|storage)\./.test(scope)));
  });

  await runTest('uses the SML highlighting classes for fixed keywords', () => {
    const structureWords = [
      '导入并打开', '导入并导出', '的类型可以是', '的类型是', '其实就是', '的定义是',
      '寻观诵', '寻观', '寻诵', '观诵', '是一种', '结合性', '之书',
      '打开', '导入', '导出', '函数', '立', '乃', '即', '号', '术', '交', '序', '寻', '观', '诵'
    ];
    const typeWords = ['结合', '中的', '化', '承', '从', '到', '自', '合', '之', '的'];
    const expressionWords = [
      '执行如下计算', '实际上是', '遇到了', '得到了', '使用于', '中的第',
      '递归虑', '类型为', '如果是', '参数是', '否则', '授以', '给予', '随后', '如果',
      '那么', '或者', '分析', '递归', '连结', '其实', '会', '遇', '循', '以', '受',
      '虑', '其', '让', '为', '于', '与', '附', '中', '有', '则', '或', '鉴', '若', '也',
      '传', '而', '者', '个', '；'
    ];
    const line = structureWords.concat(typeWords, expressionWords, [
      '《《内建类型：整数》》', '《《内建爻：阳》》', '《《内建函数：整数：相等》》',
      '《《C调用》》名', '《标签》'
    ]).join(' ');
    const tokens = tokensWithText(line, grammar.tokenizeLine(line));

    for (const word of structureWords) {
      assert.ok(hasScope(findToken(tokens, word), 'markup.bold.structure.yuyan'));
    }
    for (const word of typeWords) {
      assert.ok(hasScope(findToken(tokens, word), 'storage.type.function.yuyan'));
    }
    for (const word of expressionWords) {
      assert.ok(findToken(tokens, word).scopes.some(scope => scope.startsWith('keyword.')));
    }
    assert.ok(hasScope(findToken(tokens, '《《内建类型：整数》》'), 'support.type.builtin.yuyan'));
    assert.ok(hasScope(findToken(tokens, '《《内建爻：阳》》'), 'constant.language.builtin.yuyan'));
    assert.ok(hasScope(findToken(tokens, '《《内建函数：整数：相等》》'), 'support.function.builtin.yuyan'));
    assert.ok(hasScope(findToken(tokens, '《《C调用》》名'), 'support.function.builtin.yuyan'));
    assert.ok(tokens.some(token => token.text === '标签' && hasScope(token, 'entity.name.label.yuyan')));
  });

  await runTest('separates adjacent structure, type, and expression keywords', () => {
    const line = '「函数」乃化「参数」而「结果」也。';
    const tokens = tokensWithText(line, grammar.tokenizeLine(line));

    assert.ok(hasScope(findToken(tokens, '乃'), 'markup.bold.structure.yuyan'));
    assert.ok(hasScope(findToken(tokens, '化'), 'storage.type.function.yuyan'));
    assert.ok(findToken(tokens, '而').scopes.some(scope => scope.startsWith('keyword.')));
    assert.ok(hasScope(findToken(tokens, '也'), 'markup.bold.structure.yuyan'));
    assert.ok(hasScope(findToken(tokens, '。'), 'markup.bold.structure.terminator.yuyan'));
  });

  await runTest('highlights punctuation', () => {
    const line = '（甲）【乙】，甲；乙。';
    const tokens = tokensWithText(line, grammar.tokenizeLine(line));

    for (const mark of ['（', '）', '【', '】', '，']) {
      assert.ok(hasScope(findToken(tokens, mark), 'punctuation.'));
    }
    assert.ok(findToken(tokens, '；').scopes.some(scope => scope.startsWith('keyword.')));
    assert.ok(hasScope(findToken(tokens, '。'), 'markup.bold.structure.terminator.yuyan'));
  });

  await runTest('highlights syntax inside nested quoted expressions', () => {
    const line = '「鉴「对象」而有「模式」则「结果」」';
    const tokens = tokensWithText(line, grammar.tokenizeLine(line));

    for (const word of ['鉴', '而', '有', '则']) {
      assert.ok(findToken(tokens, word).scopes.some(scope => scope.startsWith('keyword.')));
    }
    for (const name of ['对象', '模式', '结果']) {
      assert.ok(hasScope(findToken(tokens, name), 'variable.other.yuyan'));
    }
  });

  await runTest('keeps nested multiline comments scoped until the outer terminator', () => {
    const lines = ['「：外层', '「：内层：」', '结束：」「变量」'];
    let ruleStack = textmate.INITIAL;
    const tokenized = [];

    for (const line of lines) {
      const result = grammar.tokenizeLine(line, ruleStack);
      ruleStack = result.ruleStack;
      tokenized.push(tokensWithText(line, result));
    }

    assert.ok(tokenized[1].every(token => hasScope(token, 'comment.block.yuyan')));
    assert.ok(tokenized[2].some(token => token.text === '变量' && hasScope(token, 'variable.other.yuyan')));
    assert.ok(tokenized[2].some(token => token.text === '变量' && !hasScope(token, 'comment.block.yuyan')));
  });
}

main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
