const assert = require('assert');
const {
  isSupportedYuyanSource,
  parseBuildArtifactReport
} = require('../out/buildArtifacts.js');

function runTest(name, callback) {
  callback();
  process.stdout.write(`  ✓ ${name}\n`);
}

process.stdout.write('Yuyan build artifact helpers\n');

runTest('accepts only supported Yuyan source paths', () => {
  assert.strictEqual(isSupportedYuyanSource('/工作区/库/豫章/形制/多态列。豫'), true);
  assert.strictEqual(isSupportedYuyanSource('/工作区/src/example.yuyan'), true);
  assert.strictEqual(isSupportedYuyanSource('src\\example.yyon'), true);
  assert.strictEqual(isSupportedYuyanSource('/工作区/库/豫章/形制/多态列。树码'), false);
  assert.strictEqual(isSupportedYuyanSource('/工作区/README.md'), false);
});

runTest('parses the one-line report printed by 豫构 产物', () => {
  const report = parseBuildArtifactReport(
    JSON.stringify({
      源文件: '/工作区/例/例子。豫',
      包上下文: '/工作区/.豫构缓存/豫构上下文/abc.上下文',
      阶段产物: [
        { 阶段: '检查后定义', 路径: '/tmp/豫构缓存/物/ab/abcdef。树码', 在: true },
        { 阶段: '语言服务', 路径: '/tmp/豫构缓存/物/cd/cdefab。树码', 在: false }
      ]
    })
  );

  assert.notStrictEqual(report, undefined);
  assert.strictEqual(report.sourceFile, '/工作区/例/例子。豫');
  assert.strictEqual(report.packageContext, '/工作区/.豫构缓存/豫构上下文/abc.上下文');
  assert.deepStrictEqual(report.artifacts, [
    { stage: '检查后定义', path: '/tmp/豫构缓存/物/ab/abcdef。树码', present: true },
    { stage: '语言服务', path: '/tmp/豫构缓存/物/cd/cdefab。树码', present: false }
  ]);
});

runTest('skips malformed entries and rejects reports missing required fields', () => {
  const report = parseBuildArtifactReport(
    JSON.stringify({
      源文件: '/工作区/例/例子。豫',
      包上下文: '/工作区/.豫构缓存/豫构上下文/abc.上下文',
      阶段产物: [
        { 阶段: '检查后定义', 路径: '/tmp/甲。树码', 在: true },
        { 阶段: '语言服务', 路径: '/tmp/乙。树码' },
        { 阶段: '抽象语法', 在: true },
        '不是对象'
      ]
    })
  );
  assert.notStrictEqual(report, undefined);
  assert.deepStrictEqual(report.artifacts, [
    { stage: '检查后定义', path: '/tmp/甲。树码', present: true }
  ]);

  assert.strictEqual(parseBuildArtifactReport('not json'), undefined);
  assert.strictEqual(parseBuildArtifactReport('[]'), undefined);
  assert.strictEqual(
    parseBuildArtifactReport(JSON.stringify({ 源文件: '/工作区/例/例子。豫', 阶段产物: [] })),
    undefined
  );
});
