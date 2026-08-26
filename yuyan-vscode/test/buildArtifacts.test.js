const assert = require('assert');
const {
  jsonArtifactStage,
  sortBuildCachesNewestFirst,
  sourceRelativePathToArtifactStem
} = require('../out/buildArtifacts.js');

function runTest(name, callback) {
  callback();
  process.stdout.write(`  ✓ ${name}\n`);
}

process.stdout.write('Yuyan build artifact helpers\n');

runTest('maps supported source paths to artifact stems', () => {
  assert.strictEqual(
    sourceRelativePathToArtifactStem('藏书阁/标准库/数据结构/多态列。豫'),
    '藏书阁/标准库/数据结构/多态列'
  );
  assert.strictEqual(sourceRelativePathToArtifactStem('src/example.yuyan'), 'src/example');
  assert.strictEqual(sourceRelativePathToArtifactStem('src\\example.yyon'), 'src/example');
});

runTest('rejects unsupported or outside-workspace source paths', () => {
  assert.strictEqual(sourceRelativePathToArtifactStem('src/example.json'), undefined);
  assert.strictEqual(sourceRelativePathToArtifactStem('../example。豫'), undefined);
  assert.strictEqual(sourceRelativePathToArtifactStem('/src/example。豫'), undefined);
});

runTest('extracts only matching JSON artifact stages', () => {
  assert.strictEqual(jsonArtifactStage('多态列.抽象语法.json', '多态列'), '抽象语法');
  assert.strictEqual(jsonArtifactStage('多态列.文件依赖.json', '多态列'), '文件依赖');
  assert.strictEqual(jsonArtifactStage('别的文件.抽象语法.json', '多态列'), undefined);
  assert.strictEqual(jsonArtifactStage('多态列.opt.bc', '多态列'), undefined);
  assert.strictEqual(jsonArtifactStage('多态列.json', '多态列'), undefined);
});

runTest('sorts build caches newest first without mutating the input', () => {
  const caches = [
    { name: '__yy2+2.nosync', mtime: 20 },
    { name: '__yy3+3.nosync', mtime: 30 },
    { name: '__yy1+1.nosync', mtime: 10 }
  ];
  const sorted = sortBuildCachesNewestFirst(caches);

  assert.deepStrictEqual(sorted.map(cache => cache.name), [
    '__yy3+3.nosync',
    '__yy2+2.nosync',
    '__yy1+1.nosync'
  ]);
  assert.deepStrictEqual(caches.map(cache => cache.name), [
    '__yy2+2.nosync',
    '__yy3+3.nosync',
    '__yy1+1.nosync'
  ]);
});
