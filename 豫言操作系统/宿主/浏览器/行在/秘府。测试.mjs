// 文言：秘府之块写与当存之径（提案〇〇〇四三议四）：较旧新之文得所改之区；有同步之句柄则按位书，否则可写之流按位书而截其长。
// 汉语：秘府的按块（区段）写回与哪些路径要存（提案 00043 议四）：比较旧新内容得出改动的区段；有同步访问句柄就按位置写，
//   没有就用可写流按位置写再截到当前长度；共享编译缓存根存进秘府，快照树与 /tmp 下的其余路径不存。
import test from 'node:test';
import assert from 'node:assert/strict';
import {改区, 当存, 缓存根, 句柄书区, 流书区} from './秘府.mjs';

const 编码器 = new TextEncoder();
const 编码 = 文 => 编码器.encode(文);

// 文言：假之文柄：其文随写而改，记可写流之开阖；同步之句柄可用时另给 createSyncAccessHandle。汉语：假的文件句柄：内容随写而变，
//   记下可写流是否关闭；给 同步 时另有 createSyncAccessHandle 可用。
function 假文件柄(旧, 同步 = false) {
  const 柄 = {文: 旧.slice(), 已关: false};
  if (同步) 柄.createSyncAccessHandle = async () => ({
    write(数据, {at}) { const 新 = new Uint8Array(Math.max(柄.文.length, at + 数据.length)); 新.set(柄.文); 新.set(数据, at); 柄.文 = 新; return 数据.length; },
    truncate(长) { const 新 = new Uint8Array(长); 新.set(柄.文.subarray(0, Math.min(长, 柄.文.length))); 柄.文 = 新; },
    flush() {},
    close() { 柄.已关 = true; },
  });
  柄.createWritable = async ({keepExistingData} = {}) => {
    if (!keepExistingData) 柄.文 = new Uint8Array(0);
    return {
      async write({position, data}) {
        const 新 = new Uint8Array(Math.max(柄.文.length, position + data.length));
        新.set(柄.文); 新.set(data, position); 柄.文 = 新;
      },
      async truncate(长) { const 新 = new Uint8Array(长); 新.set(柄.文.subarray(0, Math.min(长, 柄.文.length))); 柄.文 = 新; },
      async close() { 柄.已关 = true; },
      async abort() { 柄.已关 = true; },
    };
  };
  return 柄;
}

test('改区取改动的一区，文同则无', () => {
  assert.deepEqual(改区(null, 编码('甲乙')), [0, 6], '新文件整篇都是改动');
  assert.equal(改区(编码('甲乙'), 编码('甲乙')), null, '内容相同不写');
  assert.deepEqual(改区(编码('甲乙'), 编码('甲字')), [3, 6], '只改末字');
  assert.deepEqual(改区(编码('甲'), 编码('甲乙丙')), [3, 9], '接在末尾只写新添的部分');
  assert.deepEqual(改区(编码('甲乙丙'), 编码('甲')), [3, 3], '截短不必写，只截其长');
  assert.deepEqual(改区(编码('甲乙丙'), new Uint8Array(0)), [0, 0], '清空只截其长');
  assert.deepEqual(改区(编码('甲乙丙'), 编码('丙乙甲')), [0, 9], '首尾互换则整篇都是改动');
  assert.deepEqual(改区(编码('甲乙丙丁'), 编码('甲乙X丁')), [6, 10], '长度变了就从改动处写到新末');
  assert.deepEqual(改区(编码('甲乙丙丁'), 编码('甲乙戊丁')), [6, 9], '等长时中段改动前后各留');
});

test('当存：快照树与暂用树之外，共享编译缓存根存下来', () => {
  for (const 径 of ['/数据', '/数据/甲.txt', '/系统', '/系统/开机自启', '/软件', '/桌面设置.数据库'])
    assert.equal(当存(径), true, 径 + ' 应当存');
  for (const 径 of ['/', '/源', '/源/库/豫章', '/工具', '/工具/豫构。豫行', '/tmp', '/tmp/别的', '/tmp/豫构缓存甲'])
    assert.equal(当存(径), false, 径 + ' 不应当存');
  assert.equal(缓存根, '/tmp/豫构缓存');
  assert.equal(当存(缓存根), true, '共享编译缓存根要存');
  assert.equal(当存(缓存根 + '/七c七a/键/甲'), true, '缓存根下的缓存要存');
});

test('流书区只写改动的区段再截短', async () => {
  const 原 = 编码('甲乙丙丁戊'), 新 = 编码('甲乙X丁戊');
  const 柄 = 假文件柄(原);
  const [起, 止] = 改区(原, 新);
  await 流书区(柄, 新, 起, 止);
  assert.deepEqual(柄.文, 新, '只换中间一字');
  assert.equal(柄.已关, true, '可写流已关闭');
  const 短柄 = 假文件柄(原), 短 = 编码('甲乙');
  await 流书区(短柄, 短, ...改区(原, 短));
  assert.deepEqual(短柄.文, 短, '截到当前长度');
  const 长柄 = 假文件柄(编码('甲')), 长 = 编码('甲乙丙');
  await 流书区(长柄, 长, ...改区(编码('甲'), 长));
  assert.deepEqual(长柄.文, 长, '接在末尾补上新添的');
});

test('有同步访问句柄时按位置写并截短', async () => {
  const 原 = 编码('甲乙丙丁戊'), 新 = 编码('甲乙X丁戊');
  const 柄 = 假文件柄(原, true);
  assert.equal(await 句柄书区(柄, 新, ...改区(原, 新)), true, '用上了同步访问句柄');
  assert.deepEqual(柄.文, 新, '只换中间一字');
  assert.equal(柄.已关, true, '同步访问句柄已关闭');
  const 短柄 = 假文件柄(原, true), 短 = 编码('甲乙');
  await 句柄书区(短柄, 短, ...改区(原, 短));
  assert.deepEqual(短柄.文, 短, '截到当前长度');
  const 长柄 = 假文件柄(编码('甲'), true), 长 = 编码('甲乙丙');
  await 句柄书区(长柄, 长, ...改区(编码('甲'), 长));
  assert.deepEqual(长柄.文, 长, '接在末尾补上新添的');
});

test('没有同步访问句柄时交还可写流', async () => {
  const 柄 = 假文件柄(编码('甲'));
  assert.equal(await 句柄书区(柄, 编码('甲乙'), 3, 6), false, '没有同步访问句柄时报假');
  assert.deepEqual(柄.文, 编码('甲'), '没有动文件');
});
