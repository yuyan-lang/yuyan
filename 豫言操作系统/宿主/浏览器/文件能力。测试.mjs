// 汉语：目录句柄替身验证宿主原语协议，Blob使用实际实现；真实浏览器存储与豫言适配另验。文言：以目录柄之代验原语之约，物字用实术；实浏览器储存与豫言适配后验。
import test from 'node:test';
import assert from 'node:assert/strict';
import {创建文件能力} from './文件能力.mjs';

const 文件 = 文 => ({kind: 'file', getFile: async () => new Blob([文])});
const 目录 = 项们 => ({
  kind: 'directory',
  async *entries() { yield* Object.entries(项们); },
  async getDirectoryHandle(名, 选) {
    assert.equal(选.create, false);
    if (!项们[名]) throw new DOMException('不存在', 'NotFoundError');
    if (项们[名].kind !== 'directory') throw new DOMException('非目录', 'TypeMismatchError');
    return 项们[名];
  },
  async getFileHandle(名, 选) {
    assert.equal(选.create, false);
    if (!项们[名]) throw new DOMException('不存在', 'NotFoundError');
    if (项们[名].kind !== 'file') throw new DOMException('非文件', 'TypeMismatchError');
    return 项们[名];
  },
});
const 新能力 = () => 创建文件能力({目录: new Map([['桌面', 目录({'说明.txt': 文件('豫言'), '子目录': 目录({'子.txt': 文件('甲乙')})})]])});

test('目录列举、子路径与文件信息复用原生句柄', async () => {
  const 能力 = 新能力();
  const [码, 根] = await 能力.取得目录('桌面');
  assert.equal(码, 0);
  assert.deepEqual(await 能力.列目录(根, ''), [0, [['子目录', 1], ['说明.txt', 0]], '']);
  assert.deepEqual(await 能力.列目录(根, '子目录'), [0, [['子.txt', 0]], '']);
  assert.deepEqual(await 能力.查询信息(根, '说明.txt'), [0, 0, 6, '']);
  assert.deepEqual(await 能力.查询信息(根, '子目录'), [0, 1, 0, '']);
});
test('字节分块、读尽、关闭与清理失效', async () => {
  const 能力 = 新能力();
  const [, 根] = await 能力.取得目录('桌面');
  const [, 柄] = await 能力.打开(根, '子目录/子.txt', false);
  const [, 甲] = await 能力.读取(柄, 2);
  const [, 乙] = await 能力.读取(柄, 20);
  assert.equal(new TextDecoder().decode(Uint8Array.from([...甲, ...乙])), '甲乙');
  assert.equal((await 能力.读取(柄, 1))[0], 9);
  assert.equal((await 能力.关闭(柄))[0], 0);
  assert.equal((await 能力.读取(柄, 1))[0], 3);
  能力.清理();
  assert.equal((await 能力.列目录(根, ''))[0], 3);
});
test('未授目录、越界路径、不存在与只读失败有明确状态', async () => {
  const 能力 = 新能力();
  assert.equal((await 能力.取得目录('未授'))[0], 1);
  const [, 根] = await 能力.取得目录('桌面');
  for (const 路 of ['../外部', '/绝对', '子目录/../说明.txt', '子目录//子.txt']) assert.equal((await 能力.打开(根, 路, false))[0], 7);
  assert.equal((await 能力.打开(根, '缺失', false))[0], 4);
  assert.equal((await 能力.打开(根, '说明.txt', true))[0], 1);
  assert.equal((await 能力.写入('伪号', new Uint8Array()))[0], 1);
  assert.equal((await 能力.创建目录(根, '新目录'))[0], 1);
  assert.equal((await 能力.创建目录(根, '../越界'))[0], 7);
  assert.equal((await 能力.创建目录('伪号', '目录'))[0], 3);
  assert.equal((await 能力.开启写入(根, '新文件', false))[0], 1);
  assert.equal((await 能力.开启写入(根, '../越界', true))[0], 7);
  assert.equal((await 能力.开启写入('伪号', '文件', false))[0], 3);
});
