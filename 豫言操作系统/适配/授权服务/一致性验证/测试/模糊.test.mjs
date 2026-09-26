// 随机差分：任意字节流、任意切块、任意最大字节，适配所出文字必须等于 TextDecoder 的整体解码，且每块不超限、不含半个字符。
import {test, after} from 'node:test';
import assert from 'node:assert/strict';
import {编, 造流, 服务桩, 跑, 造宿主} from './桩.mjs';

// 保活：让期限计时器（AbortSignal.timeout 的定时器不占事件循环）在等待期间不致令进程提前退出。
const 保活 = setInterval(() => {}, 1000);
after(() => clearInterval(保活));
const 种子器 = 种 => () => { 种 |= 0; 种 = 种 + 0x6D2B79F5 | 0; let t = Math.imul(种 ^ 种 >>> 15, 1 | 种); t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t; return ((t ^ t >>> 14) >>> 0) / 4294967296; };
const 造字节 = (随机) => {
  const 片们 = []; const 段数 = 1 + Math.floor(随机() * 12);
  for (let i = 0; i < 段数; i++) {
    const 种类 = Math.floor(随机() * 6);
    if (种类 === 0) 片们.push(编.encode('abc xyz_' + Math.floor(随机() * 1000)));
    else if (种类 === 1) 片们.push(编.encode('豫言云工'.repeat(1 + Math.floor(随机() * 30))));
    else if (种类 === 2) 片们.push(编.encode('😀🚀'.repeat(1 + Math.floor(随机() * 10))));
    else if (种类 === 3) 片们.push(Uint8Array.from({length: 1 + Math.floor(随机() * 6)}, () => Math.floor(随机() * 256)));   // 任意字节，多为非法序列
    else if (种类 === 4) 片们.push(Uint8Array.of(0xE8, 0xB1));                                                                     // 残缺序列
    else 片们.push(编.encode('\r\n\t"\\'));
  }
  if (随机() < 0.2) 片们.unshift(Uint8Array.of(0xEF, 0xBB, 0xBF));
  const 总 = 片们.reduce((n, 片) => n + 片.length, 0); const 果 = new Uint8Array(总); let 位 = 0; for (const 片 of 片们) { 果.set(片, 位); 位 += 片.length; } return 果;
};
const 随机切 = (字节, 随机) => { const 块们 = []; let 位 = 0; while (位 < 字节.length) { const 宽 = 随机() < 0.1 ? 0 : 1 + Math.floor(随机() ** 2 * 200); 块们.push(字节.slice(位, 位 + 宽)); 位 += 宽; } return 块们; };
const 可见 = 文 => 文.replaceAll('\r', '<CR>').replaceAll('\n', '<LF>');

test('随机差分 300 例：块读取与有限读取皆等于整体解码', async () => {
  const 宿主 = 造宿主();
  for (let 例 = 0; 例 < 300; 例++) {
    const 随机 = 种子器(例 + 1);
    const 字节 = 造字节(随机); const 块们 = 随机切(字节, 随机).filter(块 => 块.length > 0);
    const 最大 = [1, 2, 3, 4, 5, 8, 16, 33, 100, 65536][Math.floor(随机() * 10)];
    const s = new 服务桩((路径, 请求, 桩) => new Response(造流(块们, {间隔: 0, 观察: {}}), {status: 200}));
    const 期 = 可见(new TextDecoder().decode(字节));
    const 甲 = await 跑({op: 'svc', url: 'https://svc/rnd', read: 'chunks', max: 最大, timeout: 5000}, {SVC: s}, 宿主);
    assert.equal(甲.状态, 200, `例${例}`);
    const 块文们 = [...甲.文.matchAll(/〔([^〕]*)〕/g)].map(m => m[1]);
    assert.equal(块文们.join(''), 期, `例${例} max=${最大}`);
    assert.match(甲.文, /‖状态1:$/, `例${例}`);
    // 每块（还原可见记号后）不超过 max(最大,4) 个 UTF-8 字节
    for (const 块 of 块文们) assert.ok(编.encode(块.replaceAll('<CR>', '\r').replaceAll('<LF>', '\n')).length <= Math.max(最大, 4), `例${例} 块过长`);
    const 乙 = await 跑({op: 'svc', url: 'https://svc/rnd', read: 'limited', limit: 100000, timeout: 5000}, {SVC: new 服务桩((路径, 请求, 桩) => new Response(造流(块们, {间隔: 0, 观察: {}}), {status: 200}))}, 宿主);
    assert.equal(乙.文.replace(/^状态码=200‖头=‖有限状态0:字节\d+:/, ''), 期, `例${例} 有限`);
  }
});
