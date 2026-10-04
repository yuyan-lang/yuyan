// 汉语：用受控TTY和字节块验证模式恢复与按键边界，实际终端另验。文言：以受控TTY与字块验复制与键之界，实终端后验。
const test = require('node:test'), assert = require('node:assert/strict');
const {创建终端输入} = require('./终端输入.cjs');
test('非TTY不进入原始模式',()=>{const 术=创建终端输入({是终端:()=>false});assert.equal(术.进入(),false);assert.deepEqual(术.读取(),[false,'']);});
test('恢复原模式，分离中文与保留方向键',()=>{
  const 模式=[]; const 块们=[Buffer.from('甲乙\x1b[A')];
  const 术=创建终端输入({是终端:()=>true,造流:()=>({isRaw:false,setRawMode:值=>模式.push(值)}),读:字=>{const 块=块们.shift();if(!块)return 0;块.copy(字);return 块.length;}});
  assert.equal(术.进入(),true);assert.equal(术.进入(),true);
  assert.deepEqual(术.读取(),[true,'甲']);assert.deepEqual(术.读取(),[true,'乙']);assert.deepEqual(术.读取(),[true,'\x1b[A']);
  术.退出();assert.deepEqual(模式,[true,false]);assert.deepEqual(术.读取(),[false,'']);
});
