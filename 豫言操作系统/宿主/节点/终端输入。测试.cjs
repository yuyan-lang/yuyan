// 汉语：用受控TTY和字节块验证模式恢复与按键边界，实际终端另验。文言：以受控TTY与字块验复制与键之界，实终端后验。
const test = require('node:test'), assert = require('node:assert/strict');
const {创建终端输入} = require('./终端输入.mjs');
test('非TTY不进入原始模式',()=>{const 术=创建终端输入({是终端:()=>false});assert.equal(术.进入(),false);assert.deepEqual(术.读取(),[false,'']);});
test('恢复原模式，分离中文与保留方向键',()=>{
  const 模式=[]; const 块们=[Buffer.from('甲乙\x1b[A')];
  const 术=创建终端输入({是终端:()=>true,造流:()=>({isRaw:false,setRawMode:值=>模式.push(值)}),读:字=>{const 块=块们[0];if(!块)return 0;const 数=Math.min(块.length,字.length);块.copy(字,0,0,数);if(数===块.length)块们.shift();else 块们[0]=块.subarray(数);return 数;}});
  assert.equal(术.进入(),true);assert.equal(术.进入(),true);
  assert.deepEqual(术.读取(),[true,'甲']);assert.deepEqual(术.读取(),[true,'乙']);assert.deepEqual(术.读取(),[true,'\x1b[A']);
  术.退出();assert.deepEqual(模式,[true,false]);assert.deepEqual(术.读取(),[false,'']);
});

test('只取当前按键，子程序继承期间恢复模式，返回后继续父输入',()=>{
  const 模式=[], 输入=Buffer.from('甲\n子\n乙');let 位置=0;
  const 流={isRaw:false,setRawMode:值=>{流.isRaw=值;模式.push(值);}};
  const 术=创建终端输入({是终端:()=>true,造流:()=>流,读:字=>{if(位置>=输入.length)return 0;const 数=Math.min(字.length,输入.length-位置);输入.copy(字,0,位置,位置+数);位置+=数;return 数;}});
  术.进入();
  assert.deepEqual(术.读取(),[true,'甲']);assert.deepEqual(术.读取(),[true,'\n']);
  assert.equal(位置,Buffer.byteLength('甲\n'),'父端不能预读子输入');
  const 恢复=术.暂停();assert.equal(流.isRaw,false);
  assert.equal(输入.subarray(位置,位置+Buffer.byteLength('子\n')).toString(),'子\n');
  位置+=Buffer.byteLength('子\n');恢复();assert.equal(流.isRaw,true);
  assert.deepEqual(术.读取(),[true,'乙']);术.退出();
  assert.deepEqual(模式,[true,false,true,false]);
});
