// 汉语：用受控TTY和字节块验证模式恢复与按键边界，实际终端另验；管道与普通文件另起子进程验证非终端读字节。文言：以受控TTY与字块验复制与键之界，实终端后验；管与常卷另起子程验非终端之读字节。
const test = require('node:test'), assert = require('node:assert/strict');
const 子进程 = require('node:child_process'), 文件系统 = require('node:fs'), 路径 = require('node:path'), 操作系统 = require('node:os'), 读行 = require('node:readline');
const {创建终端输入} = require('./终端输入.mjs');
test('非TTY不进入原始模式',()=>{const 术=创建终端输入({是终端:()=>false});assert.equal(术.进入(),false);assert.equal(术.进入(),false);});
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

test('首键阻塞，Escape尾部暂时非阻塞后恢复，零字节读取才是EOF',()=>{
  const 阻塞=[];let 次数=0;
  const 流={isRaw:false,setRawMode:值=>{流.isRaw=值;},_handle:{setBlocking:值=>阻塞.push(值)}};
  const 术=创建终端输入({是终端:()=>true,造流:()=>流,读:字=>{
    次数++;
    if(次数===1){字[0]=27;return 1;}
    if(次数===2)throw Object.assign(new Error('尚无尾字'),{code:'EAGAIN'});
    return 0;
  }});
  assert.equal(术.进入(),true);
  assert.deepEqual(术.读取(),[true,'\x1b']);
  assert.deepEqual(阻塞,[true,false,true]);
  assert.deepEqual(术.读取(),[false,'']);术.退出();assert.equal(流.isRaw,false);
});

test('首键的非阻塞错误不可冒充EOF',()=>{
  const 术=创建终端输入({是终端:()=>true,造流:()=>({isRaw:false,setRawMode:()=>{},_handle:{setBlocking:()=>{}}}),读:()=>{throw Object.assign(new Error('错误句柄'),{code:'EAGAIN'});}});
  assert.equal(术.进入(),true);assert.throws(()=>术.读取(),{code:'EAGAIN'});术.退出();
});

// 汉语：瞥键（原语 248，2026-10-24，议题 571）不等待地取一键：缓冲里有一键答阳、取走的键不再重出；没有字节答阴，不阻塞；是终端而没进原始模式时答阴。文言：瞥键不候而取一键：缓中有一键答阳，所取之键不复出；无字节答阴而不阻；为终端而未入原始之制则答阴。
test('瞥键不等待地取一键，取走的键不重出',()=>{
  const 阻塞=[], 字节=Buffer.from('ab'); let 位=0;
  const 流={isRaw:false,setRawMode:()=>{},_handle:{setBlocking:值=>阻塞.push(值)}};
  const 术=创建终端输入({是终端:()=>true,造流:()=>流,读:字=>{
    if(位>=字节.length)throw Object.assign(new Error('暂无键'),{code:'EAGAIN'});
    字[0]=字节[位++];return 1;
  }});
  assert.deepEqual(术.瞥键(),[false,''],'未进原始模式时只答无键');
  assert.equal(术.进入(),true);
  assert.deepEqual(术.瞥键(),[true,'a']);
  assert.deepEqual(术.读取(),[true,'b'],'瞥键取走的键不能再出，接着读才是下一键');
  assert.deepEqual(术.瞥键(),[false,''],'没有字节时不等待，答无键');
  assert.ok(阻塞.includes(false)&&阻塞.includes(true),'不等待的读借非阻塞模式并复其阻塞');
  术.退出();
  assert.deepEqual(术.瞥键(),[false,'']);
});

// 汉语：非终端一支（2026-10-24，议题 574）：标准输入是管道或普通文件时「听键」照样阻塞读一字节（读到文件尾答无键），「瞥键」不等待地试读，取走的字节交给调用方（读行编辑存进预听键）。子进程各印一行结果，父进程按序核对。文言：非终端之支（十月二十四日，议五百七十四）：标准入为管或常卷，听键仍阻读一字（至文尽答无键），瞥键不候而试读，所取之字节授之呼者（读行编辑藏于预听键）。子程各印一行，父程依次核之。
const 模块径 = 路径.join(__dirname, '终端输入.mjs');
const 子脚本 = `
const {创建终端输入} = require(${JSON.stringify(模块径)});
const 术 = 创建终端输入();
const 出 = 值 => process.stdout.write(JSON.stringify(值) + '\\n');
出(['进入', 术.进入()]);
出(['瞥一', 术.瞥键()]);
出(['听一', 术.读取()]);
出(['瞥二', 术.瞥键()]);
出(['听二', 术.读取()]);
出(['瞥尽', 术.瞥键()]);
出(['听尽', 术.读取()]);
`;

test('管道：瞥键不等待，听键阻塞读字节，文件尾答无键',{timeout:15000},async t=>{
  const 子 = 子进程.spawn(process.execPath, ['-e', 子脚本], {stdio:['pipe','pipe','inherit']});
  t.after(() => 子.kill());
  const 行队=[]; let 等行=null;
  读行.createInterface({input:子.stdout}).on('line', 行 => {
    const 值 = JSON.parse(行);
    if (等行) { const 解 = 等行; 等行 = null; 解(值); } else 行队.push(值);
  });
  const 下一行 = () => 行队.length ? Promise.resolve(行队.shift()) : new Promise(解 => 等行 = 解);
  assert.deepEqual(await 下一行(), ['进入', false]);
  assert.deepEqual(await 下一行(), ['瞥一', [false,'']], '管道里没有字节时不等待，立刻答无键');
  子.stdin.write('甲乙');
  assert.deepEqual(await 下一行(), ['听一', [true,'甲']], '听键阻塞等到父进程写来的首字节');
  assert.deepEqual(await 下一行(), ['瞥二', [true,'乙']], '管道里已到的字节用瞥键取出，不丢');
  子.stdin.end();
  assert.deepEqual(await 下一行(), ['听二', [false,'']], '管道关闭后听键答无键');
  assert.deepEqual(await 下一行(), ['瞥尽', [false,'']]);
  assert.deepEqual(await 下一行(), ['听尽', [false,'']]);
  assert.equal(await new Promise((解,拒) => { 子.on('exit',解); 子.on('error',拒); }), 0);
});

test('普通文件：瞥键与听键依次取字节，到文件尾答无键',{timeout:15000},()=>{
  const 目录 = 文件系统.mkdtempSync(路径.join(操作系统.tmpdir(), '终端输入-'));
  try {
    const 文径 = 路径.join(目录, '入界.txt');
    文件系统.writeFileSync(文径, '甲乙');
    const 号 = 文件系统.openSync(文径, 'r');
    try {
      const 果 = 子进程.spawnSync(process.execPath, ['-e', 子脚本], {stdio:[号,'pipe','pipe'], encoding:'utf8'});
      assert.equal(果.status, 0, 果.stderr);
      assert.deepEqual(果.stdout.trim().split('\n').map(行 => JSON.parse(行)), [
        ['进入',false],['瞥一',[true,'甲']],['听一',[true,'乙']],
        ['瞥二',[false,'']],['听二',[false,'']],['瞥尽',[false,'']],['听尽',[false,'']]]);
    } finally { 文件系统.closeSync(号); }
  } finally { 文件系统.rmSync(目录, {recursive:true, force:true}); }
});
