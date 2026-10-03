// 文言：客之所书逾管之容，父以管受之，亦当尽得。汉语：客体经节点宿主写出超过管道容量（64KB）的标准输出与标准错误，父进程经管道读取时应完整收到，不得截断。
'use strict';
const 文件=require('node:fs'),路径=require('node:path'),系统=require('node:os'),{spawnSync}=require('node:child_process'),断言=require('node:assert/strict');
const 宿主=路径.resolve(__dirname,'../宿主.cjs');
// 文言：自书最小之 WasmGC 模，以一段字节调标准库之带型导入一次。汉语：手写最小的 WasmGC 模块字节：以一段数据为参数调用一次标准库的带类型导入（签名 串→元），不依赖任何组装工具。
const 字=(...部)=>Buffer.concat(部.map(部=>Buffer.isBuffer(部)?部:Buffer.from(部)));
const 无号=数=>{const 列=[];do{let 低=数&0x7f;数>>>=7;if(数)低|=0x80;列.push(低);}while(数);return 列;};
const 有号=数=>{const 列=[];for(;;){const 低=数&0x7f;数>>=7;if((数===0&&!(低&0x40))||(数===-1&&(低&0x40))){列.push(低);return 列;}列.push(低|0x80);}};
const 向量=(...项)=>字(无号(项.length),...项);
const 名=文=>{const 字节=Buffer.from(文);return 字(无号(字节.length),字节);};
const 段=(号,...内容)=>{const 体=字(...内容);return 字([号],无号(体.length),体);};
// 文言：类型：〇字节之列，一导入之型，二启之型；_start 以一段数据造串而调之；末附边界之段。汉语：类型 0 为字节数组、1 为导入的签名（参数非空的字节数组，无结果）、2 为 _start 的签名；_start 用一段被动数据造出字节串后调用导入；末尾附「豫言边界」自定义段登记签名。
function 探针(字段,正文){
  const 体=字([0x00],
    [0x41,...有号(0),0x41,...有号(正文.length),0xfb,0x09,0x00,0x00],
    [0x10,0x00,0x0b]);
  return 字([0x00,0x61,0x73,0x6d,0x01,0x00,0x00,0x00],
    段(1,向量(字([0x5e,0x78,0x01]),字([0x60,0x01,0x64,0x00,0x00]),字([0x60,0x00,0x00]))),
    段(2,向量(字(名('标准库'),名(字段),[0x00,0x01]))),
    段(3,向量(字([0x02]))),
    段(7,向量(字(名('_start'),[0x00,0x01]))),
    段(12,无号(1)),
    段(10,向量(字(无号(体.length),体))),
    段(11,向量(字([0x01],无号(正文.length),正文))),
    段(0,名('豫言边界'),Buffer.from(`导入\t标准库\t${字段}\t串→元\n`)));
}
const 临时=文件.mkdtempSync(路径.join(系统.tmpdir(),'yy管道输出验-'));
try{
  const 正文=Buffer.from('首😀\u0000尾\n'.repeat(40000));
  for(const [字段,号,期望] of [['打印字符串',1,正文],['标准错误打印行',2,字(正文,[0x0a])]]){
    const 模块=路径.join(临时,'yy管道探针.wasm');
    文件.writeFileSync(模块,探针(字段,正文));
    const 果=spawnSync(process.execPath,[宿主,模块],{maxBuffer:1<<26});
    断言.equal(果.status,0,果.stderr.toString().slice(-2000));
    const 所得=号===1?果.stdout:果.stderr;
    断言.equal(所得.length,期望.length,`${字段} 经管道只收到 ${所得.length} 字节`);
    断言.deepEqual(所得,期望);
    process.stdout.write(`${字段} 经管道完整收到 ${所得.length} 字节\n`);
  }
}finally{文件.rmSync(临时,{recursive:true,force:true});}
