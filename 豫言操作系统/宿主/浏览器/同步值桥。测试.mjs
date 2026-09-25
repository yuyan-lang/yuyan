// 文言：无 JSPI 仍可行纯同步客器；异步原语不得伪作同步。汉语：用真实豫言 Wasm 验证无 JSPI 同步运行和异步能力拒绝。
import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {readFile} from 'node:fs/promises';
import {resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {创建豫言实例} from './值桥.mjs';

const 根=resolve(fileURLToPath(new URL('.',import.meta.url)),'../../../../yuyan-cloud/工具/云端项目核心/产物');
const 程序径=resolve(根,'yy项目核心.wasm'),值桥径=resolve(根,'yy值桥接.wasm');
const 可运行=existsSync(程序径)&&existsSync(值桥径);

test('无 JSPI 可运行同步豫言 Wasm，异步能力明确拒绝',{skip:!可运行},async()=>{
  const 程序=await WebAssembly.compile(await readFile(程序径));
  const 值桥=await WebAssembly.compile(await readFile(值桥径));
  const 原悬=WebAssembly.Suspending,原承=WebAssembly.promising;
  const 参数=[JSON.stringify({操作:'校验路径',path:'入口。豫'})];
  try{
    WebAssembly.Suspending=undefined;WebAssembly.promising=undefined;
    let 输出='';
    const 同步=创建豫言实例(程序,值桥,{}, {参数,输出:文=>{输出+=文;}});
    await 同步.运行();
    assert.deepEqual(JSON.parse(输出),{ok:true});
    const 异步=创建豫言实例(程序,值桥,{豫言_打印行:async()=>null},{参数});
    await assert.rejects(异步.运行(),/缺少 JSPI，程序调用了异步宿主能力/u);
  }finally{
    WebAssembly.Suspending=原悬;WebAssembly.promising=原承;
  }
});
test('现有 JSPI 路径仍运行真实豫言 Wasm',{skip:!可运行},async()=>{
  const 程序=await WebAssembly.compile(await readFile(程序径));
  const 值桥=await WebAssembly.compile(await readFile(值桥径));
  let 输出='';
  const 客器=创建豫言实例(程序,值桥,{}, {参数:[JSON.stringify({操作:'校验路径',path:'入口。豫'})],输出:文=>{输出+=文;}});
  await 客器.运行();
  assert.deepEqual(JSON.parse(输出),{ok:true});
});
