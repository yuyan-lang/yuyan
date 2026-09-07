import {test} from 'node:test';
import assert from 'node:assert/strict';
import {mkdtempSync,mkdirSync,writeFileSync,readFileSync,existsSync} from 'node:fs';
import {join,resolve} from 'node:path';
import {tmpdir} from 'node:os';
import {spawn,execFileSync} from 'node:child_process';
import {createServer} from 'node:http';
import {createHash} from 'node:crypto';
// 文言：假器验次序，真档验所载；不传试物于公网。汉语：工具链替身只测编排，真实生成 ZIP，上传仅使用回环测试服务器。
const binary=resolve('yy批量发布');
function fixture(){
  const base=mkdtempSync(join(tmpdir(),'yy-batch-test-')),root=join(base,'项目 空格'),kit=join(base,'工具链'),out=join(base,'发行');
  for(const p of [root,kit]){mkdirSync(p);execFileSync('git',['init','-q',p]);}
  const write=(p,s,mode=0o644)=>{mkdirSync(join(p,'..'),{recursive:true});writeFileSync(p,s,{mode});};
  write(join(kit,'yy4_bs'),'#!/bin/sh\nexit 0\n',0o755);
  write(join(kit,'yy豫构'),'#!/bin/sh\nwhile [ "$#" -gt 0 ]; do if [ "$1" = "--输出" ]; then shift; printf "binary" > "$1"; chmod +x "$1"; fi; shift; done\n',0o755);
  write(join(kit,'yy文档网站生成器'),'#!/bin/sh\nmkdir -p "$3"\nprintf "<!doctype html><title>测试文档</title>" > "$3/index.html"\n',0o755);
  for(const p of ['LICENSE','运行时支持库/Makefile','运行时支持库/原生/头.h','运行时支持库/库.a','网站/共用/主题.css','网站/共用/界面.js','工具/文档网站生成器/界面.css'])write(join(kit,p),'fixture');
  function pack(name,owner='甲',exe=false){
    const dir=join(root,name);
    write(join(dir,name+'。包。豫'),`「名称」者『${name}』也。「所有者」者『${owner}』也。「版本」者『1.0.0』也。「类型」者「典」【「种类」者『${exe?'可执行文件':'库'}』也，${exe?'「入口」者『入口』也，「文件名」者『yy样例』也，':''}】也。「简介」者『测试』也。「说明」者「典」【「汉语」者『说明.汉语.md』也，「文言」者『说明.文言.md』也，】也。`);
    for(const p of ['说明.汉语.md','说明.文言.md'])write(join(dir,p),'说明');
    write(join(dir,exe?'入口。豫':'总集。豫'),'「数」者「一」也。');return dir;
  }
  function run(args,env={}){
    return new Promise(resolve=>{const p=spawn(binary,[...args,'--工具链',kit,'--输出',out],{cwd:root,env:{...process.env,...env},timeout:60000});let text='';p.stdout.on('data',b=>text+=b);p.stderr.on('data',b=>text+=b);p.on('close',code=>resolve({code,text}));});
  }
  return{root,kit,out,write,pack,run};
}
test('计划无产物，实际 ZIP 包含源码、文档、构建与运行包，并复用完全相同的 ZIP',async()=>{
  const f=fixture();f.pack('样例','甲',true);f.pack('不选','乙');
  let r=await f.run(['--所有者','甲']);assert.equal(r.code,0,r.text);assert.equal(existsSync(f.out),false);
  r=await f.run(['--打包','--所有者','甲']);assert.equal(r.code,0,r.text);
  const zip=join(f.out,'甲/样例/1.0.0/发布.zip'),before=readFileSync(zip);
  let cursor=before.readUInt32LE(before.length-6),names=[];
  while(before.readUInt32LE(cursor)===0x02014b50){const n=before.readUInt16LE(cursor+28),x=before.readUInt16LE(cursor+30),c=before.readUInt16LE(cursor+32);names.push(before.subarray(cursor+46,cursor+46+n).toString('utf8'));cursor+=46+n+x+c;}
  const list=names.join('\n');assert.match(list,/源码\/样例。包。豫/);assert.match(list,/文档\/index.html/);assert.match(list,/构建\/.+tar.gz/);assert.match(list,/运行\/.+tar.gz/);
  r=await f.run(['--打包','--所有者','甲']);assert.equal(r.code,0,r.text);assert.match(r.text,/复用/);assert.deepEqual(readFileSync(zip),before);
});
test('失败不阻断其他包，敏感文件不打入 ZIP，批次退出非零且释放锁',async()=>{
  const f=fixture(),bad=f.pack('坏包');f.pack('好包');f.write(join(bad,'.env'),'secret');
  const r=await f.run(['--打包','--所有者','甲']);assert.equal(r.code,1,r.text);
  assert.match(r.text,/敏感文件/);assert.equal(existsSync(join(f.out,'甲/坏包/1.0.0/发布.zip')),false);
  assert.equal(existsSync(join(f.out,'甲/好包/1.0.0/发布.zip')),true);assert.equal(existsSync(join(f.out,'.发布锁')),false);
});
test('上传使用同一个 ZIP，失败可重试，令牌不出现在输出中',async()=>{
  const f=fixture();f.pack('上传包');const token='a'.repeat(64),bodies=[];let incomplete=true;
  const server=createServer(async(req,res)=>{assert.equal(req.url,'/api/releases/zip');assert.equal(req.headers.authorization,'Bearer '+token);const parts=[];for await(const p of req)parts.push(p);bodies.push(Buffer.concat(parts));res.setHeader('Content-Type','application/json');res.end(JSON.stringify({id:'b'.repeat(32),incomplete}));});
  await new Promise(r=>server.listen(0,'127.0.0.1',r));
  try {
    const args=['--发布','--所有者','甲','--服务','http://127.0.0.1:'+server.address().port];
    let r=await f.run(args,{YY_UPLOAD_TOKEN:token});assert.equal(r.code,1,r.text);assert.equal(r.text.includes(token),false);
    incomplete=false;r=await f.run(args,{YY_UPLOAD_TOKEN:token});assert.equal(r.code,0,r.text);assert.equal(r.text.includes(token),false);
    assert.equal(bodies.length,2);assert.equal(createHash('sha256').update(bodies[0]).digest('hex'),createHash('sha256').update(bodies[1]).digest('hex'));
  }finally{await new Promise(r=>server.close(r));}
});
