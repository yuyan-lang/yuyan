import {test} from 'node:test';
import assert from 'node:assert/strict';
import {mkdtempSync,mkdirSync,writeFileSync,readFileSync,readdirSync,existsSync} from 'node:fs';
import {join,resolve} from 'node:path';
import {tmpdir} from 'node:os';
import {spawn,execFileSync} from 'node:child_process';
import {createServer} from 'node:http';
import {createHash} from 'node:crypto';
// 文言：假器验次序，真档验所载；不传试物于公网。汉语：工具链替身只测编排，真实生成 ZIP，上传仅使用回环测试服务器。
const binary=resolve('yy批量发布');
test('真实源码浏览器只扫描指定根，语法错误回退纯文本且转义 HTML',()=>{
  const base=mkdtempSync(join(tmpdir(),'yy-source-read-test-')),root=join(base,'源码'),out=join(base,'网页');mkdirSync(root);
  writeFileSync(join(root,'错误。豫'),'<script>错误源码</script>');writeFileSync(join(base,'依赖。豫'),'不应出现在网页中的依赖');
  execFileSync(resolve('yy源码树浏览器'),['构建源码',root,out],{cwd:resolve('.'),timeout:60000,stdio:'pipe'});
  const page=readFileSync(join(out,'index.html'),'utf8');assert.match(page,/&lt;script&gt;/);assert.doesNotMatch(page,/不应出现在网页中的依赖/);
  assert.equal(existsSync(join(out,'主题.css')),true);assert.equal(existsSync(join(out,'语义标记/错误。豫.json')),true);
});
function fixture(){
  const base=mkdtempSync(join(tmpdir(),'yy-batch-test-')),root=join(base,'项目 空格'),kit=join(base,'工具链'),out=join(base,'发行');
  for(const p of [root,kit]){mkdirSync(p);execFileSync('git',['init','-q',p]);}
  const write=(p,s,mode=0o644)=>{mkdirSync(join(p,'..'),{recursive:true});writeFileSync(p,s,{mode});};
  write(join(kit,'yy4_bs'),'#!/bin/sh\nexit 0\n',0o755);
  write(join(kit,'yy豫构'),'#!/bin/sh\nwhile [ "$#" -gt 0 ]; do if [ "$1" = "--输出" ]; then shift; printf "binary" > "$1"; chmod +x "$1"; fi; shift; done\n',0o755);
  write(join(kit,'yy文档网站生成器'),'#!/bin/sh\n[ "$1" = "构建包" ] || exit 9\n[ -f "$2" ] || exit 8\nmkdir -p "$3"\nprintf "<!doctype html><body>测试文档</body>" > "$3/index.html"\n',0o755);
  write(join(kit,'yy源码树浏览器'),'#!/bin/sh\n[ "$1" = "构建源码" ] || exit 9\n[ -d "$2" ] || exit 8\nmkdir -p "$3"\nprintf "<!doctype html><body>源码浏览</body>" > "$3/index.html"\n',0o755);
  for(const p of ['LICENSE','运行时支持库/Makefile','运行时支持库/原生/头.h','运行时支持库/库.a','网站/共用/主题.css','网站/共用/界面.js','工具/文档网站生成器/界面.css','工具/源码树浏览器/资源/界面.css'])write(join(kit,p),'fixture');
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
  assert.match(list,/文档\/接口\/index.html/);assert.match(list,/文档\/源码浏览\/index.html/);assert.doesNotMatch(list,/临时发布文档索引/);
  const extract=p=>execFileSync('unzip',['-p',zip,p],{encoding:'utf8'});
  assert.deepEqual(JSON.parse(extract('文档/生成状态.json')),{'源码浏览':'已生成','API文档':'已生成'});
  assert.deepEqual(JSON.parse(extract('文档/包信息.json')),{dependencies:[],readmes:['说明.汉语.md','说明.文言.md']});
  assert.match(extract('文档/接口/index.html'),/\.\.\/源码浏览\/index.html/);
  assert.match(extract('文档/源码浏览/index.html'),/\.\.\/接口\/index.html/);
  r=await f.run(['--打包','--所有者','甲']);assert.equal(r.code,0,r.text);assert.match(r.text,/复用/);assert.deepEqual(readFileSync(zip),before);
});
test('API 失败仍生成源码与说明，不发布残页，说明转义；源码工具失败仍阻止打包',async()=>{
  const f=fixture(),dir=f.pack('可阅读');
  f.write(join(dir,'说明.汉语.md'),'<script>恶意内容</script>');
  f.write(join(f.kit,'yy文档网站生成器'),'#!/bin/sh\nmkdir -p "$3"\nprintf "partial" > "$3/index.html"\nexit 1\n',0o755);
  let r=await f.run(['--打包','--所有者','甲']);assert.equal(r.code,0,r.text);
  const zip=join(f.out,'甲/可阅读/1.0.0/发布.zip'),extract=p=>execFileSync('unzip',['-p',zip,p],{encoding:'utf8'});
  assert.deepEqual(JSON.parse(extract('文档/生成状态.json')),{'源码浏览':'已生成','API文档':'生成失败'});
  const home=extract('文档/index.html');assert.match(home,/API 文档生成失败/);assert.match(home,/&lt;script&gt;/);assert.doesNotMatch(home,/<script>/);
  assert.throws(()=>extract('文档/接口/index.html'));
  f.pack('不可阅读');f.write(join(f.kit,'yy源码树浏览器'),'#!/bin/sh\nexit 1\n',0o755);
  r=await f.run(['--打包','--所有者','甲','--包','不可阅读']);assert.equal(r.code,1,r.text);
  assert.equal(existsSync(join(f.out,'甲/不可阅读/1.0.0/发布.zip')),false);
});
test('失败不阻断其他包，敏感文件不打入 ZIP，批次退出非零且释放锁',async()=>{
  const f=fixture(),bad=f.pack('坏包');f.pack('好包');f.write(join(bad,'.env'),'secret');
  const r=await f.run(['--打包','--所有者','甲']);assert.equal(r.code,1,r.text);
  assert.match(r.text,/敏感文件/);assert.equal(existsSync(join(f.out,'甲/坏包/1.0.0/发布.zip')),false);
  assert.equal(existsSync(join(f.out,'甲/好包/1.0.0/发布.zip')),true);assert.equal(existsSync(join(f.out,'.发布锁')),false);
});
test('重新打包备份旧档，生成失败保留当前 ZIP，默认重试仍复用',async()=>{
  const f=fixture(),source=f.pack('修订包'),args=['--打包','--所有者','甲'];
  let r=await f.run(args);assert.equal(r.code,0,r.text);
  const dir=join(f.out,'甲/修订包/1.0.0'),path=join(dir,'发布.zip'),old=readFileSync(path);
  f.write(join(source,'说明.汉语.md'),'更新后的说明');r=await f.run([...args,'--重新打包']);assert.equal(r.code,0,r.text);
  const revised=readFileSync(path);assert.notDeepEqual(revised,old);
  const backup=readdirSync(dir).find(x=>x.startsWith('旧档.'));assert.ok(backup);assert.deepEqual(readFileSync(join(dir,backup,'发布.zip')),old);
  f.write(join(f.kit,'yy源码树浏览器'),'#!/bin/sh\nexit 1\n',0o755);
  r=await f.run([...args,'--重新打包']);assert.equal(r.code,1,r.text);assert.deepEqual(readFileSync(path),revised);
  r=await f.run(args);assert.equal(r.code,0,r.text);assert.deepEqual(readFileSync(path),revised);
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
