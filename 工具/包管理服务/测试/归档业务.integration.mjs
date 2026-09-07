import {test} from 'node:test';
import assert from 'node:assert/strict';
import {DatabaseSync} from 'node:sqlite';
import {mkdtempSync,readFileSync,writeFileSync,readdirSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join,resolve} from 'node:path';
import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {deflateRawSync} from 'node:zlib';

// 文言：以真器试真档，伪主与坏档不得立籍。汉语：测试原生 ZIP、豫言 parser 和数据库约束，不执行上传内容。
function crc(bytes){let c=0xffffffff;for(const b of bytes){c^=b;for(let i=0;i<8;i++)c=c&1?(c>>>1)^0xedb88320:c>>>1;}return(c^0xffffffff)>>>0;}
function zip(entries){
  const local=[],central=[];let offset=0;
  for(const [name,raw,mode=0o100644,method=8] of entries){
    const n=Buffer.from(name),body=Buffer.from(raw),data=method===8?deflateRawSync(body):body,h=Buffer.alloc(30),c=Buffer.alloc(46);
    h.writeUInt32LE(0x04034b50);h.writeUInt16LE(20,4);h.writeUInt16LE(0x800,6);h.writeUInt16LE(method,8);
    h.writeUInt32LE(crc(body),14);h.writeUInt32LE(data.length,18);h.writeUInt32LE(body.length,22);h.writeUInt16LE(n.length,26);
    c.writeUInt32LE(0x02014b50);c.writeUInt16LE(0x314,4);c.writeUInt16LE(20,6);h.copy(c,8,6,26);
    c.writeUInt16LE(n.length,28);c.writeUInt32LE((mode<<16)>>>0,38);c.writeUInt32LE(offset,42);
    local.push(h,n,data);central.push(c,n);offset+=h.length+n.length+data.length;
  }
  const cd=Buffer.concat(central),end=Buffer.alloc(22);end.writeUInt32LE(0x06054b50);end.writeUInt16LE(entries.length,8);end.writeUInt16LE(entries.length,10);end.writeUInt32LE(cd.length,12);end.writeUInt32LE(offset,16);
  return Buffer.concat([...local,cd,end]);
}
function entries(owner='甲',executable=false){
  const type=executable?'「典」【「种类」者『可执行文件』也，「入口」者『入口』也，「文件名」者『yy例包』也，】':'「典」【「种类」者『库』也，】';
  return [
    ['源码/例包。包。豫',`「名称」者『例包』也。「所有者」者『${owner}』也。「版本」者『1.0.0』也。「类型」者${type}也。「简介」者『测试包』也。「说明」者「典」【「汉语」者『说明.汉语.md』也，「文言」者『说明.文言.md』也，】也。`],
    ['源码/说明.汉语.md','说明'],['源码/说明.文言.md','此包之说'],['源码/总集。豫','「数」者「一」也。'],
    ['文档/index.html','<script>untrusted()</script>'],['构建/独立.tar',Buffer.from([0,1,255,13])],
  ];
}
function env(){
  const dir=mkdtempSync(join(tmpdir(),'yy-archive-integration-')),path=join(dir,'业务.sqlite'),db=new DatabaseSync(path);
  for(const file of readdirSync(new URL('../迁移/',import.meta.url)).filter(f=>f.endsWith('.sql')).sort()){
    db.exec('BEGIN;'+readFileSync(new URL('../迁移/'+file,import.meta.url),'utf8')+'COMMIT;');
  }
  db.exec(`INSERT INTO 用户 (编号,名称) VALUES (2,'用户-甲'),(3,'用户-乙');
    INSERT INTO 邮箱账户 (用户编号,邮箱,密码摘要,盐,迭代数,条款版本,接受时间,邮箱已验证) VALUES (2,'a@example.test','x','s',100000,'v',0,1),(3,'b@example.test','x','s',100000,'v',0,1);`);
  function run(actor,action,body,fail=''){
    const input=join(dir,'请求.json');writeFileSync(input,JSON.stringify({actor:String(actor),path:'/__direct/'+action,body:action==='zip'?body.sha256+'\n'+body.archive:JSON.stringify(body),fail}));
    const r=spawnSync(process.env.YY_ARCHIVE_TEST_BINARY||'/tmp/yy归档发布测试',[],{cwd:resolve('../..'),env:{...process.env,YY_PACKAGE_SQLITE:path,YY_ARCHIVE_TEST_INPUT:input},encoding:'utf8',maxBuffer:100*1024*1024,timeout:30000});
    assert.equal(r.status,0,r.stderr);const lines=r.stdout.trim().split('\n').filter(l=>l.startsWith('{')).map(l=>JSON.parse(l));
    return {...lines.at(-1),files:lines.slice(0,-1)};
  }
  const publish=(actor,bytes,fail)=>run(actor,'zip',{archive:bytes.toString('base64'),sha256:createHash('sha256').update(bytes).digest('hex')},fail);
  return {db,run,publish};
}
test('绑定唯一且不可改名，未验证邮箱和未绑定账户不可发布',()=>{
  const {db,run,publish}=env();
  assert.equal(run(2,'zip-auth',{}).status,400);
  assert.equal(publish(2,zip(entries())).status,400);
  assert.equal(run(2,'owner',{name:'甲'}).status,200);
  assert.equal(run(2,'owner',{name:'甲'}).status,200);
  assert.equal(run(3,'owner',{name:'甲'}).status,400);
  assert.equal(run(2,'owner',{name:'改名'}).status,400);
  assert.equal(run(3,'owner',{name:'用户-自称'}).status,400);
  assert.equal(run(3,'owner',{name:'A/恶意'}).status,400);
  assert.equal(run(3,'owner',{name:'乙'}).status,200);
  db.exec('UPDATE 邮箱账户 SET 邮箱已验证=0 WHERE 用户编号=3');
  assert.equal(publish(3,zip(entries('乙'))).status,400);
  assert.equal(db.prepare('SELECT COUNT(*) n FROM 即时版本').get().n,0);
  db.close();
});
test('服务器填充元数据；同档补传复用序数，异档追加修订，所有者保持隔离',()=>{
  const {db,run,publish}=env();run(2,'owner',{name:'甲'});run(3,'owner',{name:'乙'});
  const bytes=zip(entries()),first=publish(2,bytes,'docs/index.html');
  assert.equal(first.status,200,JSON.stringify(first));assert.equal(first.body.incomplete,true);
  const next=publish(2,bytes);assert.equal(next.status,200);assert.equal(next.body.id,first.body.id);assert.equal(next.body.incomplete,false);
  assert.equal(next.body.revision,1);
  assert.deepEqual(Buffer.from(next.files.find(f=>f.path==='archive/发布.zip').data,'base64'),bytes);
  assert.deepEqual(Buffer.from(next.files.find(f=>f.path==='build/独立.tar').data,'base64'),Buffer.from([0,1,255,13]));
  assert.equal(publish(3,bytes).status,400);
  assert.equal(publish(3,zip(entries('乙'))).status,200);
  const changed=publish(2,zip([...entries(),['源码/新增。豫','改变']]));assert.equal(changed.status,200);assert.equal(changed.body.revision,2);assert.notEqual(changed.body.id,first.body.id);
  const retryOld=publish(2,bytes);assert.equal(retryOld.body.id,first.body.id);assert.equal(retryOld.body.revision,1);
  const third=publish(2,zip([...entries(),['源码/另增。豫','再改变']]));assert.equal(third.body.revision,3);
  const exe=publish(2,zip([...entries('甲',true).map(([n,b,...rest])=>[n,n.endsWith('。包。豫')?String(b).replace('1.0.0','1.0.1'):b,...rest]),['运行/linux/yy例包','binary']]));
  assert.equal(exe.status,200);assert.equal(exe.body.incomplete,false);
  const rows=db.prepare('SELECT 名称,版本,类型,所有者编号 FROM 即时版本 ORDER BY 所有者编号').all();
  assert.equal(rows.length,5);assert.equal(rows[0].名称,'例包');assert.equal(rows[0].类型,'库');db.close();
});
test('坏路径、重复路径、符号链接、CRC、尺寸、缺失材料和动态包声明拒绝且不公开',()=>{
  const {db,run,publish}=env();run(2,'owner',{name:'甲'});
  const cases=[
    [...entries(),['../越界','x']],
    [...entries(),['源码/../越界','x']],
    [...entries(),['文档/index.html','duplicate']],
    [...entries(),['文档/链接','/etc/passwd',0o120777]],
    [...entries(),['文档/目录/','',0o40755],['文档/目录','conflict']],
    [...entries(),...Array.from({length:2049},(_,i)=>['文档/'+i,''])],
    [...entries(),['文档/巨物',Buffer.alloc(16777217)]],
    entries().filter(([n])=>n!=='文档/index.html'),
    entries().filter(([n])=>n!=='源码/说明.汉语.md'),
    entries('甲',true),
    entries().map(([n,b])=>[n,n.endsWith('。包。豫')?String(b)+'「恶意」者「打印行」于『不可执行』也。':b]),
    [...entries(),['源码/第二。包。豫','']],
  ];
  for(const c of cases){const r=publish(2,zip(c));assert.equal(r.status,400,JSON.stringify(r));assert.equal(r.files.length,0);}
  const corrupt=zip(entries());corrupt[60]^=255;assert.equal(publish(2,corrupt).status,400);
  const valid=zip(entries()),central=valid.readUInt32LE(valid.length-6);valid.writeUInt32LE(0xffffffff,central+24);
  assert.equal(publish(2,valid).status,400);
  const encrypted=zip(entries()),cd=encrypted.readUInt32LE(encrypted.length-6);encrypted.writeUInt16LE(0x801,cd+8);
  assert.equal(publish(2,encrypted).status,400);
  assert.equal(db.prepare('SELECT COUNT(*) n FROM 即时版本').get().n,0);db.close();
});
