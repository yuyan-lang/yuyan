// 文言：路一节点宿主之一致性验：备夹具与本机网服，以所给之发行包行一致性之客，逐行较其所出，又验装载之拒。
// 汉语：路一 Node 宿主的适配一致性验证驱动：准备文件夹具与本机 HTTP 测试服务，用给定的发行包（程序.wasm、启动.mjs、清单.json）运行一致性应用，把标准输出与标准错误逐行和期望文件比较；再验证装载核对的负例。
// 汉语：只用 Node 自带模块，macOS、Linux、Windows 通用；同一份发行包在三个平台上的输出必须逐字相同。用法：node 运行.mjs <发行包目录> [--记录] [--张量后端 名]
// 文言：--张量后端 名：以所指之后端行之（非中央处理器则并予 --允许系统库调用），验加速之后端；此时「张量·后端」一行本当异于期望。名为「自动」则不指后端而惟予 --允许系统库调用，验适配之自择（有加速之后端则不限加速亦用之）。
// 汉语：--张量后端 名：用指定的张量后端运行（不是中央处理器时同时给 --允许系统库调用），用来验收加速后端；这时「张量·后端」一行按设计与期望不同，其余各行应当相同。名为“自动”时不指定后端、只给 --允许系统库调用，验收适配的自动选择（有加速后端时不限加速也用它）。
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import * as 文件系统 from 'node:fs';
import http from 'node:http';
import 系统 from 'node:os';
import 路径 from 'node:path';
import {fileURLToPath} from 'node:url';

const 本目录 = 路径.dirname(fileURLToPath(import.meta.url));
const 参数 = process.argv.slice(2);
const 记录 = 参数.includes('--记录');
const 后端序 = 参数.indexOf('--张量后端');
const 张量后端 = 后端序 >= 0 ? 参数[后端序 + 1] : '中央处理器';
const 包目录 = 路径.resolve(参数.find((项, 序) => !项.startsWith('--') && (后端序 < 0 || 序 !== 后端序 + 1)) ?? 'dist/节点一致性');
const 期望输出路径 = 路径.join(本目录, '期望输出.txt');
const 期望错误路径 = 路径.join(本目录, '期望错误.txt');

const 结果 = [];
const 记 = (名, 通过, 说明 = '') => {
  结果.push({名, 通过});
  if (!通过) console.log(`失败：${名}${说明 ? '：' + 说明 : ''}`);
};

// 文言：夹具之目：甲可读写，乙只读，外部不授而以链指之。汉语：文件夹具（含张量文件用例的 甲/张量.bin）：甲目录授读写，乙目录只授读取；“外部”不授权，甲/越界 是指向它的目录链接（Windows 上用 junction，不需要管理员权限）。
function 备夹具() {
  const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), '豫言节点一致性-'));
  const 写 = (相对, 内容) => {
    const 全 = 路径.join(根, ...相对.split('/'));
    文件系统.mkdirSync(路径.dirname(全), {recursive: true});
    文件系统.writeFileSync(全, 内容);
  };
  写('甲/零字节.bin', Uint8Array.of(0, 1, 2, 0, 255));
  写('甲/空文件.txt', '');
  写('甲/可写.txt', '旧内容');
  写('甲/子目录/深.txt', '深处');
  // 文言：张量文件之夹具：百字，第 i 字为 (37i+11) 模 256。汉语：张量文件用例的夹具：100 字节，第 i 个字节为 (37i + 11) mod 256。
  写('甲/张量.bin', Uint8Array.from({length: 100}, (_, 序) => (序 * 37 + 11) & 255));
  写('乙/只读.txt', '只读内容');
  写('外部/秘密.txt', '秘密');
  文件系统.symlinkSync(路径.join(根, '外部'), 路径.join(根, '甲', '越界'), 'junction');
  return 根;
}

// 文言：本机网服：成、缺、转址、回显、巨答。汉语：本机 HTTP 测试服务：/hello 成功（含重复标头）、/missing 404、/redirect 302、/echo 回显二进制正文、/big 超过 16 MiB 的响应。
function 起网服() {
  const 服务 = http.createServer((请, 答) => {
    const 径 = new URL(请.url, 'http://本机').pathname;
    if (径 === '/hello') {
      答.statusCode = 200;
      答.setHeader('Content-Type', 'text/plain; charset=utf-8');
      答.setHeader('X-Dup', ['1', '2']);
      答.end(请.method === 'HEAD' ? undefined : '你好，世界');
    } else if (径 === '/missing') {
      答.statusCode = 404;
      答.end('没有');
    } else if (径 === '/redirect') {
      答.statusCode = 302;
      答.setHeader('Location', '/hello');
      答.end('跳');
    } else if (径 === '/echo') {
      const 块们 = [];
      请.on('data', 块 => 块们.push(块));
      请.on('end', () => {
        答.statusCode = 201;
        答.setHeader('Content-Type', 'application/octet-stream');
        答.end(Buffer.concat(块们));
      });
    } else if (径 === '/big') {
      答.statusCode = 200;
      const 块 = Buffer.alloc(1024 * 1024, 97);
      let 余 = 16;
      const 送 = () => {
        while (余 > 0) { 余--; if (!答.write(块)) { 答.once('drain', 送); return; } }
        答.end('a');
      };
      答.on('error', () => {});
      送();
    } else {
      答.statusCode = 500;
      答.end('未知路径');
    }
  });
  return new Promise(完成 => 服务.listen(0, '127.0.0.1', () => 完成(服务)));
}

// 文言：取一已闭之端口，以验连接之败。汉语：先监听再关闭，得到一个没有服务的本机端口，用来验证连接失败。
function 取已闭端口() {
  return new Promise(完成 => {
    const 临 = http.createServer();
    临.listen(0, '127.0.0.1', () => { const 端口 = 临.address().port; 临.close(() => 完成(端口)); });
  });
}

function 运行(启动文件, 实参, 环境) {
  return new Promise(完成 => {
    const 子 = spawn(process.execPath, [启动文件, ...实参], {env: 环境, stdio: ['ignore', 'pipe', 'pipe']});
    const 出 = [], 误 = [];
    子.stdout.on('data', 块 => 出.push(块));
    子.stderr.on('data', 块 => 误.push(块));
    子.on('close', 码 => 完成({码, 出: Buffer.concat(出), 误: Buffer.concat(误)}));
  });
}

// 文言：期望之文或经 Windows 之 git 改行尾，较前去其回车。汉语：期望文件在 Windows 上可能被 git 换成 CRLF，比较前统一去掉换行前的回车（应用本身只输出 LF）。
const 分行 = 字节 => 字节.toString('utf8').replace(/\r\n/gu, '\n').split('\n');
function 逐行较(类, 实际, 期望) {
  const 实行 = 分行(实际), 期行 = 分行(期望);
  const 长 = Math.max(实行.length, 期行.length);
  for (let 序 = 0; 序 < 长; 序++) {
    const 甲 = 实行[序], 乙 = 期行[序];
    if (甲 === '' && 乙 === '' && 序 === 长 - 1) continue;
    const 名 = `${类}第${序 + 1}行` + (乙 ? `（${乙.slice(0, 40)}）` : '');
    记(名, 甲 === 乙, `期望「${String(乙).slice(0, 200)}」，实际「${String(甲).slice(0, 200)}」`);
  }
}

// 文言：抄发行包于暂目录而改其一处，验装载之拒。汉语：把发行包复制到临时目录并改动一处，验证装载核对在运行应用前拒绝（退出码 3，标准输出为空）。
async function 验装载负例(名, 改, 期望片段) {
  const 暂 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), '豫言节点负例-'));
  for (const 文件 of ['程序.wasm', '启动.mjs', '清单.json']) 文件系统.copyFileSync(路径.join(包目录, 文件), 路径.join(暂, 文件));
  const 清单 = JSON.parse(文件系统.readFileSync(路径.join(暂, '清单.json'), 'utf8'));
  改(清单, 暂);
  文件系统.writeFileSync(路径.join(暂, '清单.json'), JSON.stringify(清单));
  const 果 = await 运行(路径.join(暂, '启动.mjs'), [], {...process.env});
  const 误文 = 果.误.toString('utf8');
  记(`装载负例·${名}`, 果.码 === 3 && 果.出.length === 0 && 误文.includes(期望片段),
    `退出码 ${果.码}，标准错误「${误文.trim().slice(0, 200)}」`);
  文件系统.rmSync(暂, {recursive: true, force: true});
}

const 摘要 = 字节 => createHash('sha256').update(字节).digest('hex');

const 根 = 备夹具();
const 服务 = await 起网服();
const 基址 = `http://127.0.0.1:${服务.address().port}`;
const 断址 = `http://127.0.0.1:${await 取已闭端口()}`;
const 启动文件 = 路径.join(包目录, '启动.mjs');
const 环境 = {...process.env, YY_CONF_A: '值甲\n第二行', YY_CONF_EMPTY: ''};
delete 环境.YY_CONF_MISSING;
delete 环境.YY_CONF_SECRET;
try {
  const 主 = await 运行(启动文件, [
    '--授权目录', `甲=${路径.join(根, '甲')}`,
    '--授权只读目录', `乙=${路径.join(根, '乙')}`,
    '--允许源', 基址, '--允许源', 断址,
    '--允许环境', 'YY_CONF_A', '--允许环境', 'YY_CONF_EMPTY', '--允许环境', 'YY_CONF_MISSING',
    // 文言：张量缺省强用中央处理器，诸平台之出乃同。汉语：张量计算缺省强制用中央处理器后端，各平台输出才相同（必须加速的用例因此一定失败）；--张量后端 可改。
    ...(张量后端 === '自动' ? ['--允许系统库调用']
      : ['--张量后端', 张量后端, ...(张量后端 === '中央处理器' ? [] : ['--允许系统库调用'])]),
    '--', 基址, 断址
  ], 环境);
  if (记录) {
    文件系统.writeFileSync(期望输出路径, 主.出);
    文件系统.writeFileSync(期望错误路径, 主.误);
    console.log(`已记录期望输出（${主.出.length} 字节）与期望错误（${主.误.length} 字节）；退出码 ${主.码}`);
  }
  记('主程序退出码为零', 主.码 === 0, `退出码 ${主.码}；标准错误末段「${主.误.toString('utf8').slice(-300)}」`);
  逐行较('标准输出', 主.出, 文件系统.readFileSync(期望输出路径));
  逐行较('标准错误', 主.误, 文件系统.readFileSync(期望错误路径));
  const 可写后 = 文件系统.readFileSync(路径.join(根, '甲', '可写.txt'), 'utf8');
  记('文件·写入落盘且不截断', 可写后 === '新内容', `实际「${可写后}」`);
  记('文件·不创建新文件', !文件系统.existsSync(路径.join(根, '甲', '不存在.txt')));

  // 文言：未授之环境变量，读之则宿主拒而程序终。汉语：读取未授权的环境变量是部署错误：宿主拒绝，程序以失败退出。
  const 未授 = await 运行(启动文件, ['--', '未授权环境'], {...环境, YY_CONF_SECRET: '不该读到'});
  记('负例·未授权环境变量', 未授.码 === 1 && 未授.误.toString('utf8').includes('未授权的ENV绑定：YY_CONF_SECRET') &&
    !未授.出.toString('utf8').includes('不该读到'), `退出码 ${未授.码}`);

  await 验装载负例('接口版本不符', 清单 => { 清单.接口要求[0].接口版本 = '9.9.9'; 清单.接口要求[0].解析闭包[0].版本 = '9.9.9'; }, '宿主不支持接口');
  await 验装载负例('接口签名被改', 清单 => { 清单.接口要求[0].函数[0].签名 = '→[「 有 」；「 整数 」]'; }, '接口规范或签名不一致');
  await 验装载负例('宿主不支持的接口', 清单 => {
    清单.接口要求.push(JSON.parse(文件系统.readFileSync(路径.join(本目录, '../../../适配/持久事务/宿主提供.json'), 'utf8')));
  }, '宿主不支持接口');
  await 验装载负例('应用提供接口不符', 清单 => { 清单.应用提供[0].函数[0].签名 = '→[「 有 」；「 整数 」]'; }, '宿主不认可应用提供的接口');
  await 验装载负例('程序摘要不符', 清单 => { 清单.程序摘要 = '0'.repeat(64); }, '程序摘要');
  await 验装载负例('发行清单格式不符', 清单 => { 清单.格式版本 = 2; }, '发行清单格式不符');
  await 验装载负例('Wasm 缺少宿主导入与启动导出', (清单, 暂) => {
    const 空 = Uint8Array.of(0, 97, 115, 109, 1, 0, 0, 0);
    文件系统.writeFileSync(路径.join(暂, '程序.wasm'), 空);
    清单.程序摘要 = 摘要(空);
  }, 'Wasm 宿主导入形状不符');
} finally {
  服务.close();
  文件系统.rmSync(根, {recursive: true, force: true});
}

const 通过 = 结果.filter(项 => 项.通过).length;
console.log(`路一 Node 宿主一致性（${process.platform} ${process.arch}，Node ${process.versions.node}）：通过 ${通过} / ${结果.length}`);
process.exitCode = 通过 === 结果.length ? 0 : 1;
