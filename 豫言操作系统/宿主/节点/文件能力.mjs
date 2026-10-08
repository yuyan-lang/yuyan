// 汉语：默认宿主与应用宿主共用文件能力；定位读取仍经豫言共核。文言：默认与应用之宿主共文件之能；定位之读仍经豫言共核。
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import {randomFillSync} from 'node:crypto';
import {共享文件定位读取} from './文件定位.mjs';
const 码 = Object.freeze({成功: 0, 未获授权: 1, 暂不可用: 2, 已失效: 3, 不存在: 4, 已存在: 5, 配额已尽: 6, 输入无效: 7, 操作失败: 8, 读尽: 9});
const 系统错码 = 错 => ({
  ENOENT: 码.不存在, ENOTDIR: 码.不存在, EEXIST: 码.已存在, EACCES: 码.未获授权, EPERM: 码.未获授权,
  EAGAIN: 码.暂不可用, EWOULDBLOCK: 码.暂不可用, EBUSY: 码.暂不可用, EISDIR: 码.输入无效, EINVAL: 码.输入无效,
  ENAMETOOLONG: 码.输入无效, ELOOP: 码.输入无效, EBADF: 码.已失效, ENOSPC: 码.配额已尽, EDQUOT: 码.配额已尽,
  EMFILE: 码.配额已尽, ENFILE: 码.配额已尽, EFBIG: 码.配额已尽
})[错?.code] ?? 码.操作失败;
const 错文 = 错 => String(错?.code ?? 错?.name ?? 'Error') + ': ' + String(错?.message ?? 错);
const 严格解码 = new TextDecoder('utf-8', {fatal: true});
const 空字节 = () => new Uint8Array(0);

function 合法相对路径(径) {
  if (!径 || 径.startsWith('/') || 径.includes('\u0000')) return false;
  // 文言：Windows 以反斜与冒号为界，恐越权，并拒之。汉语：Windows 会把反斜杠和冒号当作路径或流分隔，段内出现即拒绝。待办事项：其他平台是否也应拒绝，以求三平台完全一致。
  if (process.platform === 'win32' && /[\\:]/u.test(径)) return false;
  return 径.split('/').every(段 => 段 !== '' && 段 !== '.' && 段 !== '..');
}
const 在根内 = (根, 实) => {
  const 相对 = 路径.relative(根, 实);
  return 相对 === '' || (!路径.isAbsolute(相对) && 相对 !== '..' && !相对.startsWith('..' + 路径.sep));
};

export function 登记目录授权(目录, 文, 可写, 基准) {
  const 位 = 文.indexOf('=');
  if (!(位 > 0 && 位 < 文.length - 1)) throw Error('目录授权须写成 名=路径：' + 文);
  目录.set(文.slice(0, 位), {路径: 路径.resolve(基准, 文.slice(位 + 1)), 可写});
}
export function 提取文件授权(参数, 当前目录) {
  const 目录 = new Map(), 余参 = [];
  for (let 序 = 0; 序 < 参数.length; 序++) {
    const 项 = 参数[序];
    if (项 === '--') { 余参.push(...参数.slice(序)); break; }
    if (项 === '--授权目录' || 项 === '--授权只读目录') {
      if (++序 >= 参数.length) throw Error('宿主选项缺少值：' + 项);
      登记目录授权(目录, 参数[序], 项 === '--授权目录', 当前目录);
    } else 余参.push(项);
  }
  return {参数: 余参, 授权: {目录}};
}
export function 创建文件能力({授权, 资源 = new Map(), 文字 = 值 => typeof 值 === 'string' ? 值 : new TextDecoder().decode(值), 登记资源 = null, 交换上限 = 16 * 1024 * 1024}) {
  if (!登记资源) 登记资源 = 值 => {
    for (;;) {
      const 随机 = randomFillSync(new Uint32Array(2));
      const 号 = String((随机[0] & 0x1fffff) * 4294967296 + 随机[1]);
      if (!资源.has(号) && Number(号) > 2 ** 40) { 资源.set(号, 值); return 号; }
    }
  };
  const 取目录 = 号 => { const 项 = 资源.get(文字(号)); return 项?.种 === '目录' ? 项 : null; };
  const 取文件 = 号 => { const 项 = 资源.get(文字(号)); return 项?.种 === '文件' ? 项 : null; };
  // 汉语：Node 文件原语：目录权来自 --授权目录；路径先按规范校验，再求真实路径并确认仍在授权目录内（防符号链接逃逸）；句柄号是不可猜测的随机数，宿主核对种类与有效期。
  // 文言：新查惟许空径为授权根，余循旧法；真径必在根内。汉语：目录查询允许空路径表示授权根，其他路径沿用旧校验，真实路径必须留在授权根内。
  const 查询目录路径 = (目录号, 相对) => {
    const 目录 = 取目录(目录号);
    if (!目录) return [码.已失效, '目录权无效或已失效'];
    let 径;
    try { 径 = 严格解码.decode(相对); } catch { return [码.输入无效, '路径不是有效的 UTF-8']; }
    if (径 !== '' && !合法相对路径(径)) return [码.输入无效, '路径无效：' + 径];
    try {
      const 实 = 文件系统.realpathSync(路径.join(目录.根, ...径.split('/')));
      return 在根内(目录.根, 实) ? [码.成功, 实] : [码.输入无效, '路径越出授权目录'];
    } catch (错) { return [系统错码(错), 错文(错)]; }
  };
  const 信息种类 = 信息 => 信息.isFile() ? 0 : 信息.isDirectory() ? 1 : 2;
  const 节点文件 = {
    // 汉语：只删文件或空目录，拒绝授权根与符号链接；待办事项：查父与删除之间的竞态。文言：惟删文或空目录，拒授根与链；待办事项：查父与删之间之竞态。
    豫言_节点_文件删除: (目录号, 相对) => {
      const 目录 = 取目录(目录号);
      if (!目录) return [码.已失效, '目录权无效或已失效'];
      if (!目录.可写) return [码.未获授权, '目录只授读取'];
      let 径;
      try { 径 = 严格解码.decode(相对); } catch { return [码.输入无效, '路径不是有效的 UTF-8']; }
      if (!合法相对路径(径)) return [码.输入无效, '删除路径无效'];
      const 段们 = 径.split('/'), 名 = 段们.pop();
      const [状态, 父] = 查询目录路径(目录号, new TextEncoder().encode(段们.join('/')));
      if (状态 !== 码.成功) return [状态, 父];
      try {
        const 目 = 路径.join(父, 名), 信息 = 文件系统.lstatSync(目);
        if (信息.isFile()) 文件系统.unlinkSync(目);
        else if (信息.isDirectory()) 文件系统.rmdirSync(目);
        else return [码.输入无效, '路径不是普通文件或目录'];
        return [码.成功, ''];
      } catch (错) { return [系统错码(错), 错文(错)]; }
    },
    // 汉语：覆盖与追加共用可写目录权；先核父与已有目标，再打开，防止检查前截断外部文件。待办事项：父路径查询与打开之间的竞态。文言：覆盖与追加共写权；先核父与已有之目，而后开，毋未核而截外文。待办事项：查父与开之间之竞态。
    豫言_节点_文件开启写入: (目录号, 相对, 追加) => {
      const 目录 = 取目录(目录号);
      if (!目录) return [码.已失效, '目录权无效或已失效'];
      if (!目录.可写) return [码.未获授权, '目录只授读取'];
      let 径;
      try { 径 = 严格解码.decode(相对); } catch { return [码.输入无效, '路径不是有效的 UTF-8']; }
      if (!合法相对路径(径)) return [码.输入无效, '文件路径无效'];
      const 段们 = 径.split('/'), 名 = 段们.pop();
      const [状态, 父] = 查询目录路径(目录号, new TextEncoder().encode(段们.join('/')));
      if (状态 !== 码.成功) return [状态, 父];
      let 目 = 路径.join(父, 名), 描述符;
      try {
        if (!文件系统.statSync(父).isDirectory()) return [码.输入无效, '父路径不是目录'];
        let 已存 = true;
        try { 文件系统.lstatSync(目); } catch (错) { if (错.code === 'ENOENT') 已存 = false; else throw 错; }
        if (已存) {
          目 = 文件系统.realpathSync(目);
          if (!在根内(目录.根, 目)) return [码.输入无效, '路径越出授权目录'];
          if (!文件系统.statSync(目).isFile()) return [码.输入无效, '路径不是普通文件'];
        }
        const 旗 = 文件系统.constants;
        描述符 = 文件系统.openSync(目, 旗.O_WRONLY | 旗.O_CREAT | (追加 ? 旗.O_APPEND : 旗.O_TRUNC) | (旗.O_NOFOLLOW ?? 0));
        return [码.成功, 登记资源({种: '文件', 描述符, 可写: true})];
      } catch (错) { return [系统错码(错), 错文(错)]; }
    },
    // 文言：造目录先核写权及父之真径，不递造。汉语：创建单层目录，复用授权根检查，父目录必须存在。
    豫言_节点_文件创建目录: (目录号, 相对) => {
      const 目录 = 取目录(目录号);
      if (!目录) return [码.已失效, '目录权无效或已失效'];
      if (!目录.可写) return [码.未获授权, '目录只授读取'];
      let 径;
      try { 径 = 严格解码.decode(相对); } catch { return [码.输入无效, '路径不是有效的 UTF-8']; }
      if (!合法相对路径(径)) return [码.输入无效, '目录路径无效'];
      const 段们 = 径.split('/');
      const 名 = 段们.pop();
      const [状态, 父] = 查询目录路径(目录号, new TextEncoder().encode(段们.join('/')));
      if (状态 !== 码.成功) return [状态, 父];
      try {
        文件系统.mkdirSync(路径.join(父, 名));
        return [码.成功, ''];
      } catch (错) { return [系统错码(错), 错文(错)]; }
    },
    豫言_节点_文件列目录: (目录号, 相对) => {
      const [状态, 实] = 查询目录路径(目录号, 相对);
      if (状态 !== 码.成功) return [状态, [], 实];
      try {
        if (!文件系统.statSync(实).isDirectory()) return [码.输入无效, [], '路径不是目录'];
        // 文言：链不随而记其他，开链仍核边界；待办事项：查询与使用间之竞态。汉语：目录中的符号链接列为其他，打开时仍核授权边界；待办事项：查询与使用之间的竞态。
        const 项们 = 文件系统.readdirSync(实, {withFileTypes: true}).map(项 => [项.name, 信息种类(项)]);
        return [码.成功, 项们, ''];
      } catch (错) { return [系统错码(错), [], 错文(错)]; }
    },
    豫言_节点_文件查询信息: (目录号, 相对) => {
      const [状态, 实] = 查询目录路径(目录号, 相对);
      if (状态 !== 码.成功) return [状态, 2, 0, 实];
      try {
        const 信息 = 文件系统.statSync(实);
        return [码.成功, 信息种类(信息), 信息.isFile() ? 信息.size : 0, ''];
      } catch (错) { return [系统错码(错), 2, 0, 错文(错)]; }
    },
    豫言_节点_文件取得目录: 名 => {
      const 名称 = 文字(名);
      const 授 = 授权.目录.get(名称);
      if (!授) return [码.未获授权, '目录未获授权：' + 名称];
      let 根;
      try {
        根 = 文件系统.realpathSync(授.路径);
        if (!文件系统.statSync(根).isDirectory()) return [码.不存在, '授权路径不是目录：' + 名称];
      } catch (错) { return [系统错码(错), 错文(错)]; }
      return [码.成功, 登记资源({种: '目录', 根, 可写: 授.可写})];
    },
    豫言_节点_文件打开: (目录号, 相对, 要写) => {
      const 目录 = 取目录(目录号);
      if (!目录) return [码.已失效, '目录权无效或已失效'];
      let 径;
      try { 径 = 严格解码.decode(相对); } catch { return [码.输入无效, '路径不是有效的 UTF-8']; }
      if (!合法相对路径(径)) return [码.输入无效, '路径无效：' + 径];
      const 可写 = Boolean(要写);
      if (可写 && !目录.可写) return [码.未获授权, '目录只授读取'];
      let 实, 描述符;
      try { 实 = 文件系统.realpathSync(路径.join(目录.根, ...径.split('/'))); } catch (错) { return [系统错码(错), 错文(错)]; }
      // 文言：先求真径而后开之，其间可换链；待办事项：逐段 O_NOFOLLOW 以绝其隙。汉语：先求真实路径再打开，两步之间符号链接仍可能被替换；待办事项：逐段用 O_NOFOLLOW 打开以消除竞态。
      if (!在根内(目录.根, 实)) return [码.输入无效, '路径越出授权目录'];
      try { 描述符 = 文件系统.openSync(实, 可写 ? 文件系统.constants.O_WRONLY : 文件系统.constants.O_RDONLY); }
      catch (错) { return [系统错码(错), 错文(错)]; }
      try {
        if (!文件系统.fstatSync(描述符).isFile()) { 文件系统.closeSync(描述符); return [码.输入无效, '路径不是普通文件']; }
      } catch (错) { 文件系统.closeSync(描述符); return [系统错码(错), 错文(错)]; }
      return [码.成功, 登记资源({种: '文件', 描述符, 可写})];
    },
    豫言_节点_文件读取: (文件号, 上限) => {
      const 文件 = 取文件(文件号);
      if (!文件) return [码.已失效, 空字节(), '文件柄无效或已失效'];
      if (文件.可写) return [码.未获授权, 空字节(), '可写文件柄不可读'];
      const 限 = Number(上限);
      if (!(限 >= 0)) return [码.输入无效, 空字节(), '读取上限须非负'];
      if (限 === 0) return [码.成功, 空字节(), ''];
      const 缓 = new Uint8Array(Math.min(限, 交换上限));
      let 读数;
      try { 读数 = 文件系统.readSync(文件.描述符, 缓, 0, 缓.length, null); }
      catch (错) { return [系统错码(错), 空字节(), 错文(错)]; }
      return 读数 === 0 ? [码.读尽, 空字节(), ''] : [码.成功, 缓.slice(0, 读数), ''];
    },
    豫言_节点_文件定位读取: (文件号, 偏移, 上限) => {
      const 文件 = 取文件(文件号);
      if (!文件) return [码.已失效, 空字节(), '文件柄无效或已失效'];
      if (文件.可写) return [码.未获授权, 空字节(), '可写文件柄不可读'];
      const 偏 = BigInt(偏移), 限 = Number(上限);
      if (偏 < 0n || !(限 >= 0)) return [码.输入无效, 空字节(), '偏移与读取上限须非负'];
      if (限 === 0) return [码.成功, 空字节(), ''];
      return 共享文件定位读取(文件.描述符, 偏, Math.min(限, 交换上限));
    },
    豫言_节点_文件写入: (文件号, 内容) => {
      const 文件 = 取文件(文件号);
      if (!文件) return [码.已失效, 0, '文件柄无效或已失效'];
      if (!文件.可写) return [码.未获授权, 0, '只读文件柄不可写'];
      if (内容.length === 0) return [码.成功, 0, ''];
      try {
        const 写数 = 文件系统.writeSync(文件.描述符, 内容);
        return 写数 > 0 ? [码.成功, 写数, ''] : [码.暂不可用, 0, '暂时不能写入'];
      } catch (错) { return [系统错码(错), 0, 错文(错)]; }
    },
    豫言_节点_文件关闭: 文件号 => {
      const 名 = 文字(文件号), 文件 = 取文件(名);
      if (!文件) return [码.已失效, '文件柄无效或已失效'];
      资源.delete(名);
      try { 文件系统.closeSync(文件.描述符); return [码.成功, '']; }
      catch (错) { return [码.操作失败, 错文(错)]; }
    }
  };


  const 实现 = Object.fromEntries(Object.entries(节点文件).map(([名, 函]) => [名.replace('豫言_节点_', '诺节'), 函]));
  return {原语: 节点文件, 实现, 取文件, 清理: () => {
    for (const [号, 项] of 资源) if (项.种 === '文件' || 项.种 === '目录') {
      if (项.种 === '文件') try { 文件系统.closeSync(项.描述符); } catch {}
      资源.delete(号);
    }
  }};
}
