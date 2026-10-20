// 文言：秘府（站之私有文件系统）：行在之盘，除快照所来之 /源、/工具 与暂用之 /tmp，皆存于此，重载犹在（提案〇〇〇四三议五）。
//   开机则举而读入内存之文件系统；其后有改，稍候而书回（一径屡改惟书一次）。一时惟一页得书，余页只读，免相覆。
// 汉语：秘府（站点私有文件系统，OPFS）：网页行在的盘上除了快照带来的 /源、/工具 与临时的 /tmp，其余（/数据、/系统、/软件 与根目录下的文件，如桌面设置库）
//   都存在这里，重新载入页面后还在（提案 00043 议五）。开机时整棵读进内存文件系统；之后这些路径一改，稍等片刻就写回秘府（同一路径连改几次只写一次）。
//   同一时刻只有一个标签页能写（网页锁），别的标签页照常读入、不写回，免得互相覆盖。
//   待办事项：文件每次整篇写回，大文件慢；关页面前最后几百毫秒的改动可能来不及写回；浏览器没有 createWritable 时只读不写。
import {规范路径} from '../构建/文件系统.mjs';

const 外根们 = ['/源', '/工具', '/tmp'];
// 文言：当存之径：非根，亦不在快照与暂用之树。汉语：要存的路径：不是根目录，也不在快照与临时目录的树里。
export const 当存 = 径 => 径 !== '/' && !外根们.some(根 => 径 === 根 || 径.startsWith(根 + '/'));

const 子径 = (径, 名) => (径 === '/' ? '/' + 名 : 径 + '/' + 名);
const 分径 = 径 => { const 位 = 径.lastIndexOf('/'); return [径.slice(0, 位) || '/', 径.slice(位 + 1)]; };
const 取目柄 = async (根柄, 径, 建) => {
  let 柄 = 根柄;
  for (const 段 of 径.split('/').filter(Boolean)) 柄 = await 柄.getDirectoryHandle(段, {create: 建});
  return 柄;
};
const 未有 = 错 => 错?.name === 'NotFoundError';

// 文言：举秘府之树而读入。汉语：把秘府里的一棵树读进内存文件系统（文件的修改时间取写回的时间）。
async function 读入(柄, 径, 系统) {
  for await (const [名, 子] of 柄.entries()) {
    const 全 = 子径(径, 名);
    if (!当存(全)) continue;
    if (子.kind === 'directory') { 系统.建目录(全); await 读入(子, 全, 系统); continue; }
    const 文件 = await 子.getFile();
    系统.写(全, new Uint8Array(await 文件.arrayBuffer()), Math.max(1, Math.floor(文件.lastModified / 1000)));
  }
}

// 文言：书回一径：内存有文则覆写，有目则建，皆无则删；秘府同名而异类者先删之。
// 汉语：把一个路径写回秘府：内存里是文件就整篇覆写，是目录就建目录，都没有就删去（连同其下）；秘府里同名的是另一类（文件与目录）时先删掉再建。
async function 写回一(根柄, 系统, 径) {
  const [父, 名] = 分径(径);
  const 换类取 = (父柄, 取) => 取().catch(async 错 => {
    if (错?.name !== 'TypeMismatchError') throw 错;
    await 父柄.removeEntry(名, {recursive: true});
    return 取();
  });
  if (系统.是文件(径)) {
    const 父柄 = await 取目柄(根柄, 父, true);
    const 写流 = await (await 换类取(父柄, () => 父柄.getFileHandle(名, {create: true}))).createWritable();
    await 写流.write(系统.读(径));
    await 写流.close();
  } else if (系统.是目录(径)) {
    const 父柄 = await 取目柄(根柄, 父, true);
    await 换类取(父柄, () => 父柄.getDirectoryHandle(名, {create: true}));
  } else {
    const 父柄 = await 取目柄(根柄, 父, false).catch(错 => { if (未有(错)) return null; throw 错; });
    if (父柄) await 父柄.removeEntry(名, {recursive: true}).catch(错 => { if (!未有(错)) throw 错; });
  }
}

// 文言：接秘府：读入其树，而后截内存文件系统之改以书回。返 {可写, 刷, 等}。
// 汉语：接上秘府：先把秘府里的树读进内存文件系统，再截下内存文件系统的改动（写、删、改名、建目录）稍后写回。
//   返回 {可写, 刷, 等}：可写说明本页是否写回，刷() 立刻写回积下的改动，等() 候写回办完；报错(文) 收写回失败的说明。
export async function 接秘府(系统, {报错 = () => {}, 稍候毫秒 = 300} = {}) {
  if (typeof navigator === 'undefined' || !navigator.storage?.getDirectory) return {可写: false, 刷: async () => {}, 等: async () => {}};
  const 根柄 = await (await navigator.storage.getDirectory()).getDirectoryHandle('网页行在', {create: true});
  await 读入(根柄, '/', 系统);
  const 可写 = typeof FileSystemFileHandle !== 'undefined' && 'createWritable' in FileSystemFileHandle.prototype
    && await new Promise(成 => navigator.locks?.request('网页行在秘府', {ifAvailable: true}, 锁 => { 成(Boolean(锁)); return 锁 ? new Promise(() => {}) : undefined; }) ?? 成(true));
  let 链 = Promise.resolve(), 定时 = null;
  const 脏 = new Set();
  const 刷 = () => {
    clearTimeout(定时); 定时 = null;
    const 们 = [...脏];
    脏.clear();
    链 = 链.then(async () => { for (const 径 of 们) await 写回一(根柄, 系统, 径).catch(错 => 报错('写回秘府失败：' + 径 + '：' + (错?.message ?? 错))); });
    return 链;
  };
  if (!可写) return {可写, 刷: async () => {}, 等: async () => {}};
  const 记 = 径 => {
    径 = 规范路径(径);
    if (!当存(径)) return;
    脏.add(径);
    定时 ??= setTimeout(刷, 稍候毫秒);
  };
  for (const 名 of ['写', '删文件', '删全部', '建目录']) {
    const 原 = 系统[名].bind(系统);
    系统[名] = (径, ...余) => { const 果 = 原(径, ...余); 记(径); return 果; };
  }
  const 原改名 = 系统.改名.bind(系统);
  系统.改名 = (旧, 新) => { const 果 = 原改名(旧, 新); 记(旧); 记(新); return 果; };
  globalThis.addEventListener?.('pagehide', () => { if (脏.size) 刷(); });
  return {可写, 刷, 等: () => 链};
}
