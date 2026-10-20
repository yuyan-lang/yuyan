// 文言：秘府（站之私有文件系统）：行在之盘，除快照所来之 /源、/工具 与暂用之 /tmp，皆存于此，重载犹在（提案〇〇〇四三议五）；暂用之中惟共编译之缓存焉。
//   开机则举而读入内存之文件系统；其后有改，稍候而书回（一径屡改惟书一次）。书回惟书所改之区（同步之句柄可用则用之，否则可写之流按位书之），大文小改不必整篇重书。
//   共缓之根依编译语义号分目，开机惟留今号之目，旧号即删，故重载之后前导不必复编（提案〇〇〇四三议四、未决之一）。
// 汉语：秘府（站点私有文件系统，OPFS）：网页行在的盘上除了快照带来的 /源、/工具 与临时的 /tmp，其余（/数据、/系统、/软件 与根目录下的文件，如桌面设置库）
//   都存在这里，重新载入页面后还在（提案 00043 议五）。开机时整棵读进内存文件系统；之后这些路径一改，稍等片刻就写回秘府（同一路径连改几次只写一次）。
//   写回只写改动过的区段（有同步访问句柄就用它，没有就用可写流按位置写），大文件只改一点时不必整篇重写。
//   共享编译缓存（/tmp/豫构缓存）也存进来：缓存根下按编译语义号分目录，开机时只留当前语义号的目录、删掉旧语义号的目录，
//   所以重新载入页面之后前导不必重编（提案 00043 议四与未决问题一）。
//   同一时刻只有一个标签页能写（网页锁），别的标签页照常读入、不写回，免得互相覆盖。
//   待办事项：关页面前最后几百毫秒的改动可能来不及写回；浏览器没有 createWritable 时只读不写。
import {规范路径, 字节} from '../构建/文件系统.mjs';

const 外根们 = ['/源', '/工具'];
// 文言：共缓存之根（同行在开机之环境）。汉语：共享编译缓存根，与行在开机环境里的 豫言缓存目录 相同。
export const 缓存根 = '/tmp/豫构缓存';
// 文言：语义号之书：开机据此删旧号之目。汉语：编译语义号的数据文件，开机时据此删去旧语义号的缓存目录。
const 语义号径 = '/源/库/译场/译料/译设/语义号。数据。豫';
const 号形 = /^[0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12}$/u;
// 文言：当存之径：非根，不在快照之树，不在暂用之树；惟共缓存之根与其下仍存。汉语：要存的路径：不是根目录，不在快照树里，也不在 /tmp 的暂用树里；
//   共享编译缓存根与它下面的都存下来。
export const 当存 = 径 => 径 !== '/' && !外根们.some(根 => 径 === 根 || 径.startsWith(根 + '/'))
  && (径 === 缓存根 || 径.startsWith(缓存根 + '/') || !(径 === '/tmp' || 径.startsWith('/tmp/')));

const 子径 = (径, 名) => (径 === '/' ? '/' + 名 : 径 + '/' + 名);
const 分径 = 径 => { const 位 = 径.lastIndexOf('/'); return [径.slice(0, 位) || '/', 径.slice(位 + 1)]; };
const 取目柄 = async (根柄, 径, 建) => {
  let 柄 = 根柄;
  for (const 段 of 径.split('/').filter(Boolean)) 柄 = await 柄.getDirectoryHandle(段, {create: 建});
  return 柄;
};
const 未有 = 错 => 错?.name === 'NotFoundError';
const 解码器 = new TextDecoder();

// 文言：较旧文与新文，得所改之区（起、止；止为开区间之末），文同则无。旧新同长则前缀同者不书、后缀同者亦不书；长短异则后段之位皆移，自初异之处直至新末尽书之。
// 汉语：比较旧内容与新内容，得出改动的区段 [起, 止)（止为开区间的末端）；内容相同返回 null。旧新一样长时前缀与后缀相同的一概不写，只写中间变过的区段；
//   长度变了（增删）时后面的字节位置都移了，从一个不同处一直写到新内容的末尾，末尾再截短。旧内容为 null（新文件）时整篇都是改动。
export function 改区(旧, 新) {
  if (!旧) return [0, 新.length];
  const 共 = Math.min(旧.length, 新.length);
  let 起 = 0;
  while (起 < 共 && 旧[起] === 新[起]) 起++;
  if (起 === 共) return 旧.length === 新.length ? null : [起, 新.length];
  if (旧.length !== 新.length) return [起, 新.length];
  let 尾 = 0;
  const 限 = 共 - 起;
  while (尾 < 限 && 旧[旧.length - 1 - 尾] === 新[新.length - 1 - 尾]) 尾++;
  return [起, 新.length - 尾];
}

// 文言：举秘府之树而读入。汉语：把秘府里的一棵树读进内存文件系统（文件的修改时间取写回的时间）。
//   快照树整棵不在秘府里，但暂用的 /tmp 底下有共享缓存要读，所以目录要下探，只按“当存”取舍。
async function 读入(柄, 径, 系统) {
  for await (const [名, 子] of 柄.entries()) {
    const 全 = 子径(径, 名);
    if (子.kind === 'directory') {
      if (当存(全)) 系统.建目录(全);
      if (当存(全) || 缓存根.startsWith(全 + '/')) await 读入(子, 全, 系统);
      continue;
    }
    if (子.kind !== 'file' || !当存(全)) continue;
    const 文件 = await 子.getFile();
    系统.写(全, new Uint8Array(await 文件.arrayBuffer()), Math.max(1, Math.floor(文件.lastModified / 1000)));
  }
}

// 文言：以同步之句柄书区：就在其位书所改，末截其长而刷之。汉语：用同步访问句柄按位置写改动的区段，最后截到当前长度并刷盘。
//   同步访问句柄只有专职工作者里才有；没有就返回 false，由可写流按位写。
export async function 句柄书区(文件柄, 内容, 起, 止) {
  if (typeof 文件柄.createSyncAccessHandle !== 'function') return false;
  let 句;
  try { 句 = await 文件柄.createSyncAccessHandle(); } catch { return false; }
  try {
    if (止 > 起) 句.write(内容.subarray(起, 止), {at: 起});
    句.truncate(内容.length);
    句.flush();
  } finally { try { 句.close(); } catch {} }
  return true;
}

// 文言：以可写之流书区：留其旧文，按位书所改，末截其长。汉语：用可写流按位置写改动的区段（保留原有内容），最后截到当前长度。
export async function 流书区(文件柄, 内容, 起, 止) {
  const 写流 = await 文件柄.createWritable({keepExistingData: true});
  try {
    if (止 > 起) await 写流.write({type: 'write', position: 起, data: 内容.subarray(起, 止)});
    await 写流.truncate(内容.length);
    await 写流.close();
  } catch (错) { try { await 写流.abort(); } catch {} throw 错; }
}

// 文言：书回一径：内存有文则书其所改之区（区为无则整篇），有目则建，皆无则删；秘府同名而异类者先删之。
// 汉语：把一个路径写回秘府：内存里是文件就写回改动过的区段（区段为 null 时整篇写），是目录就建目录，都没有就删去（连同其下）；
//   秘府里同名的是另一类（文件与目录）时先删掉再建。
async function 写回一(根柄, 系统, 径, 区) {
  const [父, 名] = 分径(径);
  const 换类取 = (父柄, 取) => 取().catch(async 错 => {
    if (错?.name !== 'TypeMismatchError') throw 错;
    await 父柄.removeEntry(名, {recursive: true});
    return 取();
  });
  if (系统.是文件(径)) {
    const 内容 = 系统.读(径);
    const 起 = 区 ? Math.min(Math.max(0, 区[0]), 内容.length) : 0;
    const 止 = 区 ? Math.min(Math.max(起, 区[1]), 内容.length) : 内容.length;
    const 父柄 = await 取目柄(根柄, 父, true);
    const 文件柄 = await 换类取(父柄, () => 父柄.getFileHandle(名, {create: true}));
    if (!(await 句柄书区(文件柄, 内容, 起, 止))) await 流书区(文件柄, 内容, 起, 止);
  } else if (系统.是目录(径)) {
    const 父柄 = await 取目柄(根柄, 父, true);
    await 换类取(父柄, () => 父柄.getDirectoryHandle(名, {create: true}));
  } else {
    const 父柄 = await 取目柄(根柄, 父, false).catch(错 => { if (未有(错)) return null; throw 错; });
    if (父柄) await 父柄.removeEntry(名, {recursive: true}).catch(错 => { if (!未有(错)) throw 错; });
  }
}

// 文言：缓存根下惟留今编译语义号之目，旧号之目皆删（提案〇〇〇四三议四）。语义号之书不在（老快照）则不动。
// 汉语：缓存根下只留当前编译语义号目录，删去别的语义号目录（提案 00043 议四）；读不到语义号的数据文件（老快照）时不动。
function 清旧号(系统) {
  if (!系统.是文件(语义号径) || !系统.是目录(缓存根)) return;
  const 号 = 解码器.decode(系统.读(语义号径)).match(/「编译语义号」者『([^』]+)』/u)?.[1];
  if (!号) return;
  for (const 名 of 系统.列(缓存根)) if (名 !== 号 && 号形.test(名)) 系统.删全部(缓存根 + '/' + 名);
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
  if (!可写) return {可写, 刷: async () => {}, 等: async () => {}};
  let 链 = Promise.resolve(), 定时 = null;
  // 文言：脏：径与所改之区（无则整篇，或已删、已改类而须依内存之状定）。汉语：待写回的路径与它改动的区段（null 表示整篇写，或已删、已换了类，到时按内存里的样子定）。
  const 脏 = new Map();
  const 刷 = () => {
    clearTimeout(定时); 定时 = null;
    const 们 = [...脏];
    脏.clear();
    链 = 链.then(async () => { for (const [径, 区] of 们) await 写回一(根柄, 系统, 径, 区).catch(错 => 报错('写回秘府失败：' + 径 + '：' + (错?.message ?? 错))); });
    return 链;
  };
  const 记 = (径, 区 = null) => {
    径 = 规范路径(径);
    if (!当存(径)) return;
    if (区 === null || (脏.has(径) && 脏.get(径) === null)) 脏.set(径, null);
    else {
      const 旧区 = 脏.get(径);
      脏.set(径, 旧区 ? [Math.min(旧区[0], 区[0]), Math.max(旧区[1], 区[1])] : 区);
    }
    定时 ??= setTimeout(刷, 稍候毫秒);
  };
  for (const 名 of ['写', '删文件', '删全部', '建目录']) {
    const 原 = 系统[名].bind(系统);
    if (名 === '写') {
      // 文言：先较旧文与新文，得所改之区，乃书之。汉语：先比较旧内容与新内容得出改动的区段，再写，只把区段记下来。
      系统[名] = (径, 内容, ...余) => {
        const 全 = 规范路径(径), 有 = 当存(全) && 系统.是文件(全);
        const 区 = 有 ? 改区(系统.读(全), 字节(内容)) : null;
        const 果 = 原(径, 内容, ...余);
        if (当存(全)) 记(全, 区);
        return 果;
      };
    } else {
      系统[名] = (径, ...余) => { const 果 = 原(径, ...余); 记(径); return 果; };
    }
  }
  const 原改名 = 系统.改名.bind(系统);
  系统.改名 = (旧, 新) => { const 果 = 原改名(旧, 新); 记(旧); 记(新); return 果; };
  // 文言：缓存根下旧语义号之目开机即删。汉语：开机时删去缓存根下旧语义号的目录（经上面的记，稍后写回秘府）。
  清旧号(系统);
  globalThis.addEventListener?.('pagehide', () => { if (脏.size) 刷(); });
  return {可写, 刷, 等: () => 链};
}
