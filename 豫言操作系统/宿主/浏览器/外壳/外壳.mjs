// 文言：主站引导（豫言操作系统之引导屏）。逐项验客器之能（见 ../引导检查.mjs 之「豫言系统能力们」），缺则示所荐之版与往文档站之钮，不自迁；
//   皆具则查本机之缓存：虚机之文（虚机清单所列：源、工具、桌面三档并诸字体，存于 R2）已全则径开机；未全则示其数与大小，待按钮许之乃下，
//   下时示进度、验沙二五六、存于 Cache Storage，以后再访不复下。开机则以缓存之文起网页行在，开头句「图形」，径入桌面。
// 汉语：主站引导页（豫言操作系统的开机引导屏）。逐项检查浏览器能力（见 ../引导检查.mjs 的「豫言系统能力们」），缺了就显示推荐的浏览器版本和“前往文档站”按钮，不自动跳转；
//   都有就查本机缓存：虚机文件（虚机清单列出的源、工具、桌面三个快照与各字体，放在 R2 上）齐全就直接开机；不齐就显示个数与大小，等用户按按钮同意才下载，
//   下载时显示进度、核对沙二五六、存进 Cache Storage，以后再访问不再下载。开机时用缓存里的文件起网页行在，开头句给「图形」，直接进桌面。
import {豫言系统能力们, 推荐浏览器} from '../引导检查.mjs';
import {字体表} from '../行在/字体表.mjs';
import {版本, 基址, 文件们} from './虚机清单.mjs';

const 记 = document.getElementById('记'), 说 = document.getElementById('说'), 钮行 = document.getElementById('钮行');
const 进度 = document.getElementById('进度'), 版号 = document.getElementById('版号');
// 文言：缓存以版为名，版易则旧者于新者下毕之后删之。汉语：缓存按版本取名；版本换了，新版下载完以后删掉旧版的缓存。
const 缓存前缀 = '豫言虚机-', 缓存名 = 缓存前缀 + 版本;

// 文言：书一行于引导之记。汉语：在引导记录里写一行。
const 记行 = (文, 类 = '') => {
  const 行 = document.createElement('div');
  if (类) 行.className = 类;
  行.textContent = 文;
  记.append(行);
  记.scrollTop = 记.scrollHeight;
  return 行;
};
const 示说 = 文 => { 说.textContent = 文; };
const 清钮 = () => { 钮行.textContent = ''; };
const 加钮 = (文, 办, 次 = false) => {
  const 钮 = document.createElement('button');
  钮.type = 'button';
  钮.textContent = 文;
  if (次) 钮.className = '次';
  钮.addEventListener('click', 办);
  钮行.append(钮);
  return 钮;
};

// 文言：文档站同径之址：地址栏之径解其百分号之编，汉字照旧书汉字（用户定：链里之汉字径不编）。
// 汉语：文档站同一路径的网址：地址栏路径先解开百分号编码，汉字路径照旧写汉字（用户定：链接里的汉字路径不做百分号编码）；路径里有孤立的百分号时用原文。
const 解径 = 文 => {
  try { return decodeURI(文); } catch { return 文; }
};
const 文档站址 = () => 'https://docs.yuyan-lang.org' + 解径(location.pathname);
const 文档钮 = 次 => 加钮('前往文档站', () => { location.href = 文档站址(); }, 次);

const 兆 = 字节 => (字节 / 1048576).toFixed(字节 < 10485760 ? 1 : 0) + ' MB';
const 十六进制 = 缓冲 => [...new Uint8Array(缓冲)].map(字 => 字.toString(16).padStart(2, '0')).join('');
// 文言：基址可书全址（R2）或同源之径（本机之试），皆依本页之址解之。汉语：基址可以写完整网址（R2），也可以写同源路径（本机测试），都按本页网址解析。
const 文件网址 = 文件 => new URL(文件.名, new URL(基址, location.href)).href;

// 文言：示错：书于记，示其由，予重试与往文档站之钮。汉语：显示错误：写进记录、说明原因，给“重试”与“前往文档站”两个按钮。
function 示错(错) {
  console.error(错);
  进度.hidden = true;
  记行('  错  ' + String(错?.message ?? 错), '缺');
  示说('开机失败：' + String(错?.message ?? 错));
  清钮();
  加钮('重试', () => location.reload());
  文档钮(true);
}

// 文言：缺能：示所缺与所荐之版，予往文档站之钮，不自迁。汉语：缺能力：显示缺什么和推荐的浏览器版本，给“前往文档站”按钮，不自动跳转。
function 示不支持(缺) {
  示说('这个浏览器缺少' + 缺.join('、') + '，开不了豫言操作系统。请用 ' + 推荐浏览器 + ' 及以上版本打开；也可以先去文档站看文档。');
  清钮();
  文档钮(false).focus();
}

// 文言：下一文：流读以示进度，毕则验其沙二五六，合乃存之。汉语：下载一个文件：流式读取并更新进度，读完核对沙二五六，对得上才存进缓存。
async function 下载一个(缓存, 文件, 报进度) {
  const 回 = await fetch(文件网址(文件), {mode: 'cors', cache: 'no-store'});
  if (!回.ok || !回.body) throw Error(文件.名 + '：服务器答 ' + 回.status);
  const 读 = 回.body.getReader(), 块们 = [];
  let 已读 = 0;
  for (;;) {
    const {done, value} = await 读.read();
    if (done) break;
    块们.push(value);
    已读 += value.byteLength;
    报进度(已读);
  }
  const 内容 = new Blob(块们);
  const 摘要 = 十六进制(await crypto.subtle.digest('SHA-256', await 内容.arrayBuffer()));
  if (摘要 !== 文件.沙二五六) throw Error(文件.名 + ' 校验不符（得 ' + 摘要.slice(0, 12) + '…，应为 ' + 文件.沙二五六.slice(0, 12) + '…）');
  await 缓存.put(文件网址(文件), new Response(内容, {headers: {'Content-Type': 'application/octet-stream'}}));
}

// 文言：下诸未有之文，下毕删旧版之缓存。汉语：逐个下载缓存里还没有的文件，全部下完以后删掉别的版本的缓存。
async function 下载全部(缓存, 未有们, 共计) {
  // 文言：请客器久存（不保必得）。汉语：请浏览器把这些存储标成持久（尽力而为，不一定准）。
  try { await navigator.storage?.persist?.(); } catch {}
  进度.hidden = false;
  进度.max = 共计;
  进度.value = 0;
  let 已下 = 0;
  for (const 文件 of 未有们) {
    const 行 = 记行('  取  ' + 文件.名 + '（' + 兆(文件.字节数) + '）');
    await 下载一个(缓存, 文件, 已读 => {
      进度.value = 已下 + 已读;
      示说('正在下载 ' + 文件.名 + '：' + 兆(已下 + 已读) + ' / ' + 兆(共计));
    });
    已下 += 文件.字节数;
    行.textContent = '  存  ' + 文件.名 + '（' + 兆(文件.字节数) + '）';
  }
  for (const 名 of await caches.keys()) if (名.startsWith(缓存前缀) && 名 !== 缓存名) await caches.delete(名);
  进度.hidden = true;
}

// 文言：请许下载：示文数与大小，按钮乃下。汉语：请用户同意下载：显示文件个数与总大小，按了按钮才下载。
function 请授权(缓存, 未有们) {
  const 共计 = 未有们.reduce((和, 文件) => 和 + 文件.字节数, 0);
  示说('第一次开机要把虚机文件下载到这个浏览器里：' + 未有们.length + ' 个文件，共 ' + 兆(共计) + '。下载后存在本机，以后打开直接开机。');
  清钮();
  const 钮 = 加钮('下载并开机', async () => {
    清钮();
    try {
      await 下载全部(缓存, 未有们, 共计);
      await 开机(缓存);
    } catch (错) {
      示错(错);
    }
  });
  文档钮(true);
  钮.focus();
}

// 文言：开机：以缓存之文为 blob 之址，宣诸字体，起网页行在，开头句「图形」径入桌面；桌面退或壳终则复示引导之屏。
// 汉语：开机：把缓存里的文件转成 blob 网址，声明各字体，起网页行在，开头句「图形」直接进桌面；桌面退出或开机壳结束时回到引导屏。
async function 开机(缓存) {
  示说('正在开机……');
  清钮();
  // 文言：诸文以 blob 之址付行在；记其址末段对文名，报进度时易回文名。
  // 汉语：各文件转成 blob 网址交给行在；记下网址末段对应的文件名，报进度时换回文件名。
  const 网址们 = new Map(), 段名们 = new Map();
  const 取 = async 名 => {
    if (!网址们.has(名)) {
      const 文件 = 文件们.find(项 => 项.名 === 名);
      if (!文件) throw Error('虚机清单里没有 ' + 名);
      const 回 = await 缓存.match(文件网址(文件));
      if (!回) throw Error('浏览器缓存里没有 ' + 名);
      const 址 = URL.createObjectURL(await 回.blob());
      网址们.set(名, 址);
      段名们.set(址.split('/').pop(), 名);
    }
    return 网址们.get(名);
  };
  // 文言：报进度：易 blob 之段为文名；同一文之进度书于同一行，不逐次添行。
  // 汉语：报进度：把 blob 网址段换成文件名；同一个文件的进度写在同一行上，不每次新添一行。
  let 末行 = null, 末头 = '';
  const 报 = 文 => {
    for (const [段, 名] of 段名们) 文 = 文.split(段).join(名);
    const 头 = 文.split('：')[0];
    if (末行 && 头 === 末头 && 头.startsWith('正在取')) 末行.textContent = '  ' + 文;
    else {
      末行 = 记行('  ' + 文);
      末头 = 头;
    }
  };
  // 文言：壳之误流录于引导之记（朱字），并予重开与往文档站之钮；入桌面则引导之屏自隐。
  // 汉语：开机壳的标准错误照录在引导记录里（朱红字），同时给“重新开机”和“前往文档站”按钮；进了桌面引导屏自然隐去。
  const 出 = (流, 文) => {
    if (流 !== 2) return console.log('%s', 文);
    console.error('%s', 文);
    for (const 行 of String(文).split('\n')) if (行.trim()) 记行('  壳  ' + 行, '缺');
    if (!钮行.childElementCount) {
      示说('开机遇到错误，见上面的记录。');
      加钮('重新开机', () => location.reload());
      文档钮(true);
    }
  };
  // 文言：字体之文依发行之径：文楷在虚机之根（字体.ttf），余在 字体/ 下，同网页行在之页。汉语：字体文件按发行路径放：文楷在虚机目录根上（字体.ttf），其余在 字体/ 下，同网页行在页面。
  for (const [族, 文件名] of 字体表) {
    const 链 = document.createElement('link');
    链.setAttribute('data-yy-字体', 族);
    链.setAttribute('href', await 取(族 === '文楷' ? 文件名 : '字体/' + 文件名));
    document.head.append(链);
  }
  const 回引导 = 文 => {
    document.body.classList.remove('桌面中');
    示说(文);
    清钮();
    加钮('重新开机', () => location.reload());
    文档钮(true);
  };
  const 示屏 = 开 => {
    document.body.classList.toggle('桌面中', 开);
    if (!开) 回引导('桌面已退出。');
  };
  const {起行在} = await import('../行在/行在.mjs');
  const 会话 = await 起行在({
    快照们: [await 取('源.tar.gz'), await 取('工具.tar.gz')], 桌面快照: await 取('桌面.tar.gz'), 开头句们: ['「图形」'], 出, 报, 示屏,
  });
  // 文言：留会话以供浏览器控制台查看（只读之调试入口）。汉语：把会话留给浏览器控制台查看（只读的调试入口）。
  globalThis.网页行在 = 会话;
  记行('  开机壳已起，正在进桌面');
  const 记录 = await 会话.完成;
  回引导('豫言操作系统已关机（开机壳退出码 ' + 记录.码 + '）。');
}

// 文言：主流：验能、查缓存、请许或径开机。汉语：主流程：检查能力、查缓存，再请用户同意下载或直接开机。
async function 主() {
  版号.textContent = 版本 ? '版本 ' + 版本.slice(0, 10) + ' ·' : '';
  记行('豫言操作系统引导');
  const 缺 = [];
  for (const [名, 验] of 豫言系统能力们) {
    let 有 = false;
    try { 有 = Boolean(验()); } catch {}
    记行((有 ? '  有  ' : '  缺  ') + 名, 有 ? '有' : '缺');
    if (!有) 缺.push(名);
  }
  if (缺.length) return 示不支持(缺);
  if (!文件们.length || !基址) throw Error('虚机清单是空的（仓库里是开发用的占位清单，发行时由仓库目标写出）');
  const 缓存 = await caches.open(缓存名);
  const 未有们 = [];
  for (const 文件 of 文件们) if (!(await 缓存.match(文件网址(文件)))) 未有们.push(文件);
  if (未有们.length) return 请授权(缓存, 未有们);
  return 开机(缓存);
}

主().catch(示错);
