// 文言：主站外壳（云仓提案〇〇〇〇五）：先验浏览器有没有 WasmGC 与 JSPI，无之则惟书一行，附文档站同径之链；有之则载入精简开机，
//   取地址栏之径交应用。此篇只做此三事，故小；页面方略尽在样式表里。
// 汉语：主站外壳（云仓提案 C00005）：先检查浏览器是否支持 WasmGC 与 JSPI，不支持就只写一行字，附上文档站同一路径的链接；
//   支持就载入精简开机包，把地址栏路径交给应用。这一篇只做这三件事，所以小；页面怎么摆都写在样式表里。
//   应用的名称与快照网址可用网址参数 应用=、快照= 另指（本机测试用）；缺省取发行目录里的 工具.tar.gz 与其中的演示应用。

// 文言：验法同官网之引导检查（见同目录 ../引导检查.mjs 的「检查浏览器支持」）：WasmGC 以一段最小的模块让客器自验，JSPI 看两个函数在不在。
// 汉语：检查办法照官网的引导检查（见 ../引导检查.mjs 的「检查浏览器支持」）：WasmGC 用一段最小的模块让浏览器自己验证，
//   JSPI 看 WebAssembly.Suspending 与 WebAssembly.promising 两个函数在不在。
const 支持WasmGC = () => {
  try { return WebAssembly.validate(new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0, 1, 3, 1, 0x5f, 0])); } catch { return false; }
};
const 支持JSPI = () => typeof globalThis.WebAssembly?.Suspending === 'function' && typeof globalThis.WebAssembly?.promising === 'function';

// 文言：缺者列名：WasmGC、JSPI、跨源隔离、Worker、DecompressionStream。汉语：缺少的能力名：WasmGC、JSPI、跨源隔离（SharedArrayBuffer）、
//   Worker、DecompressionStream；都具备时为空列表。
const 缺能 = () => [!支持WasmGC() && 'WasmGC', !支持JSPI() && 'JSPI',
  !globalThis.crossOriginIsolated && '跨源隔离（SharedArrayBuffer）', typeof globalThis.Worker !== 'function' && 'Worker',
  typeof globalThis.DecompressionStream !== 'function' && 'DecompressionStream'].filter(Boolean);

// 文言：文档站同径之链：本页之径接于 docs 主机名之后；网址参数不入其径；汉字之径不百分号编之（用户定：链接里的汉字路径照旧写汉字）。
// 汉语：文档站同一路径的链接：把本页路径接在 docs 主机名后面，不带网址参数；汉字路径不做百分号编码（用户定：链接里的汉字路径一律写汉字原样），
//   地址栏给的是编码过的形式，先 decodeURI 还原。
const 文档链 = () => {
  const 径 = decodeURI(location.pathname);
  const 链 = document.createElement('a');
  链.href = 'https://docs.yuyan-lang.org' + 径;
  链.textContent = 'https://docs.yuyan-lang.org' + 径;
  return 链;
};

// 文言：示退路：一行字与文档站同径之链。汉语：显示退路：一行字加文档站同一路径的链接。
const 示退路 = 文 => {
  const 示 = document.getElementById('示');
  示.textContent = '';
  示.append(文 + '文档见 ', 文档链(), '。');
};

// 文言：显全窗之面，撤提示；应用退则反之。汉语：显示全窗画面、撤下提示；应用退出后反过来。
const 示屏 = 开 => document.body.classList.toggle('单应用中', 开);

// 文言：取参数：应用与快照可另指；地址栏之径交应用，为本机测试与阅读器取页之用。汉语：取网址参数：应用与快照可以另指；地址栏路径交给应用，
//   本机测试与阅读器取页都用它。
const 参 = new URLSearchParams(location.search);
const 应用 = 参.get('应用') ?? '/工具/问好。豫行';
const 快照们 = [new URL(参.get('快照') ?? '../工具.tar.gz', import.meta.url)];
const 页面径 = location.pathname + location.search;

try {
  const 缺 = 缺能();
  if (缺.length) 示退路('此页面暂不能运行主站（缺' + 缺.join('、') + '）；');
  else {
    const {起单应用} = await import('../行在/单应用.mjs');
    const 会话 = await 起单应用({快照们, 程序: 应用, 参数们: [页面径], 示屏, 出: (流, 文) => (流 === 2 ? console.error('%s', 文) : console.log('%s', 文)),
      报: 文 => console.log('主站外壳：%s', 文)});
    // 文言：留会话以供浏览器控制台查看（只读之调试入口）。汉语：把会话留给浏览器控制台查看（只读的调试入口）。
    globalThis.主站外壳 = 会话;
    const 记录 = await 会话.完成;
    示退路('应用已退出（退出码 ' + 记录.码 + '）；');
  }
} catch (错) {
  console.error(错);
  示退路('主站启动失败：' + String(错?.message ?? 错) + '；');
}
