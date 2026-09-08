import { 即时发布入口 } from './即时发布桥.js';

// 古曰：桥惟载籍，豫言成篇。今释：此层仅搬运现有公开接口与 R2 数据，页面结构、转义和 Markdown 均由豫言生成。
const 上限 = 2 * 1024 * 1024;
const 缓存版 = 'native-reader-5';
async function 公开数据(环境, 来源, 路径) {
  const 回应 = await 即时发布入口(new Request(来源 + 路径), 环境);
  if (!回应?.ok) throw Object.assign(Error('公开材料暂不可用'), { status: 回应?.status || 503 });
  return 回应.json();
}
async function 材料文字(环境, 编号, 路径) {
  const 对象 = await 环境.PACKAGES.get('releases/' + 编号 + '/' + 路径);
  if (!对象 || 对象.size > 上限) return null;
  return 对象.text ? 对象.text() : new Response(对象.body).text();
}
export async function 阅读页面入口(请求, 环境) {
  const 网址 = new URL(请求.url), 匹配 = 网址.pathname.match(/^\/release\/([a-f0-9]{32})(?:\/(docs|files))?$/);
  if (!匹配 || !['GET', 'HEAD'].includes(请求.method)) return null;
  const 编号 = 匹配[1], 页签 = 匹配[2] || 'overview';
  const 偏好 = 请求.headers.get('Cookie')?.match(/(?:^|;\s*)yuyan_lang=(han|wen)(?:;|$)/)?.[1];
  const 指定语言 = 网址.searchParams.get('lang');
  const 语言 = ['han', 'wen'].includes(指定语言) ? 指定语言 : 偏好 || 'han';
  const 交付 = 页面 => {
    const 回应 = new Response(请求.method === 'HEAD' ? null : 页面.body, 页面);
    回应.headers.set('Vary', 'Cookie');
    if (['han', 'wen'].includes(指定语言)) {
      const 域 = 网址.hostname.endsWith('.yuyan-lang.org') ? '; Domain=yuyan-lang.org' : '';
      回应.headers.set('Set-Cookie', `yuyan_lang=${语言}; Path=/; Max-Age=31536000; SameSite=Lax${网址.protocol === 'https:' ? '; Secure' : ''}${域}`);
      回应.headers.set('Cache-Control', 'private, max-age=60');
    }
    return 回应;
  };
  const 模式 = 网址.searchParams.get('view') === 'source' ? 'source' : 'docs';
  const 路径 = 网址.searchParams.get('path') || '';
  const 历史偏移 = 页签 === 'overview' ? Number(网址.searchParams.get('historyOffset') || 0) : 0;
  if (!Number.isSafeInteger(历史偏移) || 历史偏移 < 0 || 历史偏移 > 1000000) return new Response('历史分页参数无效', { status: 400 });
  if (路径.length > 1024) return new Response('文件路径过长', { status: 400 });
  // 古曰：公文可藏，私凭不入。今释：缓存键不含账户、令牌或任意查询参数；不向内服转发客户端标头。
  const 缓存址 = new URL(网址.origin + 网址.pathname);
  缓存址.search = new URLSearchParams({ lang: 语言, view: 模式, path: 路径, historyOffset: String(历史偏移), renderer: 缓存版 }).toString();
  const 缓存键 = new Request(缓存址), 缓存 = globalThis.caches?.default;
  let 已存;
  try { 已存 = await 缓存?.match(缓存键); } catch { /* 缓存失效不妨碍公开阅读。 */ }
  if (已存) return 交付(已存);
  try {
    const 发布 = await 公开数据(环境, 网址.origin, '/api/releases/' + 编号);
    while (发布.cursor) {
      const 续页 = await 公开数据(环境, 网址.origin, '/api/releases/' + 编号 + '?cursor=' + encodeURIComponent(发布.cursor));
      发布.files.push(...续页.files); 发布.cursor = 续页.cursor;
      if (发布.files.length > 4096) throw Error('材料过多');
    }
    const 数据 = { tab: 页签, lang: 语言, path: 路径, view: 模式, release: 发布, modules: [], file: {} };
    if (页签 === 'overview') {
      const 配置 = await 材料文字(环境, 编号, 'docs/包信息.json');
      try { 发布.info = 配置 ? JSON.parse(配置) : {}; } catch { 发布.info = {}; }
      const 包配置 = 发布.files.find(项 => /^source\/[^/]+。包。豫$/.test(项.path));
      if (包配置) 发布.packageSource = await 材料文字(环境, 编号, 包配置.path) || '';
      const 说明 = 发布.files.filter(项 => Array.isArray(发布.info?.readmes) && 发布.info.readmes.length > 0
        ? 发布.info.readmes.some(路径 => 项.path === 'source/' + 路径)
        : /^source\/[^/]+\.(汉语|文言)\.md$/.test(项.path));
      发布.readmes = await Promise.all(说明.slice(0, 8).map(async 项 => ({ path: 项.path, text: await 材料文字(环境, 编号, 项.path) || '' })));
      发布.history = await 公开数据(环境, 网址.origin, '/api/releases/' + 编号 + '/history?offset=' + 历史偏移);
    } else if (页签 === 'docs') {
      const 目录 = await 公开数据(环境, 网址.origin, '/api/releases/' + 编号 + '/reading');
      数据.modules = 目录.modules;
    } else {
      const 文件们 = 发布.files.filter(项 => 项.path.startsWith('source/') && 项.path.endsWith('。豫'));
      // 古曰：指书不获，毋以他篇冒之。今释：明确请求不存在的文件时交豫言生成错误页；只有未指定文件才选总集。
      数据.path = 路径 || 文件们.find(项 => 项.path === 'source/总集。豫')?.path || 文件们[0]?.path || '';
      if (路径 && !文件们.some(项 => 项.path === 路径)) 数据.error = 语言 === 'wen' ? '此修订未载所指之豫言文件。' : '此修订中没有指定的豫言文件。';
      else if (数据.path) 数据.file = await 公开数据(环境, 网址.origin, '/api/releases/' + 编号 + '/file?' + new URLSearchParams({ path: 数据.path, view: 模式 }));
    }
    const 正文 = JSON.stringify(数据);
    if (new TextEncoder().encode(正文).byteLength > 8 * 上限) throw Error('阅读材料过多');
    const 回应 = await 环境.PACKAGE_CONTAINER.getByName('豫言包管理').fetch(new Request('http://container.internal/__direct/render', {
      method: 'POST', headers: { 'Content-Type': 'application/json', 'Content-Length': String(new TextEncoder().encode(正文).byteLength) }, body: 正文,
    }));
    if (!回应.ok) throw Object.assign(Error('页面生成暂不可用'), { status: 503 });
    const 标头 = new Headers({
      'Content-Type': 'text/html; charset=utf-8',
      'Content-Security-Policy': "default-src 'self'; script-src 'none'; style-src 'self'; connect-src 'none'; img-src 'self'; frame-src 'none'; frame-ancestors 'none'; base-uri 'none'; form-action 'self'",
      'X-Content-Type-Options': 'nosniff', 'Referrer-Policy': 'no-referrer', 'Cache-Control': 'public, max-age=60',
      'X-Yuyan-Reader': 缓存版,
    });
    const 状态 = 数据.error ? 404 : 200;
    const 页面 = new Response(回应.body, { status: 状态, headers: 标头 });
    if (缓存) { try { await 缓存.put(缓存键, 页面.clone()); } catch { /* 成页仍交读者。 */ } }
    return 交付(页面);
  } catch (错误) {
    console.error('豫言阅读页面转发失败', 错误.message);
    // 古曰：内服有阙，静页犹可指归路。今释：容器不可用时仍用静态外壳提示，不依赖故障容器再次渲染。
    try {
      const 错页 = await 环境.ASSETS?.fetch(new Request(new URL('/错误.html', 网址)));
      if (错页?.ok) return new Response(请求.method === 'HEAD' ? null : 错页.body, {
        status: 错误.status === 404 ? 404 : 503,
        headers: { 'Content-Type': 'text/html; charset=utf-8', 'Cache-Control': 'no-store', 'Retry-After': '5', 'Content-Security-Policy': "default-src 'none'; style-src 'self'; base-uri 'none'; frame-ancestors 'none'", 'X-Content-Type-Options': 'nosniff' },
      });
    } catch { /* 静态资源也不可用时仍保留简明诊断。 */ }
    return new Response(错误.status === 404 ? '此发布版本不存在。' : '页面暂时无法读取，请稍后重试。', {
      status: 错误.status === 404 ? 404 : 503, headers: { 'Content-Type': 'text/plain; charset=utf-8', 'Cache-Control': 'no-store', 'Retry-After': '5' },
    });
  }
}
