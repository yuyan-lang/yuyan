import { 即时发布入口 } from './即时发布桥.js';
import { 页面版本, 页面键, 提交页面, 完成键 } from './持久页面.js';

// 古曰：桥惟载籍，豫言成篇。今释：此层仅搬运现有公开接口与 R2 数据，页面结构、转义和 Markdown 均由豫言生成。
const 上限 = 2 * 1024 * 1024;
const 缓存版 = 页面版本;
export async function 公开数据(环境, 来源, 路径) {
  const 回应 = await 即时发布入口(new Request(来源 + 路径), 环境);
  if (!回应?.ok) throw Object.assign(Error('公开材料暂不可用'), { status: 回应?.status || 503 });
  return 回应.json();
}
async function 材料文字(环境, 编号, 路径) {
  const 对象 = await 环境.PACKAGES.get('releases/' + 编号 + '/' + 路径);
  if (!对象 || 对象.size > 上限) return null;
  return 对象.text ? 对象.text() : new Response(对象.body).text();
}
export async function 阅读页面入口(请求, 环境, 选项 = {}) {
  const 网址 = new URL(请求.url), 匹配 = 网址.pathname.match(/^\/release\/([a-f0-9]{32})(?:\/(docs|files))?$/);
  if (!匹配 || !['GET', 'HEAD'].includes(请求.method)) return null;
  // 文言：文与源共一器，旧址亦入其器。汉语：旧 docs 地址只兼容进入统一阅读器，不再生成另一份目录。
  const 编号 = 匹配[1], 页签 = 匹配[2] ? 'files' : 'overview';
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
  if (!Number.isSafeInteger(历史偏移) || 历史偏移 < 0 || 历史偏移 > 1000000 || 历史偏移 % 50 !== 0) return new Response('历史分页参数无效', { status: 400 });
  if (路径.length > 1024) return new Response('文件路径过长', { status: 400 });
  // 古曰：公文可藏，私凭不入。今释：缓存键不含账户、令牌或任意查询参数；不向内服转发客户端标头。
  const 缓存址 = new URL(网址.origin + 网址.pathname);
  缓存址.search = new URLSearchParams({ lang: 语言, view: 模式, path: 路径, historyOffset: String(历史偏移), renderer: 缓存版 }).toString();
  const 缓存键 = new Request(缓存址), 缓存 = globalThis.caches?.default;
  let 已存;
  try { if(!选项.生成) 已存 = await 缓存?.match(缓存键); } catch { /* 缓存失效不妨碍公开阅读。 */ }
  if (已存) return 交付(已存);
  try {
    const 规范址 = new URL('/release/'+编号+(页签==='files'?'/files':''),网址.origin);
    规范址.search = new URLSearchParams({lang:语言,view:模式,path:路径,historyOffset:String(历史偏移)}).toString();
    const 持久键 = await 页面键(规范址);
    if(环境.PAGE_QUEUE && 环境.PERSISTENT_PAGES === 'true' && !选项.生成) {
      const 存页 = await 环境.PACKAGES.get(持久键);
      const 公头 = {'Content-Type':'text/html; charset=utf-8','X-Yuyan-Reader':缓存版,'X-Content-Type-Options':'nosniff','Referrer-Policy':'no-referrer','Content-Security-Policy':"default-src 'self'; script-src 'none'; style-src 'self'; connect-src 'none'; img-src 'self'; frame-src 'none'; frame-ancestors 'none'; base-uri 'none'; form-action 'self'"};
      if(存页) {
        if(页签==='overview' && Date.now()-Number(存页.customMetadata?.created||0)>300000)try{await 提交页面(环境,规范址);}catch(error){console.error('概览刷新待重试',error.message);}
        const response=new Response(存页.body,{headers:{...公头,'Cache-Control':'public, max-age=60','X-Yuyan-Page-Store':'r2'}});
        try{await 缓存?.put(缓存键,response.clone());}catch{}
        return 交付(response);
      }
      const 存在 = await 环境.DB.prepare('SELECT "编号" FROM "即时版本" WHERE "编号"=?').bind(编号).first();
      const 合径 = !路径 || (/^source\/.+。豫$/.test(路径) && !路径.split('/').some(p=>!p||p==='.'||p==='..'||p.includes('\\')) && await 环境.PACKAGES.head('releases/'+编号+'/'+路径));
      let 合页 = true;
      if(存在 && 历史偏移>0) { const row=await 环境.DB.prepare('SELECT COUNT(*) AS n FROM "即时版本" WHERE ("所有者编号","名称","版本")=(SELECT "所有者编号","名称","版本" FROM "即时版本" WHERE "编号"=?)').bind(编号).first();合页=历史偏移<Number(row?.n||0); }
      const 状态 = !存在 || !合径 || !合页 ? 404 : 202;
      const 完成标记 = 状态===202 ? await 环境.PACKAGES.head(完成键(编号)) : null;
      if(完成标记) await 提交页面(环境,规范址);
      // 文言：旧篇未藏，仍成而示之；新篇待后台备。汉语：迁移期间旧修订缺页沿用即时渲染并写入 R2，避免全量刷新使既有文档变成等待页。
      if(完成标记?.customMetadata?.legacy === 'true') return await 阅读页面入口(请求,环境,{生成:true});
      const 提示 = await 环境.ASSETS.fetch(new Request(new URL(状态===202?'/生成中.html':'/错误.html',网址)));
      return new Response(请求.method==='HEAD'?null:提示.body,{status:状态,headers:{...公头,'Cache-Control':'no-store','Retry-After':'5'}});
    }
    const 发布 = 选项.发布 || await 公开数据(环境, 网址.origin, '/api/releases/' + 编号);
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
    } else {
      const 文件们 = 发布.files.filter(项 => 项.path.startsWith('source/') && 项.path.endsWith('。豫'));
      // 文言：阅篇惟传篇目，不负归档之全册。汉语：文件阅读不使用下载材料目录；避免每页重复解析全部构建和旧 HTML 条目。
      数据.release = { ...发布, files: 文件们 };
      // 古曰：指书不获，毋以他篇冒之。今释：明确请求不存在的文件时交豫言生成错误页；只有未指定文件才选总集。
      数据.path = 路径 || 文件们.find(项 => 项.path === 'source/总集。豫')?.path || '';
      if (路径 && !文件们.some(项 => 项.path === 路径)) 数据.error = 语言 === 'wen' ? '此修订未载所指之豫言文件。' : '此修订中没有指定的豫言文件。';
      else if (数据.path) 数据.file = await 公开数据(环境, 网址.origin, '/api/releases/' + 编号 + '/file?' + new URLSearchParams({ path: 数据.path, view: 模式 }));
    }
    if(选项.生成 && 数据.file?.documentationError) throw Error(数据.file.documentationError);
    const 正文 = JSON.stringify(数据);
    if (new TextEncoder().encode(正文).byteLength > 8 * 上限) throw Error('阅读材料过多');
    // 文言：析篇分器，勿塞纳包之门。汉语：三个固定后台实例与上传服务隔开，避免大文件阻塞上传。
    const 实例 = 选项.生成 ? '豫言文档-' + (parseInt(持久键.split('/').at(-1).slice(0,8),16) % 3) : '豫言包管理';
    const 回应 = await 环境.PACKAGE_CONTAINER.getByName(实例).fetch(new Request('http://container.internal/__direct/render', {
      method: 'POST', signal: AbortSignal.timeout(60000), headers: { 'Content-Type': 'application/json', 'Content-Length': String(new TextEncoder().encode(正文).byteLength) }, body: 正文,
    }));
    if (!回应.ok) throw Object.assign(Error('页面生成暂不可用'), { status: 503 });
    // 文言：新代之藏，不纳旧器之篇。汉语：滚动部署时拒绝把旧容器输出写入新一代 R2 命名空间。
    if(选项.生成 && !(await 回应.clone().text()).includes('<meta name="yuyan-document-layout" content="3">')) throw Error('文档渲染器正在升级');
    const 标头 = new Headers({
      'Content-Type': 'text/html; charset=utf-8',
      'Content-Security-Policy': "default-src 'self'; script-src 'none'; style-src 'self'; connect-src 'none'; img-src 'self'; frame-src 'none'; frame-ancestors 'none'; base-uri 'none'; form-action 'self'",
      'X-Content-Type-Options': 'nosniff', 'Referrer-Policy': 'no-referrer', 'Cache-Control': 'public, max-age=60',
      'X-Yuyan-Reader': 缓存版,
    });
    const 状态 = 数据.error ? 404 : 200;
    const 页面 = new Response(回应.body, { status: 状态, headers: 标头 });
    if(选项.生成 && 状态===200) {
      await 环境.PACKAGES.put(持久键,await 页面.clone().arrayBuffer(),{httpMetadata:{contentType:'text/html; charset=utf-8'},customMetadata:{created:String(Date.now()),kind:页签}});
    }
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
