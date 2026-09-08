import { 查会话 } from './账户.js';
import { 读取文档映射, 阅读入口, 补全阅读页, 文件阅读数据 } from './阅读.js';

// 古曰：内服定籍与权，外桥惟送字节。今释：版本写入和发布权限由豫言处理；此桥负责流式 R2、公开查询和浏览器隔离。
export const 门户来源 = env => env.PORTAL_ORIGIN || 'https://packages.yuyan-lang.org';
export const 内容来源 = env => env.USERCONTENT_ORIGIN || 'https://usercontent.yuyan-lang.org';
const 编号式 = /^[a-f0-9]{32}$/;
const 种类们 = new Set(['source', 'build', 'readme', 'docs', 'runtime', 'archive']);
const 回 = (data, status = 200) => Response.json(data, { status, headers: { 'Cache-Control': 'no-store', 'X-Content-Type-Options': 'nosniff' } });
const 错 = (message, status = 400) => { throw Object.assign(new Error(message), { status }); };
export function 安全文件路径(raw) {
  let path;
  try { path = decodeURIComponent(raw); } catch { 错('文件路径编码错误'); }
  if (!path || path.length > 1024 || /[\\\x00-\x1f\x7f]/u.test(path) ||
      path.split('/').some(p => !p || p === '.' || p === '..' || p.includes(':'))) 错('文件路径无效');
  return path;
}
const 编路径 = path => path.split('/').map(encodeURIComponent).join('/');
const 版本查询 = 'SELECT v."编号" AS id,u."名称" AS owner,u."已验证" AS identityVerified,v."名称" AS name,v."版本" AS version,v."上传序数" AS revision,v."类型" AS type,v."简介" AS description,v."创建时间" AS created FROM "即时版本" v JOIN "用户" u ON u."编号"=v."所有者编号"';
async function 查版本(env, id) { return env.DB.prepare(版本查询 + ' WHERE v."编号"=?').bind(id).first(); }
export async function 请求内服(req, env, action, body) {
  if (env.REGISTRATION_ENABLED === 'false') return 回({ error: '发布暂未开放' }, 503);
  const origin = req.headers.get('Origin');
  if (origin && origin !== new URL(req.url).origin) return 回({ error: '拒绝跨站发布' }, 403);
  const headers = new Headers({ 'Content-Type': 'application/json', 'Content-Length': String(new TextEncoder().encode(body).byteLength) });
  // 古曰：主号由桥重定，不信客首。今释：绝不转发客户端伪造的发布者或内部路径标头。
  if (req.headers.has('Authorization')) {
    if (action === 'owner') return 回({ error: '绑定名称需要登录会话' }, 403);
    headers.set('Authorization', req.headers.get('Authorization'));
  }
  else {
    if (origin !== new URL(req.url).origin) return 回({ error: '会话发布需要同源 Origin' }, 403);
    const user = await 查会话(req, env.DB, Math.floor(Date.now() / 1000));
    if (!user) return 回({ error: '请先登录' }, 401);
    if (!user.emailVerified) return 回({ error: '请先验证邮箱' }, 403);
    if (action !== 'owner' && !user.ownerBound) return 回({ error: '请先绑定所有者名称' }, 403);
    headers.set('X-Yuyan-Publisher', String(user.id));
  }
  return env.PACKAGE_CONTAINER.getByName('豫言包管理').fetch(new Request('http://container.internal/__direct/' + action, {
    method: 'POST', headers, body,
  }));
}
export async function 即时发布入口(req, env) {
  const url = new URL(req.url);
  if (!url.pathname.startsWith('/api/releases')) return null;
  try {
    const parts = url.pathname.split('/').slice(3), id = parts[0];
    if (url.pathname === '/api/releases') {
      if (req.method === 'POST') return 回({ error: '请使用 /api/releases/zip 上传完整 ZIP' }, 410);
      if (req.method !== 'GET') return 回({ error: '不支持的方法' }, 405);
      const offset = Number(url.searchParams.get('offset') || 0);
      if (!Number.isSafeInteger(offset) || offset < 0 || offset > 1000000) 错('分页参数错误');
      const conditions=['NOT EXISTS (SELECT 1 FROM "即时版本" r WHERE r."所有者编号"=v."所有者编号" AND r."名称"=v."名称" AND r."版本"=v."版本" AND r."上传序数">v."上传序数")'],args=[];
      if(url.searchParams.get('mine')==='1'){
        const user=await 查会话(req,env.DB,Math.floor(Date.now()/1000));
        if(!user)return 回({error:'请先登录'},401);
        conditions.push('v."所有者编号"=?');args.push(user.id);
      }
      if(url.searchParams.get('catalog')==='1')conditions.push('NOT EXISTS (SELECT 1 FROM "即时版本" newer WHERE newer."所有者编号"=v."所有者编号" AND newer."名称"=v."名称" AND (newer."版本"<>v."版本" OR newer."上传序数">v."上传序数") AND (newer."创建时间">v."创建时间" OR (newer."创建时间"=v."创建时间" AND newer."编号">v."编号")))');
      const q=(url.searchParams.get('q')||'').trim();if(q.length>100)错('搜索词过长');
      if(q){conditions.push('(instr(v."名称",?)>0 OR instr(u."名称",?)>0 OR instr(v."简介",?)>0)');args.push(q,q,q);}
      const result = await env.DB.prepare(版本查询 + (conditions.length?' WHERE '+conditions.join(' AND '):'') + ' ORDER BY v."创建时间" DESC,v."编号" DESC LIMIT 50 OFFSET ?').bind(...args,offset).all();
      return 回({ releases: result.results, nextOffset: result.results.length === 50 ? offset + 50 : null });
    }
    if (!编号式.test(id || '')) return 回({ error: '版本不存在' }, 404);
    if(parts.length===2&&parts[1]==='file'&&req.method==='GET'){
      if(!await 查版本(env,id))return 回({error:'版本不存在'},404);
      return 回(await 文件阅读数据(env,id,url.searchParams.get('path'),url.searchParams.get('view')));
    }
    if(parts.length===2&&parts[1]==='reading'&&req.method==='GET'){
      if(!await 查版本(env,id))return 回({error:'版本不存在'},404);
      return 回({modules:await 读取文档映射(env,id)});
    }
    // 文言：旧号仍指旧物，别列其同版诸次。汉语：历史按修订降序分页，下载固定在原 ID，不重定向到新文件。
    if(parts.length===2&&parts[1]==='history'&&req.method==='GET'){
      const version=await 查版本(env,id);if(!version)return 回({error:'版本不存在'},404);
      const offset=Number(url.searchParams.get('offset')||0);if(!Number.isSafeInteger(offset)||offset<0||offset>1000000)错('分页参数错误');
      const result=await env.DB.prepare(版本查询+' WHERE u."名称"=? AND v."名称"=? AND v."版本"=? ORDER BY v."上传序数" DESC LIMIT 50 OFFSET ?').bind(version.owner,version.name,version.version,offset).all();
      return 回({revisions:result.results.map(r=>({...r,url:'/release/'+r.id,downloadUrl:'/api/releases/'+r.id+'/files/archive/'+编路径('发布.zip')})),nextOffset:result.results.length===50?offset+50:null});
    }
    if (parts.length === 1 && req.method === 'GET') {
      const version = await 查版本(env, id);
      if (!version) return 回({ error: '版本不存在' }, 404);
      const prefix = 'releases/' + id + '/';
      const latest=await env.DB.prepare(版本查询+' WHERE u."名称"=? AND v."名称"=? AND v."版本"=? ORDER BY v."上传序数" DESC LIMIT 1').bind(version.owner,version.name,version.version).first();
      const listed = await env.PACKAGES.list({ prefix, limit: 1000, cursor: url.searchParams.get('cursor') || undefined });
      return 回({ ...version, latestId:latest.id, historyUrl:'/api/releases/'+id+'/history', review: 'unreviewed', docsUrl: 内容来源(env) + '/' + id + '/index.html',
        files: listed.objects.map(o => ({ path: o.key.slice(prefix.length), size: o.size,
          url: '/api/releases/' + id + '/files/' + 编路径(o.key.slice(prefix.length)) })),
        cursor: listed.truncated ? listed.cursor : null });
    }
    if (parts[1] !== 'files' || parts.length < 4) return 回({ error: '接口不存在' }, 404);
    const path = 安全文件路径(parts.slice(2).join('/'));
    if (!种类们.has(path.split('/')[0])) 错('文件分类无效');
    const key = 'releases/' + id + '/' + path;
    if (req.method === 'PUT') return 回({ error: '逐文件上传已关闭，请重新上传同一 ZIP 补传' }, 410);
    if (!['GET', 'HEAD'].includes(req.method)) return 回({ error: '不支持的方法' }, 405);
    if (!await 查版本(env, id)) return 回({ error: '版本不存在' }, 404);
    const object = await env.PACKAGES.get(key);
    if (!object) return 回({ error: '文件暂不可用，上传者可继续补传' }, 404);
    return new Response(req.method === 'HEAD' ? null : object.body, { headers: {
      'Content-Type': 'application/octet-stream', 'Content-Length': String(object.size),
      'Content-Disposition': "attachment; filename*=UTF-8''" + encodeURIComponent(path.split('/').at(-1)),
      'Cache-Control': 'public, max-age=31536000, immutable', 'X-Content-Type-Options': 'nosniff',
      'Content-Security-Policy': "sandbox; default-src 'none'", ETag: object.httpEtag,
    } });
  } catch (e) { return 回({ error: e.status ? e.message : '发布服务暂不可用' }, e.status || 503); }
}

// 古曰：客文居别域，不得借主站之权。今释：此入口不调用账户或容器，也不传播存储对象的任意 HTTP 元数据。
export async function 用户内容入口(req, env) {
  const headers = {
    'Content-Security-Policy': "sandbox allow-scripts; default-src 'none'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; font-src 'self'; connect-src 'self'; base-uri 'none'; form-action 'none'; frame-ancestors " + 门户来源(env),
    'X-Content-Type-Options': 'nosniff', 'Referrer-Policy': 'no-referrer',
    'Access-Control-Allow-Origin': '*', 'Cache-Control': 'no-store',
  };
  const unavailable = (status = 404) => new Response('文档暂不可用，上传者可以继续补传。', { status, headers: { ...headers, 'Content-Type': 'text/plain; charset=utf-8' } });
  if (!['GET', 'HEAD'].includes(req.method)) return unavailable(405);
  try {
    const url = new URL(req.url), parts = url.pathname.split('/').slice(1), id = parts.shift();
    if (!编号式.test(id || '')) return unavailable();
    const path = 安全文件路径(parts.join('/'));
    if (!await 查版本(env, id)) return unavailable();
    if(path==='阅读')return 阅读入口(req,env,id,headers);
    const object = await env.PACKAGES.get('releases/' + id + '/docs/' + path);
    if (!object) return unavailable();
    const mime = { html: 'text/html; charset=utf-8', htm: 'text/html; charset=utf-8',
      js: 'text/javascript; charset=utf-8', mjs: 'text/javascript; charset=utf-8',
      css: 'text/css; charset=utf-8', json: 'application/json', txt: 'text/plain; charset=utf-8',
      png: 'image/png', jpg: 'image/jpeg', jpeg: 'image/jpeg', gif: 'image/gif', svg: 'image/svg+xml',
      webp: 'image/webp', woff: 'font/woff', woff2: 'font/woff2', ico: 'image/x-icon' }[path.split('.').at(-1).toLowerCase()];
    if (!mime) return unavailable(415);
    if(mime.startsWith('text/html')){
      const html=req.method==='HEAD'?null:补全阅读页(await new Response(object.body).text(),env,id,url.searchParams.get('file')||'');
      return new Response(html,{headers:{...headers,'Content-Type':mime,'Cache-Control':'no-store'}});
    }
    return new Response(req.method === 'HEAD' ? null : object.body, { headers: { ...headers, 'Content-Type': mime,
      'Content-Length': String(object.size), 'Cache-Control': 'public, max-age=31536000, immutable' } });
  } catch { return unavailable(); }
}
