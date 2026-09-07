import { 查会话 } from './账户.js';

// 古曰：内服定籍与权，外桥惟送字节。今释：版本写入和发布权限由豫言处理；此桥负责流式 R2、公开查询和浏览器隔离。
export const 门户来源 = env => env.PORTAL_ORIGIN || 'https://packages.yuyan-lang.org';
export const 内容来源 = env => env.USERCONTENT_ORIGIN || 'https://usercontent.yuyan-lang.org';
const 编号式 = /^[a-f0-9]{32}$/;
const 种类们 = new Set(['source', 'build', 'readme', 'docs', 'runtime']);
const 回 = (data, status = 200) => Response.json(data, { status, headers: { 'Cache-Control': 'no-store', 'X-Content-Type-Options': 'nosniff' } });
const 错 = (message, status = 400) => { throw Object.assign(new Error(message), { status }); };
export function 安全文件路径(raw) {
  let path;
  try { path = decodeURIComponent(raw).normalize('NFC'); } catch { 错('文件路径编码错误'); }
  if (!path || path.length > 1024 || /[\\\x00-\x1f\x7f]/u.test(path) ||
      path.split('/').some(p => !p || p === '.' || p === '..' || p.includes(':'))) 错('文件路径无效');
  return path;
}
const 编路径 = path => path.split('/').map(encodeURIComponent).join('/');
const 版本查询 = 'SELECT v."编号" AS id,u."名称" AS owner,v."名称" AS name,v."版本" AS version,v."类型" AS type,v."简介" AS description,v."创建时间" AS created FROM "即时版本" v JOIN "用户" u ON u."编号"=v."所有者编号"';
async function 查版本(env, id) { return env.DB.prepare(版本查询 + ' WHERE v."编号"=?').bind(id).first(); }
async function 有限正文(req) {
  if (!req.headers.get('Content-Type')?.startsWith('application/json')) 错('需要 JSON 元数据');
  if (!req.body) 错('缺少元数据');
  const reader = req.body.getReader(), parts = []; let size = 0;
  while (true) {
    const { done, value } = await reader.read(); if (done) break;
    size += value.byteLength;
    if (size > 4096) { await reader.cancel(); 错('元数据超过 4096 字节', 413); }
    parts.push(value);
  }
  const bytes = new Uint8Array(size); let n = 0;
  for (const p of parts) { bytes.set(p, n); n += p.length; }
  return new TextDecoder('utf-8', { fatal: true }).decode(bytes);
}
async function 请求内服(req, env, action, body) {
  if (env.REGISTRATION_ENABLED === 'false') return 回({ error: '发布暂未开放' }, 503);
  const origin = req.headers.get('Origin');
  if (origin && origin !== new URL(req.url).origin) return 回({ error: '拒绝跨站发布' }, 403);
  const headers = new Headers({ 'Content-Type': 'application/json', 'Content-Length': String(new TextEncoder().encode(body).byteLength) });
  // 古曰：主号由桥重定，不信客首。今释：绝不转发客户端伪造的发布者或内部路径标头。
  if (req.headers.has('Authorization')) headers.set('Authorization', req.headers.get('Authorization'));
  else {
    if (origin !== new URL(req.url).origin) return 回({ error: '会话发布需要同源 Origin' }, 403);
    const user = await 查会话(req, env.DB, Math.floor(Date.now() / 1000));
    if (!user) return 回({ error: '请先登录' }, 401);
    if (!user.emailVerified) return 回({ error: '请先验证邮箱' }, 403);
    headers.set('X-Yuyan-Publisher', String(user.id));
  }
  return env.PACKAGE_CONTAINER.getByName('豫言包管理').fetch(new Request('http://container.internal/__direct/' + action, {
    method: 'POST', headers, body,
  }));
}
function 限长流(body, expected) {
  let size = 0;
  return body.pipeThrough(new TransformStream({
    transform(chunk, ctrl) { size += chunk.byteLength; if (size > expected) throw new Error('实际长度超过声明'); ctrl.enqueue(chunk); },
    flush() { if (size !== expected) throw new Error('实际长度不足'); },
  }));
}
export async function 即时发布入口(req, env) {
  const url = new URL(req.url);
  if (!url.pathname.startsWith('/api/releases')) return null;
  try {
    const parts = url.pathname.split('/').slice(3), id = parts[0];
    if (url.pathname === '/api/releases') {
      if (req.method === 'POST') {
        const res = await 请求内服(req, env, 'create', await 有限正文(req));
        if (!res.ok) return res;
        const result = await res.json();
        return 回({ ...result, url: '/release/' + result.id }, 201);
      }
      if (req.method !== 'GET') return 回({ error: '不支持的方法' }, 405);
      const offset = Number(url.searchParams.get('offset') || 0);
      if (!Number.isSafeInteger(offset) || offset < 0 || offset > 1000000) 错('分页参数错误');
      const result = await env.DB.prepare(版本查询 + ' ORDER BY v."创建时间" DESC,v."编号" DESC LIMIT 50 OFFSET ?').bind(offset).all();
      return 回({ releases: result.results, nextOffset: result.results.length === 50 ? offset + 50 : null });
    }
    if (!编号式.test(id || '')) return 回({ error: '版本不存在' }, 404);
    if (parts.length === 1 && req.method === 'GET') {
      const version = await 查版本(env, id);
      if (!version) return 回({ error: '版本不存在' }, 404);
      const prefix = 'releases/' + id + '/';
      const listed = await env.PACKAGES.list({ prefix, limit: 1000, cursor: url.searchParams.get('cursor') || undefined });
      return 回({ ...version, review: 'unreviewed', docsUrl: 内容来源(env) + '/' + id + '/index.html',
        files: listed.objects.map(o => ({ path: o.key.slice(prefix.length), size: o.size,
          url: '/api/releases/' + id + '/files/' + 编路径(o.key.slice(prefix.length)) })),
        cursor: listed.truncated ? listed.cursor : null });
    }
    if (parts[1] !== 'files' || parts.length < 4) return 回({ error: '接口不存在' }, 404);
    const path = 安全文件路径(parts.slice(2).join('/'));
    if (!种类们.has(path.split('/')[0])) 错('文件分类无效');
    const key = 'releases/' + id + '/' + path;
    if (req.method === 'PUT') {
      const size = Number(req.headers.get('X-Package-Size')), hash = req.headers.get('X-Package-SHA256') || '';
      const max = Number(env.MAX_RELEASE_FILE_BYTES || 104857600);
      if (!req.headers.has('X-Package-Size') || (!req.body && size !== 0) || !Number.isSafeInteger(size) || size < 0 || size > max || !/^[a-f0-9]{64}$/.test(hash)) 错('文件大小或 SHA256 无效');
      if (req.headers.has('Content-Encoding')) 错('不接受 Content-Encoding');
      const permission = await 请求内服(req, env, 'authorize', JSON.stringify({ id }));
      if (!permission.ok) return permission;
      const existing = await env.PACKAGES.head(key);
      if (existing) return existing.size === size && existing.customMetadata?.sha256 === hash
        ? 回({ ok: true, alreadyUploaded: true }) : 回({ error: '已成功上传的文件不可覆盖' }, 409);
      try {
        const body = req.body || new ReadableStream({ start(c) { c.close(); } });
        const controller = new AbortController();
        const fixed = typeof FixedLengthStream === 'function' ? new FixedLengthStream(size) : null;
        const transfer = fixed ? body.pipeTo(fixed.writable, { signal: controller.signal }) : Promise.resolve();
        transfer.catch(() => {});
        let object;
        try { object = await env.PACKAGES.put(key, fixed ? fixed.readable : 限长流(body, size), {
          onlyIf: new Headers({ 'If-None-Match': '*' }), sha256: hash,
          httpMetadata: { contentType: 'application/octet-stream' }, customMetadata: { sha256: hash },
        }); if (object) await transfer; }
        finally { controller.abort(); }
        if (!object) return 回({ error: '已有并发上传完成，请刷新后重试' }, 409);
        return 回({ ok: true }, 201);
      } catch { return 回({ error: '文件未完成上传或摘要不匹配，可以重试' }, 400); }
    }
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
    const object = await env.PACKAGES.get('releases/' + id + '/docs/' + path);
    if (!object) return unavailable();
    const mime = { html: 'text/html; charset=utf-8', htm: 'text/html; charset=utf-8',
      js: 'text/javascript; charset=utf-8', mjs: 'text/javascript; charset=utf-8',
      css: 'text/css; charset=utf-8', json: 'application/json', txt: 'text/plain; charset=utf-8',
      png: 'image/png', jpg: 'image/jpeg', jpeg: 'image/jpeg', gif: 'image/gif', svg: 'image/svg+xml',
      webp: 'image/webp', woff: 'font/woff', woff2: 'font/woff2', ico: 'image/x-icon' }[path.split('.').at(-1).toLowerCase()];
    if (!mime) return unavailable(415);
    return new Response(req.method === 'HEAD' ? null : object.body, { headers: { ...headers, 'Content-Type': mime,
      'Content-Length': String(object.size), 'Cache-Control': 'public, max-age=31536000, immutable' } });
  } catch { return unavailable(); }
}
