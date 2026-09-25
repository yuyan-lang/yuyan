// 文言：桥惟传稿与成色，色法在豫言；异请各有其号。
// 汉语：浏览器通过事件与豫言 Wasm 交换源码和安全 HTML，不在 JavaScript 中重写着色规则。
let 启动中;
const 待答 = new Map();
let 下号 = 1;

async function 启动() {
  const {启动豫言浏览器应用} = await import('./产物/入口.mjs');
  const 文树 = document;
  const 回答 = 事件 => 已成(事件.detail?.编号, 事件.detail?.超文本);
  文树.addEventListener('豫言高亮完成', 回答);
  let 宿主;
  try { 宿主 = await 启动豫言浏览器应用({路径: new URL('./产物/', import.meta.url)}); }
  catch (错误) { 文树.removeEventListener('豫言高亮完成', 回答); throw 错误; }
  const 收束 = 错误 => {
    文树.removeEventListener('豫言高亮完成', 回答);
    for (const 项 of 待答.values()) {clearTimeout(项.计时); 项.失败(错误);}
    待答.clear();
    启动中 = undefined;
  };
  宿主.完成.then(() => 收束(new Error('豫言高亮客器已退出')), 收束);
  await 宿主.就绪;
  return 宿主;
}

function 已成(编号, 超文本) {
  const 项 = 待答.get(编号);
  if (!项) return;
  待答.delete(编号); clearTimeout(项.计时);
  if (typeof 超文本 === 'string') 项.完成(超文本);
  else 项.失败(new Error('豫言高亮响应无效'));
}

export async function 高亮豫言源码(源码) {
  if (typeof 源码 !== 'string') throw new TypeError('源码必须是字符串');
  if (!启动中) 启动中 = 启动().catch(错 => {启动中 = undefined; throw 错;});
  await 启动中;
  const 编号 = 下号++;
  return new Promise((完成, 失败) => {
    const 计时 = setTimeout(() => { 待答.delete(编号); 失败(new Error('豫言高亮等待超时')); }, 30000);
    待答.set(编号, {完成, 失败, 计时});
    document.dispatchEvent(new CustomEvent('豫言高亮请求', {detail: {编号, 源码}}));
  });
}

export function 成色词元(超文本, 文树 = globalThis.document) {
  const 模板 = 文树.createElement('template'); 模板.innerHTML = 超文本;
  const 结果 = []; let 偏移 = 0;
  const 遍 = 节点 => {
    if (节点.nodeType === 文树.defaultView.Node.TEXT_NODE) {
      const text = 节点.textContent || '', kind = 节点.parentElement?.closest('[class^="tok-"]')?.className?.slice(4) || 'plain';
      if (text) { 结果.push({kind, text, start: 偏移, end: 偏移 + text.length}); 偏移 += text.length; }
    } else for (const 子 of 节点.childNodes) 遍(子);
  };
  遍(模板.content);
  return 结果;
}
