// 文言：客桥惟呈豫言已定之色；入编则见素稿，出编乃异步呈色。
// 汉语：页面只展示豫言高亮库返回的 HTML；编辑时使用原生 textarea，失焦后异步显示高亮。
import {高亮豫言源码} from './桥接.mjs';

const 转义 = 文 => 文.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');

export function 启动页面高亮(文树 = document) {
  for (const 节点 of 文树.querySelectorAll('[data-yuyan-highlight], #首页示例')) {
    let 版本 = 0;
    let 已见 = 节点.textContent;
    // 文言：记所呈之文；客器复书同文，色随之去，亦当重呈。汉语：记下本脚本写入的内容；应用写回同样的源码也会冲掉着色，这时同样要重新着色。
    let 已呈 = null;
    const 更新 = async () => {
      const 本版 = ++版本;
      const 原文 = 节点.textContent;
      已见 = 原文;
      try {
        const 超文本 = await 高亮豫言源码(原文);
        if (本版 === 版本 && 节点.textContent === 原文) { 节点.innerHTML = 超文本; 已呈 = 节点.innerHTML; }
      } catch { if (本版 === 版本) { 节点.innerHTML = 转义(原文); 已呈 = 节点.innerHTML; } }
    };
    void 更新();
    if (节点.id === '首页示例') {
      new MutationObserver(() => { if (节点.textContent !== 已见 || 节点.innerHTML !== 已呈) { ++版本; queueMicrotask(() => void 更新()); } })
        .observe(节点, {childList: true, characterData: true});
    }
  }

  const 编辑器 = 文树.getElementById('源码');
  const 预览 = 文树.getElementById('源码着色');
  if (!编辑器 || !预览) return;
  let 版本 = 0;
  const 展示 = async () => {
    const 本版 = ++版本;
    const 原文 = 编辑器.value;
    try {
      const 超文本 = await 高亮豫言源码(原文);
      if (本版 !== 版本 || 文树.activeElement === 编辑器 || 编辑器.value !== 原文) return;
      预览.innerHTML = 超文本;
    } catch {
      if (本版 !== 版本 || 文树.activeElement === 编辑器) return;
      预览.textContent = 原文;
    }
    预览.hidden = false;
    编辑器.hidden = true;
  };
  编辑器.addEventListener('focus', () => { ++版本; 编辑器.hidden = false; 预览.hidden = true; });
  编辑器.addEventListener('blur', () => { void 展示(); });
  编辑器.addEventListener('input', () => { ++版本; });
  预览.addEventListener('click', () => { 编辑器.hidden = false; 预览.hidden = true; 编辑器.focus(); });
  预览.addEventListener('keydown', 事件 => { if (事件.key === 'Enter' || 事件.key === ' ') { 事件.preventDefault(); 编辑器.hidden = false; 预览.hidden = true; 编辑器.focus(); } });
  const 示例 = 文树.getElementById('示例');
  示例?.addEventListener('change', () => {
    ++版本;
    const 原文 = 编辑器.value;
    let 次数 = 0;
    const 等待 = setInterval(() => {
      if (编辑器.value !== 原文 || ++次数 >= 20) {
        clearInterval(等待);
        if (文树.activeElement !== 编辑器) void 展示();
      }
    }, 50);
  });
  // 文言：初稿既入，始呈其色。汉语：等待豫言应用写入默认示例后再显示初始高亮。
  const 初稿 = setInterval(() => { if (编辑器.value) { clearInterval(初稿); if (文树.activeElement !== 编辑器) void 展示(); } }, 100);
}

启动页面高亮();
