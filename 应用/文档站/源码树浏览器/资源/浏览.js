// 文言：旧锚仍至其书，目录随所阅而展。汉语：兼容旧单页链接，只跳转到页面已有的文件链接，并展开当前文件目录。
function 定位源码() {
  const 当前 = document.querySelector('.source-document');
  if (!当前) return;
  let 锚点;
  try { 锚点 = decodeURIComponent(location.hash.slice(1)); } catch { return; }
  const 链接们 = [...document.querySelectorAll('.tree-link')];
  const 目标 = 链接们.find(链接 => {
    try { return decodeURIComponent(new URL(链接.href).hash.slice(1)) === 锚点; } catch { return false; }
  });
  if (目标 && 锚点 !== 当前.id) { location.replace(目标.href); return; }
  for (const 链接 of 链接们) {
    const 选中 = decodeURIComponent(new URL(链接.href).hash.slice(1)) === 当前.id;
    链接.classList.toggle('is-default', 选中);
    if (选中) {
      链接.setAttribute('aria-current', 'page');
      for (let 父 = 链接.parentElement; 父; 父 = 父.parentElement) if (父.tagName === 'DETAILS') 父.open = true;
      const 永久链接 = 当前.querySelector('.source-link');
      if (永久链接) 永久链接.href = 链接.href;
    } else 链接.removeAttribute('aria-current');
  }
}
window.addEventListener('hashchange', 定位源码);
定位源码();
