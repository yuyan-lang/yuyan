// 汉语：节点与浏览器共用具名控制台授权和等待寿命；平台只供行读写，不解析命令。文言：节点与浏览器共具名控制台之授与候寿；平台惟供行读写，不析命令。
export function 创建控制台能力(授权 = new Map()) {
  let 已关闭 = false;
  const 等待们 = new Map();
  const 结果 = (码, 文 = '', 错文 = '') => [码, 文, 错文];
  const 读取 = async 名 => {
    if (已关闭) return 结果(3);
    const 台 = 授权.get(名);
    if (!台 || typeof 台.读取行 !== 'function') return 结果(1);
    if (等待们.has(名)) return 结果(7, '', '同一控制台已有读取等待');
    let 结束;
    const 完成 = new Promise(成 => {结束 = 成;});
    const 候 = {结束}; 等待们.set(名, 候);
    // 汉语：挂起期间关闭可先结束；后台晚到的结果不进入下一读取。文言：悬候之际闭可先终；后至之果不入次读。
    Promise.resolve().then(() => 已关闭 ? [false, ''] : 台.读取行()).then(果 => {
      if (!Array.isArray(果) || typeof 果[0] !== 'boolean' || typeof 果[1] !== 'string') {
        结束(结果(7, '', '控制台行结果无效')); return;
      }
      结束(果[0] ? 结果(0, 果[1].replace(/\r?\n$/, '')) : 结果(4));
    }, 错 => 结束(结果(8, '', String(错?.message ?? 错))));
    try { return await 完成; }
    finally { if (等待们.get(名) === 候) 等待们.delete(名); }
  };
  const 写入 = async (名, 文) => {
    if (已关闭) return 结果(3);
    const 台 = 授权.get(名);
    if (!台 || typeof 台.写文本 !== 'function') return 结果(1);
    if (typeof 文 !== 'string') return 结果(7, '', '控制台正文须为字符串');
    try { await 台.写文本(文); return 结果(0); }
    catch (错) { return 结果(8, '', String(错?.message ?? 错)); }
  };
  const 关闭 = () => {
    if (已关闭) return;
    已关闭 = true;
    for (const 候 of 等待们.values()) 候.结束(结果(4));
    等待们.clear();
    for (const 台 of new Set(授权.values())) 台.关闭?.();
  };
  return {读取, 写入, 关闭};
}
