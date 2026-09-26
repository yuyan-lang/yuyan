// 文言：此器仿一慢上游：请以 id、ms 二参，候若干毫秒而答；凡始终皆记，供验事之并发或独占。
// 汉语：模拟出站网络：GET https://slow.test/hold?id=甲&ms=50 等待 ms 毫秒后返回 ok，并按序记录“开始/结束”事件，
// 测试据此判断请求区间是否重叠（并发）或不重叠（独占）。可传给 创建云工宿主({网络}) 的 网络 参数。
export function 创建模拟慢网络() {
  const 记录 = [];
  const 取回 = async (网址, 选项 = {}) => {
    const 址 = new URL(String(网址));
    const 编号 = 址.searchParams.get('id') ?? '';
    const 毫秒 = Number(址.searchParams.get('ms') ?? 0);
    记录.push({事件: '开始', 编号, 时: performance.now(), 方法: 选项.method ?? 'GET'});
    await new Promise(完成 => setTimeout(完成, 毫秒));
    记录.push({事件: '结束', 编号, 时: performance.now()});
    return new Response('ok');
  };
  // 文言：某编号之诸区间；同号多次则并列。汉语：返回某个 id 的全部 [开始时, 结束时] 区间。
  const 区间 = 编号 => {
    const 出 = [];
    let 始 = null;
    for (const 项 of 记录) {
      if (项.编号 !== 编号) continue;
      if (项.事件 === '开始') 始 = 项.时;
      else if (始 !== null) { 出.push([始, 项.时]); 始 = null; }
    }
    return 出;
  };
  // 文言：二区间相交乎。汉语：判断两个 id 的区间是否有重叠（严格相交）。
  const 相交 = (甲, 乙) => 区间(甲).some(([甲始, 甲终]) => 区间(乙).some(([乙始, 乙终]) => 甲始 < 乙终 && 乙始 < 甲终));
  return {fetch: 取回, 记录, 区间, 相交, 清空: () => { 记录.length = 0; }};
}
