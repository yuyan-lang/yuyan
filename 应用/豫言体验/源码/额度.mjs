// 古曰：总数共计，客簿分藏，易日而更其簿。
// 今释：事务原子扣减额度；访客分 256 个桶，避免单个 KV 值超出大小上限。
export class Quota {
  constructor(状态, 环境) { this.状态 = 状态; this.环境 = 环境; }
  async fetch(请求) {
    const { ip, action } = await 请求.json();
    if (!/^[0-9a-f]{64}$/.test(ip) || !["assist", "run"].includes(action)) return new Response(null, { status: 400 });
    const 日 = new Date().toISOString().slice(0, 10), 分 = Math.floor(Date.now() / 60000);
    const 总限 = Number(action === "assist" ? this.环境.DAILY_AI_LIMIT : this.环境.DAILY_RUN_LIMIT);
    if (!Number.isSafeInteger(总限) || 总限 < 1 || 总限 > 10000) return Response.json({ error: "服务额度须为 1 至 10000" }, { status: 503 });
    return this.状态.storage.transaction(async 存储 => {
      let 账 = await 存储.get("总账"), 桶 = await 存储.get(`客簿-${ip.slice(0, 2)}`);
      if (账?.day !== 日) 账 = { day: 日, assist: 0, run: 0 };
      if (桶?.day !== 日) 桶 = { day: 日, visitors: {} };
      const 客 = 桶.visitors[ip] ?? { minute: 分, count: 0, assist: 0, run: 0 };
      if (客.minute !== 分) { 客.minute = 分; 客.count = 0; }
      if (账[action] >= 总限 || 客[action] >= (action === "assist" ? 10 : 50) || 客.count >= 6)
        return Response.json({ error: "试用额度已用尽，请稍后再试" }, { status: 429, headers: { "Cache-Control": "no-store" } });
      客.count++; 客[action]++; 账[action]++; 桶.visitors[ip] = 客;
      await 存储.put({ "总账": 账, [`客簿-${ip.slice(0, 2)}`]: 桶 });
      return Response.json({ ok: true });
    });
  }
}
