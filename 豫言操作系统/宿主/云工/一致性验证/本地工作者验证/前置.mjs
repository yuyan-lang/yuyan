import {豫言持久对象} from './产物/入口.mjs';
// 文言：实验之壳，增测平台原语，以验宿主壳之义。汉语：workerd 实验用外壳：在豫言持久对象上增加实例号与若干原生探针路径。
export class Shell extends 豫言持久对象 {
  constructor(ctx, env) { super(ctx, env); this.实例号 = crypto.randomUUID(); this.启于 = Date.now(); }
  async fetch(request) {
    const url = new URL(request.url);
    if (!url.pathname.startsWith('/__')) return super.fetch(request);
    const s = this.ctx.storage;
    switch (url.pathname) {
      case '/__instance': return Response.json({实例号: this.实例号, 启于: this.启于});
      case '/__raw-block-throw': {
        let 错 = null;
        try { await this.ctx.blockConcurrencyWhile(async () => { throw new Error('故意在独占区抛错'); }); } catch (e) { 错 = String(e?.message ?? e); }
        return Response.json({错, 实例号: this.实例号});
      }
      case '/__raw-block-long': {
        const ms = Number(url.searchParams.get('ms') ?? 0);
        const t0 = Date.now();
        let 错 = null;
        try { await this.ctx.blockConcurrencyWhile(async () => { await scheduler.wait(ms); }); } catch (e) { 错 = String(e?.message ?? e); }
        return Response.json({错, 耗: Date.now() - t0, 实例号: this.实例号});
      }
      case '/__raw-tx-alarm': {
        const 结果 = {有txn告警法: null};
        await s.deleteAlarm();
        await s.transaction(async txn => {
          结果.有txn告警法 = [typeof txn.setAlarm, typeof txn.getAlarm, typeof txn.deleteAlarm];
          await txn.put('rawtx', 1);
          await s.setAlarm(Date.now() + 3600_000);
          txn.rollback();
        });
        结果.回滚后告警 = await s.getAlarm();
        结果.回滚后键 = (await s.get('rawtx')) ?? null;
        await s.deleteAlarm();
        return Response.json(结果);
      }
      case '/__raw-tx-alarm2': {
        const 情形 = url.searchParams.get('case');
        const 时 = Date.now() + 3600_000;
        const 出 = {情形};
        await s.deleteAlarm(); await s.delete('direct');
        if (情形 === 'delete-rollback') await s.setAlarm(时);
        try {
          await s.transaction(async txn => {
            if (情形 === 'commit' || 情形 === 'throw' || 情形 === 'rollback' || 情形 === 'readback') await s.setAlarm(时);
            if (情形 === 'txn-set') await txn.setAlarm(时);
            if (情形 === 'delete-rollback') await s.deleteAlarm();
            if (情形 === 'readback') 出.事务内读 = await s.getAlarm();
            if (情形 === 'direct-put') await s.put('direct', 1);
            await txn.put('rawtx', 1);
            if (情形 === 'throw') throw new Error('故意');
            if (情形 === 'rollback' || 情形 === 'delete-rollback' || 情形 === 'direct-put' || 情形 === 'txn-set') txn.rollback();
          });
        } catch (e) { 出.抛 = String(e.message); }
        出.事后告警 = await s.getAlarm();
        出.事后rawtx = (await s.get('rawtx')) ?? null;
        出.事后direct = (await s.get('direct')) ?? null;
        await s.deleteAlarm(); await s.delete('rawtx'); await s.delete('direct');
        return Response.json(出);
      }
      case '/__raw-tx-hold': {
        const ms = Number(url.searchParams.get('ms') ?? 300);
        const t0 = Date.now();
        await s.delete('other'); await s.delete('rawtx');
        await s.transaction(async txn => {
          await txn.put('rawtx', 1);
          await scheduler.wait(ms);
          txn.rollback();
        });
        return Response.json({事务耗: Date.now() - t0, 事后rawtx: (await s.get('rawtx')) ?? null, 事后other: (await s.get('other')) ?? null});
      }
      case '/__raw-put': {
        const t0 = Date.now();
        await s.put(url.searchParams.get('k'), 1);
        return Response.json({耗: Date.now() - t0, 时: Date.now()});
      }
      case '/__raw-bg': {
        const 模式 = url.searchParams.get('mode'), ms = Number(url.searchParams.get('ms') ?? 500);
        const 做 = async () => { await scheduler.wait(ms); await s.put('bg-' + 模式, Date.now()); };
        if (模式 === 'waituntil') this.ctx.waitUntil(做()); else if (模式 === 'none') void 做();
        return new Response('early');
      }
      case '/__raw-getalarm': return Response.json({告警: await s.getAlarm()});
      case '/__raw-get': return Response.json({值: (await s.get(url.searchParams.get('k'))) ?? null});
      case '/__raw-sql': {
        const 行 = s.sql.exec(url.searchParams.get('q')).toArray();
        return Response.json(行);
      }
      default: return new Response('无此探针', {status: 404});
    }
  }
}
export default {
  async fetch(request, env) {
    const url = new URL(request.url);
    const 名 = url.searchParams.get('do') ?? 'a';
    return env.OBJ.getByName(名).fetch(request);
  }
};
