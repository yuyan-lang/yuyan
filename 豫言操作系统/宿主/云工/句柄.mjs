// 文言：客但执号，不见 JS 物；用毕释之。汉语：豫言只持有不透明编号，JS 对象留在宿主并可显式释放。
// 文言：待办之事：宿主库百余原语仍以字符串句柄传物，先改为资源（边界之“资”形，客执 externref），乃可去此表。
// 汉语：待办事项：宿主库还有一百多个原语用字符串句柄传对象，先把它们改成资源（边界的“资”形，客体直接持有 externref），才能删去这张句柄表。
const 禁名 = new Set(['__proto__', 'prototype', 'constructor', 'eval', 'Function', 'AsyncFunction', 'GeneratorFunction']);
const 允名 = 名 => {
  if (typeof 名 !== 'string' || !名 || 禁名.has(名)) throw Error('不允许访问宿主成员：' + String(名));
  return 名;
};

// 文言：柄之限从宽，同浏览器之宿；巨篇成页一事可执数千柄。汉语：句柄上限与浏览器宿主相同（409600）；包管理服务为大源文件生成一页时，单个事件就会持有数千个句柄。
export function 创建句柄表({上限 = 409600} = {}) {
  const 表 = new Map();
  const 反查 = new Map();
  // 文言：号不可推，则应用之 JSON 不能伪造宿主物之引。汉语：句柄号取密码学随机的 53 位安全整数，而非自增序号；应用传入的 JSON 里即使写了 {"$句柄":"1"} 也指不到任何现存句柄。
  const 随机号 = () => {
    for (;;) {
      const 缓 = new Uint32Array(2);
      crypto.getRandomValues(缓);
      const 号 = (缓[0] & 0x1fffff) * 4294967296 + 缓[1];
      if (号 > 2 ** 40 && !表.has(号)) return 号;
    }
  };
  const 登记 = 值 => {
    if ((typeof 值 !== 'object' || 值 === null) && typeof 值 !== 'function' && typeof 值 !== 'symbol') throw Error('仅对象或符号可登记句柄');
    const 旧号 = 反查.get(值);
    if (旧号 && 表.has(旧号)) return String(旧号);
    if (表.size >= 上限) throw Error('宿主句柄达到上限');
    const 号 = 随机号();
    表.set(号, 值);
    反查.set(值, 号);
    return String(号);
  };
  const 取得 = 号 => {
    const 序 = Number(号);
    if (!Number.isSafeInteger(序) || !表.has(序)) throw Error('宿主句柄无效：' + String(号));
    return 表.get(序);
  };
  const 释放 = 号 => {
    const 序 = Number(号);
    const 值 = 取得(号);
    表.delete(序);
    if (反查.get(值) === 序) 反查.delete(值);
  };
  const 入 = (值, 深 = 0) => {
    if (深 > 32) throw Error('宿主参数嵌套过深');
    if (Array.isArray(值)) return 值.map(项 => 入(项, 深 + 1));
    if (值 && typeof 值 === 'object') {
      if (Object.keys(值).length === 1 && Object.hasOwn(值, '$句柄')) return 取得(值.$句柄);
      if (Object.keys(值).length === 1 && Object.hasOwn(值, '$未定义')) return undefined;
      if (Object.keys(值).length === 1 && Object.hasOwn(值, '$大整数')) {
        if (typeof 值.$大整数 !== 'string' || !/^-?[0-9]{1,40}$/.test(值.$大整数)) throw Error('宿主大整数记号格式无效');
        return BigInt(值.$大整数);
      }
      if (Object.keys(值).length === 1 && Object.hasOwn(值, '$数字')) {
        if (!['NaN', 'Infinity', '-Infinity'].includes(值.$数字)) throw Error('宿主数字记号只可为 NaN、Infinity、-Infinity');
        return Number(值.$数字);
      }
      return Object.fromEntries(Object.entries(值).map(([名, 项]) => [名, 入(项, 深 + 1)]));
    }
    return 值;
  };
  const 出 = (值, 深 = 0) => {
    if (深 > 32) throw Error('宿主结果嵌套过深');
    if (值 === undefined) return {$未定义: true};
    if (typeof 值 === 'bigint') return {$大整数: String(值)};
    if (typeof 值 === 'number' && !Number.isFinite(值)) return {$数字: String(值)};
    if (typeof 值 === 'symbol') return {$句柄: 登记(值)};
    if (值 === null || typeof 值 !== 'object' && typeof 值 !== 'function') return 值;
    if (Array.isArray(值)) return 值.map(项 => 出(项, 深 + 1));
    const 原型 = Object.getPrototypeOf(值);
    if (原型 === Object.prototype || 原型 === null) {
      return Object.fromEntries(Object.entries(值).map(([名, 项]) => [名, 出(项, 深 + 1)]));
    }
    return {$句柄: 登记(值)};
  };
  const 参数 = 文 => {
    const 值 = JSON.parse(文);
    if (!Array.isArray(值)) throw Error('宿主方法参数须为 JSON 数组');
    return 入(值);
  };
  return {登记, 取得, 释放, 入, 出, 参数, 允名, 数量: () => 表.size};
}
