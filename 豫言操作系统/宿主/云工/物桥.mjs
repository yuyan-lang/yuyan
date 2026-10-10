// 文言：薄宿主之通用原语：客以资执 JS 之物，宿主惟译名与值，不存按事之态。
// 汉语：薄宿主的通用原语（库/云工宿主/物宿主。接口。豫）：豫言用资源「云工物」直接持有 JS 对象，宿主只翻译名字和值，不保存任何按事件或按调用的状态。
//   云工宿主与 Node 应用宿主共用本模块；平台语义都写在豫言里。
import {文字} from './值桥.mjs';

// 文言：云工值之支序，同 物宿主。接口。豫 构造器之次。汉语：「云工值」的支序，与 物宿主。接口。豫 里构造器的次序一致。
const 空 = 0, 未定义 = 1, 爻 = 2, 整 = 3, 数 = 4, 大整数 = 5, 文 = 6, 字节 = 7, 物 = 8;
const 禁名 = new Set(['__proto__', 'prototype', 'constructor', 'eval', 'Function', 'AsyncFunction', 'GeneratorFunction']);

// 文言：客之值入宿主。汉语：边界给来的云工值 [支序, 载荷] 还原成 JS 值（整为 BigInt，串为 Uint8Array 副本）。
export const 值入 = 值 => {
  switch (Number(值[0])) {
    case 空: return null;
    case 未定义: return undefined;
    case 爻: return Boolean(值[1]);
    case 整: {
      const 数字 = Number(值[1]);
      if (!Number.isSafeInteger(数字)) throw RangeError('云工整值超出 JS 安全整数范围');
      return 数字;
    }
    case 数: return Number(值[1]);
    case 大整数: return BigInt(值[1]);
    case 文: return 文字(值[1]);
    case 字节: return 值[1] instanceof Uint8Array ? 值[1] : new Uint8Array(值[1]);
    case 物: return 值[1];
    default: throw Error('云工值支序无效：' + String(值[0]));
  }
};

// 文言：宿主之值出于客。汉语：JS 值转成云工值 [支序, 载荷]；安全整数给整值，Uint8Array 与 ArrayBuffer 复制成字节值。
export const 值出 = 值 => {
  if (值 === null) return [空];
  if (值 === undefined) return [未定义];
  switch (typeof 值) {
    case 'boolean': return [爻, 值];
    case 'number': return Number.isSafeInteger(值) ? [整, BigInt(值)] : [数, 值];
    case 'bigint': return [大整数, 值];
    case 'string': return [文, 值];
  }
  if (值 instanceof Uint8Array) return [字节, 值.slice()];
  if (值 instanceof ArrayBuffer) return [字节, new Uint8Array(值.slice(0))];
  return [物, 值];
};

const 成 = 值 => [0, 值出(值)];
const 败 = 错 => [1, String(错?.name ?? 'Error'), String(错?.message ?? 错)];
const 名 = 值 => {
  const 名称 = 文字(值);
  if (!名称 || 禁名.has(名称)) throw Error('不允许访问宿主成员：' + 名称);
  return 名称;
};
const 守 = 函 => { try { return 成(函()); } catch (错) { return 败(错); } };
const 守候 = async 函 => { try { return 成(await 函()); } catch (错) { return 败(错); } };
const 调 = (物, 法, 参们) => {
  const 法名 = 名(法);
  const 函 = 物[法名];
  if (typeof 函 !== 'function') throw TypeError('宿主对象没有方法：' + 法名);
  return Reflect.apply(函, 物, 参们.map(值入));
};

// 文言：造通用原语之表。汉语：造通用原语实现表（键为 物宿主。接口。豫 的函数名）。
//   取全局(名) 返回白名单里的全局对象，否则抛错；取绑定(类, 名) 按许可清单返回平台绑定，否则抛错；
//   回调(号, 实参云工值列) 以 执行云工回调 进入 Wasm，返回 Promise<云工果>。
export function 创建物桥({取全局, 取绑定, 回调}) {
  return {
    云工全局: 名称 => 守(() => 取全局(文字(名称))),
    云工绑定: (类, 名称) => 守(() => 取绑定(文字(类), 文字(名称))),
    云工取: (对象, 属性) => 守(() => 对象[名(属性)]),
    云工设: (对象, 属性, 值) => 守(() => { 对象[名(属性)] = 值入(值); return undefined; }),
    云工调: (对象, 法, 参们) => 守(() => 调(对象, 法, 参们)),
    云工调候: async (对象, 法, 参们) => 守候(() => 调(对象, 法, 参们)),
    云工呼: (函, 参们) => 守(() => Reflect.apply(函, undefined, 参们.map(值入))),
    云工呼候: async (函, 参们) => 守候(() => Reflect.apply(函, undefined, 参们.map(值入))),
    云工造: (构造器, 参们) => 守(() => Reflect.construct(构造器, 参们.map(值入))),
    云工候: async 值 => 守候(() => 值入(值)),
    云工类型: 值 => Object.prototype.toString.call(值入(值)).slice(8, -1),
    云工是实例: (对象, 构造器) => typeof 构造器 === 'function' && 对象 instanceof 构造器,
    云工新数组: 参们 => 参们.map(值入),
    云工新对象: 项们 => Object.fromEntries(项们.map(([键, 值]) => [名(键), 值入(值)])),
    云工数组项们: 对象 => (Array.isArray(对象) || ArrayBuffer.isView(对象) ? Array.from(对象, 值出) : []),
    云工对象项们: 对象 => Object.entries(对象 ?? {}).map(([键, 值]) => [键, 值出(值)]),
    云工造回调: 号 => async (...实参) => {
      const 果 = await 回调(Number(号), 实参.map(值出));
      if (Number(果[0]) === 0) return 值入(果[1]);
      throw Object.assign(Error(文字(果[2])), {name: 文字(果[1])});
    }
  };
}
