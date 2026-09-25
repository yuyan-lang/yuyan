// 文言：客需与宿供各立簿，先核公约及客器之形，后许启行。
// 汉语：应用要求与宿主支持独立入包；装载前核对接口清单及 Wasm 宿主桥形状。
const 须 = (条件, 消息) => {
  if (!条件) throw new Error(`豫言操作系统装载失败：${消息}`);
};

const 身份 = 项 => `${项.接口所有者}/${项.接口名称}/${项.接口版本}`;
const 依赖身份 = 项 => `${项.所有者}/${项.名称}/${项.版本}`;
const 函数身份 = 项 => `${项.模块}/${项.函数}/${项.方向}/${项.签名}`;
const 摘要格式 = /^[0-9a-f]{64}$/u;

function 核对清单(清单) {
  须(清单 && 清单.格式版本 === 4, '接口清单格式不符');
  须(清单.接口所有者 === '豫言' && /^豫言操作系统/u.test(清单.接口名称) &&
    /^\d+\.\d+\.\d+$/u.test(清单.接口版本), '接口包身份不符');
  const 根 = 依赖身份({所有者: 清单.接口所有者, 名称: 清单.接口名称, 版本: 清单.接口版本});
  须(Array.isArray(清单.解析闭包) && 清单.解析闭包.length > 0 &&
    依赖身份(清单.解析闭包[0]) === 根, `解析闭包不含接口本身：${根}`);
  const 闭包 = 清单.解析闭包.map(依赖身份);
  须(new Set(闭包).size === 闭包.length, `解析闭包重复：${根}`);
  须(Array.isArray(清单.直接依赖) && 清单.直接依赖.every(项 => 闭包.includes(依赖身份(项))),
    `直接依赖未解析：${根}`);
  须(Array.isArray(清单.规范摘要) && 清单.规范摘要.length > 0, `缺少规范摘要：${根}`);
  const 文件 = new Set();
  for (const 项 of 清单.规范摘要) {
    须(typeof 项.文件 === 'string' && 项.文件.length > 0 && !文件.has(项.文件) &&
      项.算法 === 'SHA-256' && 摘要格式.test(项.摘要), `规范摘要格式不符：${根}`);
    文件.add(项.文件);
  }
  须(文件.has(`包/${清单.接口名称}。包。豫`) &&
    [...文件].some(名 => /^包\/[^/]+。接口。豫$/u.test(名)), `缺少接口规范正文：${根}`);
  须(Array.isArray(清单.函数) && 清单.函数.length > 0, `缺少接口函数：${根}`);
  const 函数 = 清单.函数.map(函数身份);
  须(new Set(函数).size === 函数.length && 清单.函数.every(项 =>
    (项.方向 === '宿主' || 项.方向 === '应用') &&
    typeof 项.签名 === 'string' && 项.签名.startsWith('→[')), `接口函数格式不符：${根}`);
  return 根;
}

function 正规化(值) {
  if (Array.isArray(值)) return 值.map(正规化);
  if (值 && typeof 值 === 'object') return Object.fromEntries(
    Object.entries(值).sort(([左], [右]) => 左.localeCompare(右, 'zh')).map(([键, 项]) => [键, 正规化(项)]));
  return 值;
}

// 文言：客器不得列模块之表时，直析 Wasm 导入导出节，仍核唯一通桥与启口。汉语：浏览器无法调用 Module.imports/exports 时，从同一模块字节核对导入和启动导出。
function 核对Wasm字节形状(原字节) {
  const 字节 = 原字节 instanceof Uint8Array ? 原字节 : new Uint8Array(原字节);
  须(字节.length >= 8 && [0,97,115,109,1,0,0,0].every((值, 序) => 字节[序] === 值), 'Wasm 字节头无效');
  let 位 = 8, 导入 = null, 有启动 = false, 有导出节 = false;
  const 整数 = 界 => {
    let 值 = 0;
    for (let 次 = 0; 次 < 5; 次++) {
      须(位 < 界, 'Wasm 节长度无效');
      const 字 = 字节[位++];
      值 += (字 & 127) * 2 ** (次 * 7);
      须(Number.isSafeInteger(值) && 值 <= 0xffffffff, 'Wasm 整数越界');
      if (!(字 & 128)) return 值;
    }
    throw Error('豫言操作系统装载失败：Wasm 整数过长');
  };
  const 名称 = 界 => {
    const 长 = 整数(界);
    须(位 + 长 <= 界, 'Wasm 名称越界');
    const 文 = new TextDecoder('utf-8', {fatal: true}).decode(字节.subarray(位, 位 + 长));
    位 += 长;
    return 文;
  };
  while (位 < 字节.length) {
    const 节 = 字节[位++], 长 = 整数(字节.length), 界 = 位 + 长;
    须(界 <= 字节.length, 'Wasm 节长度无效');
    if (节 === 2) {
      须(导入 === null, 'Wasm 导入节重复');
      const 数 = 整数(界);
      须(数 === 1, 'Wasm 宿主导入形状不符');
      const 模块名 = 名称(界), 函数名 = 名称(界);
      须(位 < 界, 'Wasm 导入节不完整');
      const 种类 = 字节[位++];
      整数(界);
      导入 = 模块名 === 'yuyan:gc-host/v1' && 函数名 === 'call' && 种类 === 0;
      须(位 === 界, 'Wasm 导入节尾部无效');
    } else if (节 === 7) {
      须(!有导出节, 'Wasm 导出节重复');
      有导出节 = true;
      const 数 = 整数(界);
      for (let 序 = 0; 序 < 数; 序++) {
        const 名 = 名称(界);
        须(位 < 界, 'Wasm 导出节不完整');
        const 种类 = 字节[位++];
        整数(界);
        if (名 === '_start' && 种类 === 0) 有启动 = true;
      }
      须(位 === 界, 'Wasm 导出节尾部无效');
    }
    位 = 界;
  }
  return {导入, 有启动};
}

export function 核对接口装载({程序模块, 程序字节, 应用要求, 宿主提供, 宿主}) {
  须(程序模块 instanceof WebAssembly.Module, '程序不是 Wasm 模块');
  须(宿主 === '浏览器' || 宿主 === '云工' || 宿主 === '节点', '宿主身份无效');
  须(Array.isArray(应用要求) && Array.isArray(宿主提供), '接口清单组格式不符');
  const 支持 = new Map();
  for (const 项 of 宿主提供) {
    const 键 = 核对清单(项);
    须(!支持.has(键), `宿主接口重复：${键}`);
    支持.set(键, 项);
  }
  const 已需 = new Set();
  for (const 项 of 应用要求) {
    const 键 = 核对清单(项);
    须(!已需.has(键), `应用接口重复：${键}`);
    已需.add(键);
    const 宿主项 = 支持.get(键);
    须(宿主项 !== undefined, `宿主不支持接口：${键}`);
    须(JSON.stringify(正规化(项)) === JSON.stringify(正规化(宿主项)), `接口规范或签名不一致：${键}`);
  }
  // 文言：此时诸术皆投一通桥，故仅能验通桥之形；逐术施行另由运行验收证之。
  // 汉语：当前编译器把源级宿主函数统一投影到一个导入；Wasm 表不能反推出各函数名。
  let 导入正确, 有启动;
  try {
    const 导入 = WebAssembly.Module.imports(程序模块);
    导入正确 = 导入.length === 1 && 导入[0].module === 'yuyan:gc-host/v1' &&
      导入[0].name === 'call' && 导入[0].kind === 'function';
    有启动 = WebAssembly.Module.exports(程序模块).some(项 => 项.name === '_start' && 项.kind === 'function');
  } catch (错) {
    须(宿主 === '浏览器' && (程序字节 instanceof ArrayBuffer || ArrayBuffer.isView(程序字节)),
      '浏览器不能读取 Wasm 导入导出且缺少原始字节');
    ({导入: 导入正确, 有启动} = 核对Wasm字节形状(程序字节));
  }
  须(导入正确, 'Wasm 宿主导入形状不符');
  须(有启动, 'Wasm 缺少程序启动导出');
  return true;
}
