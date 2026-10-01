// 文言：客需与宿供各立簿，先核公约及客器之形，后许启行。
// 汉语：应用要求与宿主支持独立入包；装载前核对接口清单及 Wasm 宿主桥形状。
const 须 = (条件, 消息) => {
  if (!条件) throw new Error(`豫言操作系统装载失败：${消息}`);
};

const 身份 = 项 => `${项.接口所有者}/${项.接口名称}/${项.接口版本}`;
const 依赖身份 = 项 => `${项.所有者}/${项.名称}/${项.版本}`;
const 函数身份 = 项 => `${项.模块}/${项.函数}/${项.方向}/${项.签名}`;

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

// 文言：豫言之客，或有边界段，或导入旧通调；诸导入皆函，或为旧通调，或为工具模块、平台接口包与应用所需接口包之带型导入，且边界段录其签名。
// 汉语：是豫言程序（带「豫言边界」段，或导入旧的 yuyan:gc-host/v1.call），且每个导入都是函数导入，要么是过渡期的旧 call，
//   要么是「豫言边界」段里有签名的带类型导入、导入模块是工具模块（标准库、构建基础）、平台接口包或应用要求的接口包。
//   返回全部不合之处（空表即合），一次列全。
const 边界段名 = '豫言边界';
const 工具模块们 = ['标准库', '构建基础'];
// 文言：平台接口包：宿主自具之能，非可移植之约，适配与平台之库导入之；宿主未实者可给桩。新平台之包增于此。
// 汉语：平台接口包：各宿主自带的平台能力（不是可移植接口），由适配或平台库导入；宿主没有实现的可以给桩。新增平台接口包时加在这里。
export const 平台接口包们 = ['云工宿主', '浏览器宿主', '诺节宿主', '中央张量宿主', '系统库调用', '安全外壳密码'];
function 导入问题(导入们, 边界文们, 可导入模块们) {
  const 有签名 = new Set();
  for (const 文 of 边界文们) {
    for (const 行 of 文.split('\n')) {
      const 列 = 行.split('\t');
      if (列[0] === '导入' && 列.length === 4) 有签名.add(列[1] + '\t' + 列[2]);
    }
  }
  const 是通调 = 项 => 项.module === 'yuyan:gc-host/v1' && 项.name === 'call' && 项.kind === 'function';
  if (边界文们.length === 0 && !导入们.some(是通调)) return ['不是豫言程序（既没有「豫言边界」段，也不导入 yuyan:gc-host/v1.call）'];
  const 问题 = [];
  for (const 项 of 导入们) {
    if (是通调(项)) continue;
    const 名 = 项.module + '.' + 项.name;
    if (项.kind !== 'function') 问题.push(名 + '（不是函数导入）');
    else if (!有签名.has(项.module + '\t' + 项.name)) 问题.push(名 + '（「豫言边界」段里没有它的签名）');
    else if (!可导入模块们.has(项.module)) 问题.push(名 + '（导入模块不是标准库、构建基础、平台接口包或应用要求的接口包）');
  }
  return 问题;
}

// 文言：客器不得列模块之表时，直析 Wasm 之导入、导出与边界段，以同法核之。汉语：浏览器无法调用 Module.imports/exports 时，从同一模块字节读出导入、导出与自定义段「豫言边界」，按同样的规则核对。
function 核对Wasm字节形状(原字节) {
  const 字节 = 原字节 instanceof Uint8Array ? 原字节 : new Uint8Array(原字节);
  须(字节.length >= 8 && [0,97,115,109,1,0,0,0].every((值, 序) => 字节[序] === 值), 'Wasm 字节头无效');
  let 位 = 8, 导入 = null, 有导出节 = false;
  const 导出名们 = new Set();
  const 边界文们 = [];
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
      导入 = [];
      const 数 = 整数(界);
      for (let 序 = 0; 序 < 数; 序++) {
        const 模块名 = 名称(界), 函数名 = 名称(界);
        须(位 < 界, 'Wasm 导入节不完整');
        const 种类 = 字节[位++];
        导入.push({module: 模块名, name: 函数名, kind: 种类 === 0 ? 'function' : '其他'});
        // 文言：非函之导入其述长短不一，遇之即止，核必不合。汉语：非函数导入的描述长短不一，遇到就停止读取（核对必然不通过）。
        if (种类 !== 0) break;
        整数(界);
      }
      须(导入.at(-1)?.kind === '其他' || 位 === 界, 'Wasm 导入节尾部无效');
    } else if (节 === 0) {
      if (名称(界) === 边界段名) 边界文们.push(new TextDecoder('utf-8', {fatal: true}).decode(字节.subarray(位, 界)));
    } else if (节 === 7) {
      须(!有导出节, 'Wasm 导出节重复');
      有导出节 = true;
      const 数 = 整数(界);
      for (let 序 = 0; 序 < 数; 序++) {
        const 名 = 名称(界);
        须(位 < 界, 'Wasm 导出节不完整');
        const 种类 = 字节[位++];
        整数(界);
        if (种类 === 0) 导出名们.add(名);
      }
      须(位 === 界, 'Wasm 导出节尾部无效');
    }
    位 = 界;
  }
  return {导入们: 导入 ?? [], 边界文们, 导出名们};
}

// 文言：带型实现者，宿主带型导入之实之表（模块名 → 字段名 → 函），可不授；授之，则核可移植之接口为宿主所尽实。
// 汉语：带型实现（可选）：宿主的带类型导入实现表 {模块名: {字段名: 函数}}；给了就核对应用要求的可移植接口（豫言操作系统…）的导入全部由宿主实现。
// 文言：应用提供者，应用所供接口之簿（如启动），可不授；授之，则核其术皆有导出。
// 汉语：应用提供（可选）：应用提供的接口清单组（如启动）；给了就核对其中方向为“应用”的每个函数都有导出 接口名称/函数名（见网页汇编接口网五）。
export function 核对接口装载({程序模块, 程序字节, 应用要求, 宿主提供, 宿主, 带型实现, 应用提供}) {
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
  // 文言：导入或为旧通调，或为工具模块、平台接口包与应用所需接口包之带型导入；工具模块与平台接口包，宿主未实者可给桩。
  // 汉语：导入只能是过渡期的旧 yuyan:gc-host/v1.call，或工具模块（标准库、构建基础）、平台接口包与应用要求的接口包的带类型导入；
  //   工具模块与平台接口包宿主没有实现的给桩，调用时才报错。
  let 导入们, 边界文们, 导出名们;
  try {
    导入们 = WebAssembly.Module.imports(程序模块);
    边界文们 = WebAssembly.Module.customSections(程序模块, 边界段名).map(段 => new TextDecoder('utf-8').decode(段));
    导出名们 = new Set(WebAssembly.Module.exports(程序模块).filter(项 => 项.kind === 'function').map(项 => 项.name));
  } catch (错) {
    须(宿主 === '浏览器' && (程序字节 instanceof ArrayBuffer || ArrayBuffer.isView(程序字节)),
      '浏览器不能读取 Wasm 导入导出且缺少原始字节');
    ({导入们, 边界文们, 导出名们} = 核对Wasm字节形状(程序字节));
  }
  const 要求名们 = new Set(应用要求.map(项 => 项.接口名称));
  const 问题 = 导入问题(导入们, 边界文们, new Set([...工具模块们, ...平台接口包们, ...要求名们]));
  须(问题.length === 0, 'Wasm 宿主导入形状不符：' + 问题.join('；'));
  // 文言：可移植之接口，宿主须尽实之，不许给桩；授带型实现乃核，缺者一次列全。
  // 汉语：应用要求的可移植接口的带类型导入，宿主必须全部实现、不许给桩；给了 带型实现 才核对，缺的一次列全。
  if (带型实现 !== undefined) {
    const 缺 = 导入们
      .filter(项 => 项.kind === 'function' && 要求名们.has(项.module) && typeof 带型实现?.[项.module]?.[项.name] !== 'function')
      .map(项 => 项.module + '.' + 项.name);
    须(缺.length === 0, '宿主没有实现可移植接口的这些函数：' + 缺.join('、'));
  }
  须(导出名们.has('_start'), 'Wasm 缺少程序启动导出');
  // 文言：应用所供之接口，其术皆须有导出（接口名称/函数名）；缺者一次列全。汉语：应用提供的接口函数都须有导出 接口名称/函数名，缺的一次列全。
  if (应用提供 !== undefined) {
    须(Array.isArray(应用提供), '应用提供的接口清单组格式不符');
    const 缺 = 应用提供.flatMap(项 => (Array.isArray(项?.函数) ? 项.函数 : []).filter(函 => 函.方向 === '应用')
      .map(函 => 项.接口名称 + '/' + 函.函数)).filter(名 => !导出名们.has(名));
    须(缺.length === 0, 'Wasm 缺少应用提供的接口导出：' + 缺.join('、'));
  }
  return true;
}
