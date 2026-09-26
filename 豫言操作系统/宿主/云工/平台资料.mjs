// 文言：平台所供之只读资料集，宿主一取而缓之，惟留客所当见之键；客但依键取文，不于程序中解全包。
// 汉语：云工宿主的「平台资料」能力（接口见 豫言操作系统接口/平台资料）。资料集 `标准库` = 许可授权的服务绑定 BROWSER_COMPILER 上的
// https://playground.yuyan-lang.org/编译/资源/标准库.json.gz（gzip 压缩的 JSON：路径 → 文字 或 {内容}，解压约 57 MB）；宿主解压解析后只保留
// 键以 `/库/标准库/` 起首且值是字符串（或带字符串 `内容` 的对象）的条目，其余立即丢弃，结果按宿主实例缓存。本文件不依赖 宿主.mjs 的任何内部名字：
// 宿主.mjs 在 创建云工宿主 里调用 创建平台资料 一次，并在能力表里登记两个原语（见文末注释与 说明.汉语.md）。
const 编码器 = new TextEncoder();
const 字节长 = 文 => 编码器.encode(文).length;

// 文言：限额同公约。汉语：与接口规范一致的限额。
export const 平台资料限额 = Object.freeze({集名字节: 64, 前缀字节: 512, 键字节: 512, 键数: 4096, 列表字节: 1048576, 文字字节: 2097152});

// 文言：集名与其源皆宿主所定，客不得自择网址。汉语：资料集到服务绑定与网址的映射由宿主固定，应用不能自选来源。
export const 默认资料集 = Object.freeze({
  标准库: Object.freeze({
    绑定: 'BROWSER_COMPILER',
    网址: 'https://playground.yuyan-lang.org/编译/资源/标准库.json.gz',
    前缀: '/库/标准库/'
  })
});

const 字节序比较 = (甲, 乙) => {
  const 左 = 编码器.encode(甲), 右 = 编码器.encode(乙), 共 = Math.min(左.length, 右.length);
  for (let 位 = 0; 位 < 共; 位++) if (左[位] !== 右[位]) return 左[位] - 右[位];
  return 左.length - 右.length;
};
const 含控制字符 = 文 => /[\u0000-\u001f\u007f]/u.test(文);
const 参数无效 = () => Error('平台资料参数无效');
const 验集名 = 名 => {
  if (typeof 名 !== 'string' || !名 || 字节长(名) > 平台资料限额.集名字节 || 含控制字符(名)) throw 参数无效();
  return 名;
};
const 验前缀 = 前缀 => {
  if (typeof 前缀 !== 'string' || 字节长(前缀) > 平台资料限额.前缀字节) throw 参数无效();
  return 前缀;
};
const 验键 = 键 => {
  if (typeof 键 !== 'string' || !键 || 字节长(键) > 平台资料限额.键字节) throw 参数无效();
  return 键;
};

// 文言：一宿主一器，缓存与之同寿；许可与环境由每事件所传。
// 汉语：创建平台资料器。选项 全局（默认 globalThis，测试可传假的 Request/Response/DecompressionStream）与 资料集（默认 默认资料集）。
// 返回 {列键, 读文字, 列键安全, 读文字安全}：环境是当前事件的 Worker env，许可是应用许可（读其 SERVICE 列表）。
export function 创建平台资料({全局 = globalThis, 资料集 = 默认资料集} = {}) {
  const 缓存 = new Map();
  const 载入 = (环境, 许可, 集名) => {
    if (!Object.hasOwn(资料集, 集名)) throw Error('未知的平台资料集：' + 集名);
    const 定义 = 资料集[集名];
    if (!Array.isArray(许可?.SERVICE) || !许可.SERVICE.includes(定义.绑定)) throw Error('未授权的平台资料集：' + 集名);
    const 服务 = 环境?.[定义.绑定];
    if (!服务 || typeof 服务.fetch !== 'function') throw Error(集名 + '资料不可用');
    const 旧 = 缓存.get(集名);
    if (旧) return 旧;
    const 新 = (async () => {
      try {
        const 回应 = await 服务.fetch(new 全局.Request(定义.网址));
        if (!回应.ok) throw Error('状态' + 回应.status);
        const 全 = await new 全局.Response(回应.body.pipeThrough(new 全局.DecompressionStream('gzip'))).json();
        const 表 = new Map();
        for (const 键 of Object.keys(全)) {
          if (!键.startsWith(定义.前缀) || 字节长(键) > 平台资料限额.键字节) continue;
          const 值 = 全[键], 文 = typeof 值 === 'string' ? 值 : (值 && typeof 值.内容 === 'string' ? 值.内容 : null);
          if (文 === null || 字节长(文) > 平台资料限额.文字字节) continue;
          表.set(键, 文);
        }
        const 键们 = [...表.keys()].sort(字节序比较);
        if (键们.length > 平台资料限额.键数) throw Error('键数超过上限');
        return {表, 键们};
      } catch { throw Error(集名 + '资料不可用'); }
    })();
    缓存.set(集名, 新);
    新.catch(() => { if (缓存.get(集名) === 新) 缓存.delete(集名); });
    return 新;
  };
  const 列键 = async (环境, 许可, 集名, 前缀) => {
    验集名(集名); 验前缀(前缀);
    const {键们} = await 载入(环境, 许可, 集名);
    const 文 = JSON.stringify(键们.filter(键 => 键.startsWith(前缀)));
    if (字节长(文) > 平台资料限额.列表字节) throw Error(集名 + '资料不可用');
    return 文;
  };
  const 读文字 = async (环境, 许可, 集名, 键) => {
    验集名(集名); 验键(键);
    const {表} = await 载入(环境, 许可, 集名);
    return 表.has(键) ? [true, 表.get(键)] : [false, ''];
  };
  const 错文 = 错 => String(错?.message ?? 错);
  return {
    列键,
    读文字,
    // 文言：宿主之败作值而返，不令 JS 异越 Wasm 之界。汉语：适配只用安全变体：列键安全 返回 [是否成功, JSON 文字或错误文]；读文字安全 返回 [状态, 文字或错误文]（0 存在、1 不存在、2 失败）。
    列键安全: async (环境, 许可, 集名, 前缀) => {
      try { return [true, await 列键(环境, 许可, 集名, 前缀)]; } catch (错) { return [false, 错文(错)]; }
    },
    读文字安全: async (环境, 许可, 集名, 键) => {
      try { const [在, 文] = await 读文字(环境, 许可, 集名, 键); return [在 ? 0 : 1, 文]; } catch (错) { return [2, 错文(错)]; }
    }
  };
}

// 文言：接入宿主.mjs 之法，惟三处。汉语：把本能力接入 宿主.mjs 只需三处（由主代理落实，见 说明.汉语.md）：
// 1. 文件头：import {创建平台资料} from './平台资料.mjs';
// 2. 创建云工宿主 函数体里、`const 执行 = async …` 之前（宿主级，跨事件共享缓存）：const 平台资料 = 创建平台资料({全局});
// 3. 能力表里（`豫言_云工_服务请求文字` 附近）加两项：
//    豫言_云工_平台资料列键安全: async (集, 前缀) => 平台资料.列键安全(环境, 许可, 文字(集), 文字(前缀)),
//    豫言_云工_平台资料读文字安全: async (集, 键) => 平台资料.读文字安全(环境, 许可, 文字(集), 文字(键)),
// 另：工具/双宿主构建/入口。豫 复制宿主文件的清单里要加一行 「抄文」于『平台资料.mjs』。
