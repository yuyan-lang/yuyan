// 汉语：用户态子程序只运行宿主具名授权的节点发行入口；输入、环境随调用传入，不保留跨调用状态。文言：用户态子程序惟行宿主具名所授之节点发行入口；输入、环境随调而入，不留跨调之态。
import {spawnSync as 同步启动, spawn as 原生服务启动} from 'node:child_process';

const 解码 = new TextDecoder('utf-8', {fatal: true});
const 空字节 = () => new Uint8Array();
const 失败 = (码, 文) => [码, -1, 空字节(), 空字节(), 文];
export function 创建子程序能力({程序 = new Map(), 环境 = new Set(), 上限 = 16 * 1024 * 1024, 暂停输入 = () => () => {}} = {}) {
  return {
    运行: (名, 参数, 输入, 环境项们, 继承输入 = false) => {
      if (!程序.has(名)) return 失败(1, '子程序未获授权：' + 名);
      if (!Array.isArray(参数) || 参数.some(值 => typeof 值 !== 'string' || 值.includes('\0')) ||
          !(输入 instanceof Uint8Array) || !Array.isArray(环境项们)) return 失败(7, '子程序参数无效');
      if (输入.length > 上限) return 失败(6, '子程序输入超过交换上限');
      const 环境值 = Object.create(null);
      for (const 项 of 环境项们) {
        if (!Array.isArray(项) || 项.length !== 2 || 项.some(值 => typeof 值 !== 'string' || 值.includes('\0')) ||
            !项[0] || 项[0].includes('=') || Object.hasOwn(环境值, 项[0])) return 失败(7, '子程序环境条目无效');
        if (!环境.has(项[0])) return 失败(1, '子程序环境未获授权：' + 项[0]);
        环境值[项[0]] = 项[1];
      }
      // 汉语：待办事项：节点选项类环境变量应由公共进程规范统一限制；本实现直接拒绝，避免改变子宿主的装载方式。文言：待办事项：节点选项类环境当由公进程规范一统限之；今直拒，以免易子宿主之装载。
      if (Object.hasOwn(环境值, 'NODE_OPTIONS') || Object.hasOwn(环境值, 'NODE_PATH')) return 失败(7, '子程序环境不得改变节点装载选项');
      const 授 = 程序.get(名);
      try {
        const 恢复输入 = 继承输入 ? 暂停输入() : () => {};
        try {
          const 环境授权参数 = Object.keys(环境值).flatMap(名 => ['--允许环境', 名]);
          const 果 = 同步启动(process.execPath, [授.入口, ...(授.宿主参数 ?? []), ...环境授权参数, '--', ...参数], {
            cwd: 授.目录, env: 环境值, input: 继承输入 ? undefined : 输入, maxBuffer: 上限,
            stdio: [继承输入 ? 'inherit' : 'pipe', 'pipe', 'pipe'],
            // 汉语：当前执行顺序与原壳一致；待办事项：异步启动与取消。文言：今循原壳顺次之行；待办事项：异步启与取消。
            timeout: 授.时限 ?? 120000, windowsHide: true,
          });
          if (果.error) return 失败(果.error.code === 'ENOBUFS' ? 6 : 8, String(果.error.message));
          if (果.signal || 果.status === null) return 失败(8, '子程序被信号终止：' + 果.signal);
          return [0, 果.status, new Uint8Array(果.stdout), new Uint8Array(果.stderr), ''];
        } finally { 恢复输入(); }
      } catch (错) { return 失败(8, String(错?.message ?? 错)); }
    },
    // 汉语：文本输入适配须明确解码失败，不能静默替换字节。文言：文本输入之适配须明报解码之败，不暗易字节。
    解码输出: 字节 => 解码.decode(字节),
  };
}

// 汉语：节点只转发标准 JSON，请求在豫言原生服务中完成授权检查与进程操作。文言：节点惟转标准 JSON，其授之验与进程之事皆成于豫言原生服务。
export function 创建原生子程序桥({服务路径, 程序 = new Map(), 环境 = new Set(), 当前目录 = process.cwd()} = {}) {
  let 服务 = null, 待答 = [], 行缓 = '', 已闭 = false;
  const 失败果 = () => ({状态: '失败', 错误码: 服务路径 ? 29 : 58});
  function 断开() {
    已闭 = true;
    for (const 答 of 待答.splice(0)) 答(失败果());
  }
  function 开启() {
    if (服务) return true;
    if (!服务路径 || 已闭) return false;
    const 参数 = [];
    for (const [名, 项] of 程序) {
      const 入口 = typeof 项 === 'string' ? 项 : 项.入口;
      参数.push('--授权子程序', 名 + '=' + (/\.mjs$/i.test(入口) ? process.execPath : 入口));
    }
    for (const 名 of 环境) 参数.push('--允许环境', 名);
    try {
      服务 = 原生服务启动(服务路径, 参数, {cwd: 当前目录, stdio: ['pipe', 'pipe', 'pipe']});
      服务.stdout.setEncoding('utf8');
      服务.stdout.on('data', 块 => {
        行缓 += 块;
        let 位;
        while ((位 = 行缓.indexOf('\n')) >= 0) {
          const 行 = 行缓.slice(0, 位); 行缓 = 行缓.slice(位 + 1);
          const 答 = 待答.shift();
          if (!答) continue;
          try { 答(JSON.parse(行)); } catch { 答(失败果()); }
        }
      });
      服务.stderr.resume();
      服务.on('error', 断开);
      服务.on('close', 断开);
      服务.stdin.on('error', 断开);
      return true;
    } catch { 断开(); return false; }
  }
  function 请求(请) {
    if (!开启()) return Promise.resolve(失败果());
    return new Promise(答 => {
      待答.push(答);
      服务.stdin.write(JSON.stringify(请) + '\n');
    });
  }
  return {
    async 启动(名, 参数, 输入, 环境项们) {
      const 项 = 程序.get(名);
      const 入口 = typeof 项 === 'string' ? 项 : 项?.入口;
      const 参数们 = 入口 && /\.mjs$/i.test(入口) ? [入口, '--', ...参数] : 参数;
      const 果 = await 请求({动作: '启动', 名称: 名, 参数: 参数们, 输入: Array.from(输入), 环境: 环境项们});
      return 果.状态 === '已启动' ? [0, 果.句柄] : [Number(果.错误码 ?? 29), -1];
    },
    async 收取(柄) {
      const 果 = await 请求({动作: '收取', 句柄: Number(柄)});
      if (果.状态 === '运行中') return [0, 0, new Uint8Array(), new Uint8Array()];
      if (果.状态 === '完成') return [1, 果.退出码, Uint8Array.from(果.输出), Uint8Array.from(果.错误)];
      return [-Number(果.错误码 ?? 29), 0, new Uint8Array(), new Uint8Array()];
    },
    async 终止(柄) {
      const 果 = await 请求({动作: '终止', 句柄: Number(柄)});
      return Number(果.错误码 ?? 29);
    },
    // 汉语：资源查询由同一个豫言原生服务执行，节点只传五字段。文言：资源之查询，同一豫言原生之服行之；节点惟传五项。
    async 资源(柄) {
      const 果 = await 请求({动作: '资源', 句柄: Number(柄)});
      return [Number(typeof 果.状态 === 'number' ? 果.状态 : 果.错误码 ?? 29), 果.当前可用 === true, BigInt(果.当前字节 ?? 0), 果.峰值可用 === true, BigInt(果.峰值字节 ?? 0)];
    },
    关闭() { if (服务 && !已闭) 服务.stdin.end(); }
  };
}
