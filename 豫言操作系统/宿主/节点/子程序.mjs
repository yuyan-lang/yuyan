// 汉语：用户态子程序只运行宿主具名授权的节点发行入口；输入、环境随调用传入，不保留跨调用状态。文言：用户态子程序惟行宿主具名所授之节点发行入口；输入、环境随调而入，不留跨调之态。
import {spawnSync as 同步启动} from 'node:child_process';

const 解码 = new TextDecoder('utf-8', {fatal: true});
const 空字节 = () => new Uint8Array();
const 失败 = (码, 文) => [码, -1, 空字节(), 空字节(), 文];
export function 创建子程序能力({程序 = new Map(), 环境 = new Set(), 上限 = 16 * 1024 * 1024} = {}) {
  return {
    运行: (名, 参数, 输入, 环境项们) => {
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
        const 环境授权参数 = Object.keys(环境值).flatMap(名 => ['--允许环境', 名]);
        const 果 = 同步启动(process.execPath, [授.入口, ...(授.宿主参数 ?? []), ...环境授权参数, '--', ...参数], {
          cwd: 授.目录, env: 环境值, input: 输入, maxBuffer: 上限,
          // 汉语：当前执行顺序与原壳一致；待办事项：异步启动与取消。文言：今循原壳顺次之行；待办事项：异步启与取消。
          timeout: 授.时限 ?? 120000, windowsHide: true,
        });
        if (果.error) return 失败(果.error.code === 'ENOBUFS' ? 6 : 8, String(果.error.message));
        if (果.signal || 果.status === null) return 失败(8, '子程序被信号终止：' + 果.signal);
        return [0, 果.status, new Uint8Array(果.stdout), new Uint8Array(果.stderr), ''];
      } catch (错) { return 失败(8, String(错?.message ?? 错)); }
    },
    // 汉语：文本输入适配须明确解码失败，不能静默替换字节。文言：文本输入之适配须明报解码之败，不暗易字节。
    解码输出: 字节 => 解码.decode(字节),
  };
}
