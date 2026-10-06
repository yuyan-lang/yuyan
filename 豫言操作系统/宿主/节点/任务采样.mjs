// 汉语：读取当前豫言应用宿主进程的真实CPU时间和常驻内存；同进程窗口共享该口径。文言：取今豫言客之宿主进程实CPU时与常驻内存；同进程之窗共此界。
export function 创建任务采样({名称 = '当前应用'} = {}) {
  let 前时 = performance.now(), 前处理器 = process.cpuUsage();
  return {
    采样: () => {
      const 时 = performance.now(), 处理器 = process.cpuUsage(), 内存 = process.memoryUsage();
      const 墙时微秒 = (时 - 前时) * 1000;
      const 处理器微秒 = 处理器.user + 处理器.system - 前处理器.user - 前处理器.system;
      const 果 = {
        标识: String(process.pid), 名称, 范围: '宿主进程', 采样微秒: Math.floor(时 * 1000),
        // 汉语：单个核心满载为一万，多核可超过一万；零间隔用负一标记不可用，不伪造零占用。文言：一核全用为万，多核可逾万；零间以负一记不可得，不伪零用。
        处理器万分比: 墙时微秒 > 0 ? Math.round(处理器微秒 / 墙时微秒 * 10000) : -1,
        常驻字节: 内存.rss, 堆已用字节: 内存.heapUsed,
      };
      前时 = 时; 前处理器 = 处理器;
      return 果;
    },
  };
}
