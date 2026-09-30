// 文言：中央张量之工作线程：受主线所寄之模与共享之存，实例之，告就绪，乃于役板上候役（原子之候，工作线程得为之）。
// 汉语：中央张量内核的工作线程（Web Worker）：收到主线程寄来的多线程版内核模块与共享内存后实例化，回报就绪，然后调用导出的 工作线程(板址, 序号)，在工作板上等工作（工作线程里可以 Atomics.wait）。
self.addEventListener('message', 事 => {
  const {模块, 内存, 板址, 序号} = 事.data ?? {};
  let 实例;
  try {
    实例 = new WebAssembly.Instance(模块, {环境: {内存}});
  } catch (错) {
    self.postMessage({错: String(错?.message ?? 错)});
    return;
  }
  self.postMessage({就绪: true});
  实例.exports.工作线程(板址, 序号);
}, {once: true});
