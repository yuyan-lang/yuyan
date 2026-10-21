// 文言：网页行在之声出（提案〇〇〇四三）：行在（主线程）持一声境与工作件（AudioWorklet）；每流一环形之缓，
//   工作线程书之于外，工作件于音之线程读而奏之。浏览器之自动播禁须先有手势，故初按一键或一点时复其声境。
// 汉语：网页行在的声音输出（提案 00043）：行在（主线程）持一个 AudioContext 与声音工作件（AudioWorklet）；
//   每条流一个环形缓冲，工作线程在另一侧写入，工作件在音频线程里读出播放。浏览器的自动播放规定要求页面先有用户手势，
//   所以页面在第一次按键或点击时恢复这个 AudioContext。工作线程侧见 ../构建/豫寓.mjs，工作件见同目录的 声音工作件.mjs。
const 控制字 = 8;

// 文言：造声音行在：根(document) 与 全局(globalThis) 供听手势、造声境。所还可 操作(记录号, 操作, …参) 与 结进程(记录号)。
// 汉语：造声音行在：根（document）与 全局（globalThis）用来听用户手势、造 AudioContext。返回「操作」(记录号, 操作, …参)
//   与「结进程」(记录号)：进程结束时关掉它开着的流，免得声境与工作件留着。
export function 造声音行在({根 = globalThis, 全局 = globalThis} = {}) {
  const 流表 = new Map();
  let 下号 = 1, 语境 = null, 载入 = null, 已交互 = false;

  // 文言：复其声境：浏览器惟于手势之后许其发声。汉语：恢复声境：浏览器只在用户手势之后才允许它发声。
  const 恢复 = () => {
    已交互 = true;
    if (语境?.state === 'suspended') 语境.resume().catch(() => {});
  };
  for (const 事 of ['pointerdown', 'keydown']) 根?.addEventListener?.(事, 恢复, {capture: true, passive: true});

  // 文言：造声境一次：首流之请率为其率，浏览器不受则用其器之率；工作件一并取来。汉语：只造一次 AudioContext：
  //   第一个流请求的采样率作它的采样率，浏览器不接受就用设备自己的；工作件模块也在这里取来。
  const 保证语境 = 率 => {
    if (语境) return 载入;
    const 类 = 全局.AudioContext ?? 全局.webkitAudioContext;
    if (typeof 类 !== 'function') throw Error('资源暂不可用：浏览器不支持 Web Audio');
    try { 语境 = new 类({sampleRate: 率}); } catch { 语境 = new 类(); }
    if (已交互) 语境.resume().catch(() => {});
    载入 = 语境.audioWorklet.addModule(new URL('./声音工作件.mjs', import.meta.url));
    return 载入;
  };

  // 文言：开一流：环与容量由工作线程造而送来，工作件据此环出声；设备延迟之帧书于控制之字。汉语：打开一条流：
  //   环形缓冲与其容量由工作线程造好送来，工作件按这个缓冲出声；设备延迟折算成帧写进控制字。
  const 打开 = async (环, 率, 道, 容量, 记录号) => {
    const 率数 = Number(率), 道数 = Number(道), 容 = Number(容量);
    if (!(环 instanceof SharedArrayBuffer) || !(率数 >= 1 && 率数 <= 768000) || !(道数 >= 1 && 道数 <= 2)) return [1, [6]];
    if (!(容 >= 2 && 容 <= 1048576) || (容 & (容 - 1)) !== 0) return [1, [6]];
    try {
      await 保证语境(率数);
      const 控制 = new Int32Array(环, 0, 控制字);
      Atomics.store(控制, 2, Math.max(0, Math.round((语境.outputLatency ?? 语境.baseLatency ?? 0) * 率数)));
      const 节点 = new AudioWorkletNode(语境, '豫寓鸣', {numberOfInputs: 0, numberOfOutputs: 1, outputChannelCount: [道数],
        processorOptions: {环, 容量: 容, 道: 道数, 源率: 率数}});
      节点.connect(语境.destination);
      const 号 = 下号++;
      流表.set(号, {节点, 记录号});
      return [0, 号];
    } catch (错) {
      // 文言：留其误于案头，便于调试（浏览器无音频设备、工作件取不来等）。汉语：把错误留在控制台，便于调试（浏览器没有音频设备、工作件取不来等）。
      (全局.console ?? console).error?.('声音输出：打开失败：', 错);
      return [1, [1]];
    }
  };

  const 关 = 号 => {
    const 条 = 流表.get(Number(号));
    if (!条) return [1, [2]];
    流表.delete(Number(号));
    try { 条.节点.disconnect(); } catch {}
    return [0];
  };

  // 文言：行一国之声务。汉语：办一项声音事务（工作线程经同步提问送来）。
  const 操作 = async (记录号, 操作, 甲, 乙, 丙, 环) => {
    if (操作 === '打开') return 打开(环, 甲, 乙, 丙, 记录号);
    if (操作 === '关闭') return 关(甲);
    throw Error('未知的声音操作：' + 操作);
  };

  const 结进程 = 记录号 => {
    for (const [号, 条] of 流表) if (条.记录号 === 记录号) 关(号);
  };

  return {操作, 结进程, 状态: () => ({语境: 语境?.state ?? '未造', 流数: 流表.size})};
}
