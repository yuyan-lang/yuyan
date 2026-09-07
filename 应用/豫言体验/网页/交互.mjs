// 古曰：新稿验成，即易今稿。
// 今释：AI 生成验证通过的代码后直接替换编辑器内容，无需单独应用。
import { 读取日志流 } from "./日志流.mjs";
import { 示例 as 共用示例 } from "./共用/示例.mjs";
const 取 = 名 => document.getElementById(名);
// 古曰：界面之辞可易，客之所出不易。今释：用独立节点翻译状态说明，原始诊断仍作为文本保留。
function 写界面文字(域, 汉, 文) {
  const 节 = document.createElement("span"); 节.dataset.han = 汉; 节.dataset.wen = 文;
  节.textContent = window.豫言界面?.语言 === "wen" ? 文 : 汉; 域.replaceChildren(节);
}
const 示例们 = {
  你好: "寻观「标准库」之书。\n\n「打印行」于『你好，豫言！』。\n",
  算术: "寻观「标准库」之书。\n\n「答案」者「加」于「三」于「五」也。\n「打印行」于（「整数表示」于「答案」）。\n"
};
Object.assign(示例们, Object.fromEntries(Object.entries(共用示例).map(([名, 例]) => [名, 例.code])));
const 指定示例 = new URL(location.href).searchParams.get("example");
取("源码").value = Object.hasOwn(共用示例, 指定示例) ? 共用示例[指定示例].code : 示例们.你好;
if (Object.hasOwn(共用示例, 指定示例)) 取("示例").value = 指定示例;
let 可用 = false, 有助手 = false, 运行中 = false, 生成中 = false;
let 上个输出, 上个输出键;
let 编译日志长度 = 0;
function 写编译日志(文) {
  const 域 = 取("编译日志");
  const 跟随 = 域.scrollHeight - 域.scrollTop - 域.clientHeight < 50;
  if (编译日志长度 === 0) 域.textContent = "";
  域.append(document.createTextNode(文)); 编译日志长度 += 文.length;
  取("编译日志计数").textContent = `${编译日志长度} 字符`;
  if (跟随) 域.scrollTop = 域.scrollHeight;
}
取("切换编译日志").addEventListener("click", () => {
  const 域 = 取("编译日志"), 钮 = 取("切换编译日志");
  域.hidden = !域.hidden; 钮.textContent = 域.hidden ? "显示" : "隐藏";
  钮.setAttribute("aria-expanded", String(!域.hidden));
});
function 开始日志(标题) {
  // 古曰：客文既至，不复以初辞易之。今释：开始请求后移除占位译文，避免切换语言覆盖真实输出。
  for (const 名 of ["输出", "诊断", "编译日志"]) { delete 取(名).dataset.han; delete 取(名).dataset.wen; }
  编译日志长度 = 0;
  取("编译日志").textContent = "等待编译器输出…";
  取("编译日志计数").textContent = "等待编译";
  取("过程日志").querySelector(".日志空")?.remove(); 上个输出 = null; 上个输出键 = null;
  取("日志摘要").textContent = `${标题} · 等待响应`;
  记日志({ type: "stage", label: `新请求：${标题}，等待编译室响应` });
}
function 记日志(事件) {
  const 日志 = 取("过程日志");
  const 在末尾 = 日志.scrollHeight - 日志.scrollTop - 日志.clientHeight < 80;
  const 时间 = new Date(事件.time || Date.now()).toLocaleTimeString();
  if (事件.type === "stage") {
    取("日志摘要").textContent = 事件.label;
    if (生成中) 取("助手消息").textContent = 事件.label;
    else if (运行中) 取("执行状态").textContent = 事件.phase === "run" ? "运行中" : "编译中";
  }
  if (事件.type === "stage" && 事件.phase === "compile") 写编译日志(`\n[${时间}] ${事件.label}\n`);
  if (事件.type === "output" && 事件.phase === "compile") 写编译日志(事件.text);
  const 键 = `${事件.phase}:${事件.stream}`;
  if (事件.type === "output" && 上个输出 && 上个输出键 === 键) {
    上个输出.append(document.createTextNode(事件.text));
  } else {
    const 项 = document.createElement("article"), 题 = document.createElement("h3"), 文 = document.createElement("pre");
    项.className = `日志项 日志-${事件.type}`;
    const 标签 = 事件.type === "output" ? `${事件.phase === "run" ? "运行" : "编译"} · ${事件.stream}` : 事件.label;
    题.textContent = `${时间} · ${标签 || 事件.type}`; 项.append(题);
    if (事件.text !== undefined || 事件.data !== undefined) {
      文.textContent = 事件.text ?? JSON.stringify(事件.data, null, 2); 项.append(文);
    }
    日志.append(项);
    上个输出 = 事件.type === "output" ? 文 : null; 上个输出键 = 键;
  }
  if (事件.type === "output" && 事件.phase === "run" && 事件.stream === "stdout") {
    if (取("输出").dataset.streaming !== "true") { 取("输出").textContent = ""; 取("输出").dataset.streaming = "true"; }
    取("输出").append(document.createTextNode(事件.text));
  }
  if (在末尾) 日志.scrollTop = 日志.scrollHeight;
}
function 完成日志(值) {
  // 古曰：编之录独陈于侧，勿杂于长篇往来之中。
  // 今释：右侧独立保留编译输出；兼容服务更新期间的一次性 JSON 响应。
  if (编译日志长度 === 0) {
    const 编译 = 值.compilation ?? (值.phase === "compile" ? 值 : null);
    if (编译) 写编译日志([编译.stdout, 编译.stderr, 编译.error].filter(Boolean).join("\n") || "编译器没有文本输出。");
    else 取("编译日志").textContent = 值.error ? "请求未进入编译阶段，详见下方对话与调试日志。" : "本次没有收到编译日志，详见下方对话与调试日志。";
  }
  const 失败 = Boolean(值.error || 值.ok === false || 值.refused);
  取("日志摘要").textContent = 失败 ? "未完成 · 展开查看原因" : "已完成 · 点击隐藏 / 显示";
  if (失败) {
    记日志({ type: "error", label: 值.error || "编译或运行失败", data: 值 });
  }
}
function 刷新() {
  取("运行").disabled = !可用 || 运行中 || 生成中;
  取("生成").disabled = !可用 || !有助手 || 生成中 || 运行中;
  取("字数").textContent = `${new TextEncoder().encode(取("源码").value).length} 字节`;
}
async function 请求(路径, 内容) {
  记日志({ type: "request", label: "浏览器发送", data: { path: 路径, body: 内容 } });
  const 回 = await fetch(路径, { method: "POST", headers: { "Content-Type": "application/json", Accept: "application/x-ndjson" }, body: JSON.stringify(内容), signal: AbortSignal.timeout(600000) });
  const 值 = await 读取日志流(回, 记日志);
  完成日志(值);
  return 值;
}
取("源码").addEventListener("input", 刷新);
取("示例").addEventListener("change", () => { 取("源码").value = 示例们[取("示例").value]; 刷新(); });
document.querySelectorAll(".需求示例").forEach(钮 => 钮.addEventListener("click", () => { 取("需求").value = 钮.dataset.prompt; 取("需求").focus(); }));
取("源码").addEventListener("keydown", 事 => {
  if (事.key === "Tab") { 事.preventDefault(); const 域 = 取("源码"); 域.setRangeText("  ", 域.selectionStart, 域.selectionEnd, "end"); 刷新(); }
  if ((事.metaKey || 事.ctrlKey) && 事.key === "Enter") { 事.preventDefault(); 取("运行").click(); }
});
取("运行").addEventListener("click", async () => {
  运行中 = true; 刷新(); 取("执行状态").textContent = "编译运行中"; 写界面文字(取("输出"), "正在编译并运行…", "正编而行之…"); 写界面文字(取("诊断"), "首次启动可能需要稍等。", "初启须稍候。");
  取("输出").dataset.streaming = "false"; 开始日志("编译与运行");
  try {
    const 值 = await 请求("/api/run", { code: 取("源码").value });
    if (值.stdout) 取("输出").textContent = 值.stdout; else 写界面文字(取("输出"), 值.ok ? "程序正常结束，没有文本输出。" : "没有输出。", 值.ok ? "程序已毕，无文字所出。" : "无所出。");
    写界面文字(取("诊断"), 值.ok ? "编译与运行成功。" : `${值.phase === "compile" ? "编译" : "运行"}未完成。`, 值.ok ? "编行皆成。" : `${值.phase === "compile" ? "编译" : "运行"}未成。`);
    const 原诊 = [值.error, 值.stderr].filter(Boolean).join("\n");
    if (原诊) 取("诊断").prepend(document.createTextNode(原诊 + "\n"));
    取("执行状态").textContent = 值.ok ? "已完成" : "待修正";
  } catch (错) { 完成日志({ error: 错.message }); 取("诊断").textContent = 错.name === "TimeoutError" ? "等待超时，已收到的日志保留在下方。" : 错.message; 取("执行状态").textContent = "暂不可用"; }
  finally { 运行中 = false; 刷新(); }
});
取("生成").addEventListener("click", async () => {
  if (!取("需求").value.trim()) { 取("需求").focus(); return; }
  生成中 = true; 刷新(); 取("助手消息").textContent = "正在理解需求、生成源码并验证编译…";
  开始日志("AI 助写");
  try {
    const 值 = await 请求("/api/assist", { prompt: 取("需求").value, code: 取("源码").value });
    if (!值.code) { 取("助手消息").textContent = 值.error; if (值.diagnostic) 取("诊断").textContent = 值.diagnostic; return; }
    取("源码").value = 值.code;
    写界面文字(取("助手消息"), "代码已通过编译并更新到编辑器。", "新稿已验可编，已易今稿。");
  } catch (错) { 完成日志({ error: 错.message }); 取("助手消息").textContent = 错.name === "TimeoutError" ? "等待超时，已收到的对话保留在下方。" : 错.message; }
  finally { 生成中 = false; 刷新(); }
});
try {
  const 回 = await fetch("/api/status"); if (!回.ok) throw new Error();
  const 状态 = await 回.json(); 可用 = 状态.enabled; 有助手 = 状态.ai;
  取("服务状态").textContent = 可用 ? `编译室已就绪 · ${有助手 ? 状态.model : "AI 待配置"}` : "编译室准备中 · 可先编辑代码";
} catch { 取("服务状态").textContent = "暂未连接到编译室"; }
刷新();
