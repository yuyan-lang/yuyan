import { test } from "node:test";
import assert from "node:assert/strict";
import { 助写 } from "../源码/助写.mjs";
import { 构建知识 } from "../源码/知识.mjs";
import { 组模型请求, 验工具参数, 取关键诊断 } from "../源码/模型协议.mjs";

// 古曰：以假使验真约，器不可越其司。今释：模拟连续工具调用，检查上下文保留与执行边界。
const 源码 = "寻观「标准库」之书。\n「打印行」于『你好』。";
const 知识 = 构建知识("指南", { 模块: [{ 路径: "标准库。豫", 声明: [{ 名称: "打印行", 签名: "化字符串而有" }] }] }, { "你好。豫": 源码 });
const 工具答 = (name, args, id) => ({ message: { role: "assistant", content: "", reasoning_content: `保留${id}`, tool_calls: [{ id, type: "function", function: { name, arguments: JSON.stringify(args) } }] } });
test("模型连续查询、读例、编译纠错与运行，工具结果和推理字段保留", async () => {
  let 次 = 0, 编次 = 0; const 事件 = [];
  const 结果 = await 助写({ prompt: "打印你好", code: "" }, 知识, async (消息, 报告, 选项) => {
    const 轮 = 次++;
    if (!轮) { assert.equal(选项.mode, "scope"); return { allow: true }; }
    assert.equal(选项.mode, "agent");
    if (轮 > 1) {
      assert.equal(消息.at(-2).reasoning_content, `保留${轮 - 1}`);
      assert.equal(消息.at(-1).tool_call_id, String(轮 - 1));
    }
    if (轮 === 1) return 工具答("query_library", { query: "打印" }, "1");
    if (轮 === 2) return 工具答("read_example", { name: "你好。豫" }, "2");
    if (轮 === 3) return 工具答("compile_yuyan", { code: "「坏源码」。" }, "3");
    if (轮 === 4) { assert.match(消息.at(-1).content, /语法错误/); return 工具答("run_yuyan", { code: 源码 }, "4"); }
    assert.match(消息.at(-1).content, /你好/);
    return { refuse: false, code: 源码 };
  }, async (文, 仅编译) => {
    编次++;
    if (文 === "「坏源码」。") { assert.equal(仅编译, true); return { ok: false, stderr: "语法错误", phase: "compile" }; }
    assert.equal(仅编译, false); return { ok: true, stdout: "你好", phase: "run" };
  }, 事 => 事件.push(事));
  assert.equal(结果.verified, true); assert.equal(编次, 2);
  assert.equal(事件.filter(事 => 事.type === "tool_call").length, 4);
});
test("模型不能添加终端、扩大工具参数或替换型号", () => {
  assert.throws(() => 验工具参数("shell", { command: "pwd" }));
  assert.throws(() => 验工具参数("run_yuyan", { code: 源码, command: "pwd" }));
  assert.throws(() => 验工具参数("query_library", { limit: 81 }));
  const 参数 = 组模型请求({ mode: "agent", messages: [{ role: "user", content: "任务" }], model: "其他", max_tokens: 999999, tools: [] }, "deepseek-v4-pro");
  assert.equal(参数.model, "deepseek-v4-pro"); assert.equal(参数.tools.length, 4);
  assert.equal(参数.thinking.type, "enabled"); assert.equal(参数.reasoning_effort, "high");
  assert.equal(组模型请求({ mode: "scope", messages: [{}] }, "deepseek-v4-pro").tools, undefined);
});
test("目录查询和示例读取只访问内存中可信资料，诊断保留末尾错误", () => {
  assert.equal(知识.查询({}).total, 1);
  assert.equal(知识.示例({ name: "../../etc/passwd" }).error, "示例不存在");
  assert.equal(知识.查询({ module: "../../etc/passwd" }).error, "模块不存在");
  assert.match(知识.提示, /化字符串而有/);
  assert.match(取关键诊断({ stderr: "进度".repeat(6000) + "error: 最后的真实错误" }), /最后的真实错误/);
});
test("最终源码与已验证版本不同则重新编译", async () => {
  let 次 = 0; const 编过 = [];
  const 改稿 = 源码.replace("你好", "世界");
  const 结果 = await 助写({ prompt: "打印世界", code: "" }, 知识, async () => {
    if (次++ === 0) return { allow: true };
    if (次 === 2) return 工具答("compile_yuyan", { code: 源码 }, "1");
    return { refuse: false, code: 改稿 };
  }, async 文 => { 编过.push(文); return { ok: true }; });
  assert.deepEqual(编过, [源码, 改稿]); assert.equal(结果.code, 改稿);
});
test("连续请求运行工具仍不能超过六次执行预算", async () => {
  let 次 = 0, 执行 = 0;
  const 结果 = await 助写({ prompt: "打印你好", code: "" }, 知识, async () => {
    if (次++ === 0) return { allow: true };
    const 答 = 工具答("run_yuyan", { code: 源码 }, `${次}甲`);
    答.message.tool_calls.push(...["乙", "丙"].map(字 => 工具答("run_yuyan", { code: 源码 }, `${次}${字}`).message.tool_calls[0]));
    return 答;
  }, async () => { 执行++; return { ok: false, stderr: "error: 失败" }; });
  assert.equal(执行, 6); assert.equal(结果.code, undefined); assert.match(结果.error, /预算/);
});
