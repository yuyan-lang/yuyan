# RSA固定算术核验结果

2026-10-08，根授予pit-mac唯一重验证窗口。源树以正式yybs提交5192cda88为基线，仅恢复RSA造钥、定宽RSA、两测试、双语公钥说明及静核记录七文件，形成干净提交477195cd7。恢复来源为22714b2ea及824999d65所在完整链，保留正式密码包和依赖。本次类型、Node及原生硬断言均通过；机器恒时审计仍待完成。

本机专属树 `/Users/zc/repos/yuyan-worktrees/基础库-密码公钥核验`，远端专属树 `/Users/zhiboc/repos/yuyan-worktrees/基础库-密码公钥核验`。验证后远端源码工作树干净，所有任务结束，重窗口已释放。本文及原始日志作为后续证据提交。

## 工具来源

完整ec25b243发布原包由字体代理只读提供，原路径 `/private/tmp/yy字体工具链后续.tar.gz`，SHA256 `efdd12a2cce92ec06634f1091a94754992063f0234c3a8a68afb3d9ffd3a1dd3`。本机及远端原包均核得同值，整包解于远端树内 `yyRSA验证链`。

| 实际工具 | SHA256 |
| --- | --- |
| yyRSA验证链/yy_bs_stable.wasm | e09cc2803ffc6f61d114f589b7138534e511bb1a448120d41366f563b1b3c3fb |
| yyRSA验证链/yy豫构_stable.wasm | aae2de4aaaca28015dffe6cbaff7d43714219041e30261a8614e22c05721b52f |
| yyRSA验证链/yy稳定节点宿主/宿主.cjs | 34127409d7f74c07105129cc2c096b7c77520abcd73ef593c90c05a9f244a5dd |

根另授权已验原生两器，均位于 `/Users/zhiboc/repos/yuyan-worktrees/基础库-原生两器`，两器彼此同源提交16cffa740。生成器SHA为 `c342466d18bc7ce4f6d8d6316f7d3fa4335b867313827903723e42c622fddc0b`，执行器SHA为 `89f3fdd1329c3bc53592eb7517a9f10ffb5c8f3425ac29e5b334c344a167a57a`，使用该树同配宿主；运行前逐字核对SHA一致。客体由ec25编译，两器来源分别记录。

## 实际顺序与结果

| 步骤 | 权威句柄或完成输出 | 结果及原日志 |
| --- | --- | --- |
| 首次类型调用 | chunk621166 | 退出1；编译前报告结森字符串编码不合规；yyRSA整体类型.log |
| 关闭构建日志后重试整体密码类型 | session96450，完成chunka1552e | 退出0；yyRSA整体类型重试.log |
| RSA造钥测试构建 | session48471，完成chunke78134 | 退出0，PID30113任务281最终链接0；yyRSA造钥构建.log |
| Node硬断言运行 | chunkab9f01 | 退出0；yyRSA造钥Node.log |
| 原生预编 | session69754，完成chunk572dea | 退出0；yyRSA原生生成.log |
| 原生硬断言运行 | chunk28d365 | 退出0；yyRSA造钥原生.log |

首次失败保留原日志。根确认构建日志读取中文分支的既有路径风险，使用既有环境变量 `YY_BUILD_LOG=0` 后同源同工具重试；未改包描述或算法。整体类型日志实际包含RSA造钥、定宽RSA、公钥与正式密码模块的类型完成记录。

Node与原生均输出：

```text
RSA素数密钥构造测试通过
RSA固定扫描密钥生成测试通过
```

测试的失败条件直接发生事故，进程失败；两种实际运行均为硬断言通过。覆盖8组65537拼肢独立商余、5组小域积独立参考、零/单位/二/负一模逆常量预期、已知p=61/q=53密钥、固定批次生成与模幂回验、合数/重复素数拒绝、失败归零及随机调用次数。Miller-Rabin判据官方出处与逐中间范围、公开迭代及下标证明见《RSA秘密除余静核记录.汉语.md》。拼肢参考采用测试内独立整数除余，模逆参考采用独立常量恒等式；这些边界是本库构造向量。

## 复现参数

以下为远端树根的串行命令参数；本次已执行，单缓存同刻仅一个构建。

```text
YY_BUILD_LOG=0 /opt/homebrew/bin/node yyRSA验证链/yy稳定节点宿主/宿主.cjs yyRSA验证链/yy豫构_stable.wasm 类型检查 密码 --编译器 ./yyRSA验证链/yy_bs_stable.wasm -j 24
YY_BUILD_LOG=0 /opt/homebrew/bin/node yyRSA验证链/yy稳定节点宿主/宿主.cjs yyRSA验证链/yy豫构_stable.wasm 构建 密码 测试 RSA造钥。测试 --编译器 ./yyRSA验证链/yy_bs_stable.wasm --输出 yyRSA造钥测试.wasm -j 24
/opt/homebrew/bin/node yyRSA验证链/yy稳定节点宿主/宿主.cjs yyRSA造钥测试.wasm
/opt/homebrew/bin/node /Users/zhiboc/repos/yuyan-worktrees/基础库-原生两器/yy稳定节点宿主/宿主.cjs /Users/zhiboc/repos/yuyan-worktrees/基础库-原生两器/yy原生生成.wasm 苹果 臂六十四 /Users/zhiboc/repos/yuyan-worktrees/基础库-原生两器/yy执行器.wasm yyRSA造钥测试.exe yyRSA造钥测试.wasm --预编二
chmod +x yyRSA造钥测试.exe
./yyRSA造钥测试.exe
```

## 产物与证据边界

Wasm客体SHA256 `0063b88a1d2f84677f56a72865163d6d67d4eec649b91b484ae43a96cb287aeb`；苹果臂六十四原生程序SHA256 `60eb72a81eef314df220ff2d5c521eb36723967ab6b1673e77cb9b9327f31ff0`。产物保存在远端专属树；六份原始日志归档于同目录《验证记录》内，保留首次失败及成功调用的完整控制台。

本次证明正式密码整体类型与RSA造钥测试在Node、原生运行通过。大位宽随机造钥、独立RSA2048互操作重放、全部定宽模幂向量重放、生成机器码和机器恒时测量列入后续事项。甲乙级仍按整库验收范围登记。
