# 证书固定 DER 入口双端核验记录

2026-10-08 在可信 pit-mac 独立树 `/Users/zhiboc/repos/yuyan-worktrees/基础库-证书当前核验` 核验正式源 `465d3bde8`。包检查和完整证书类型检查退出零。既有 `证书 测试 编码。测试` 构建退出零，同一客体在 Node 与苹果臂六十四原生执行均退出零。两端均输出：

```text
证书编码与名称测试通过
```

客体 `yy编码证书测试.wasm` SHA256 为 `61455e785c0fa532dc2e497685a89159640bc447685083379abadf9bd2001359`。本次固定入口覆盖其既有 ASN.1/DER、对象标识、证书时间与名称断言。完整证书链、信任库策略、完整 RFC5280、新增 CRL/OCSP 语义与甲乙验收列为后续逐项核验。

完整配套源为 `/private/tmp/yy最新完整链25a843`，整目录复制为自身 `yy证书工具链` 并赋自身写权。原包 `20261008T050000Z-yuyan-wasm-25a84347aa5ad9118a52f110ea37df4634ef7436.tar.gz` SHA256 为 `14911d64ea510d2f5ca1e761b872ead89709832f3f7285f5cd8d958c2d63a23a`；编译器核心为 `e3c8798df3d56aefd3ebbb71a6528f95df1281846fae9dcb647f97eb8ec3c961`；豫构核心为 `35df553ef152ce388e6e4b7dc234950f89f4cbc6ee947c4a5d0d0ee57060560b`。

现成原生两器位于 `/Users/zhiboc/repos/yuyan-worktrees/基础库-原生浮点验证`，生成器 SHA256 为 `e44afc8a64b8211e4ee63102d339745275bec4dd51c8175fb35b04eef077faa3`，执行器为 `e801b3a9d46a7a1730665637bfee29473741ef01133a05f98329f299ff0f0df6`。复用该树完整宿主，原生生成退出零后直接运行自身程序。

所有命令在上述证书独立树执行，串行、独立缓存：

```text
/opt/homebrew/bin/node yy证书工具链/yy稳定节点宿主/宿主.cjs yy证书工具链/yy豫构_stable.wasm 检查 证书
YY_BUILD_LOG=0 /opt/homebrew/bin/node yy证书工具链/yy稳定节点宿主/宿主.cjs yy证书工具链/yy豫构_stable.wasm 类型检查 证书 --编译器 ./yy证书工具链/yy_bs_stable.wasm -j 24
YY_BUILD_LOG=0 /opt/homebrew/bin/node yy证书工具链/yy稳定节点宿主/宿主.cjs yy证书工具链/yy豫构_stable.wasm 构建 证书 测试 编码。测试 --输出 yy编码证书测试.wasm --编译器 ./yy证书工具链/yy_bs_stable.wasm -j 24
/opt/homebrew/bin/node yy证书工具链/yy稳定节点宿主/宿主.cjs yy编码证书测试.wasm
/opt/homebrew/bin/node /Users/zhiboc/repos/yuyan-worktrees/基础库-原生浮点验证/yy稳定节点宿主/宿主.cjs /Users/zhiboc/repos/yuyan-worktrees/基础库-原生浮点验证/yy原生生成.wasm 苹果 臂六十四 /Users/zhiboc/repos/yuyan-worktrees/基础库-原生浮点验证/yy执行器.wasm yy编码证书测试.exe yy编码证书测试.wasm --预编二
chmod u+x yy编码证书测试.exe
./yy编码证书测试.exe
```

六份真实日志：

- `/Users/zhiboc/repos/yuyan-worktrees/基础库-证书当前核验/yy证书包检查.log`
- `/Users/zhiboc/repos/yuyan-worktrees/基础库-证书当前核验/yy证书整体类型.log`
- `/Users/zhiboc/repos/yuyan-worktrees/基础库-证书当前核验/yy证书编码构建.log`
- `/Users/zhiboc/repos/yuyan-worktrees/基础库-证书当前核验/yy证书编码节点运行.log`
- `/Users/zhiboc/repos/yuyan-worktrees/基础库-证书当前核验/yy证书编码原生生成.log`
- `/Users/zhiboc/repos/yuyan-worktrees/基础库-证书当前核验/yy证书编码原生运行.log`

工具结果记录：包与类型句柄 78286 最终结果 `7f0aca` 退出零；构建句柄 55117 最终结果 `bac4b8` 退出零；Node `156acd` 退出零；原生生成句柄 56217 最终结果 `033a69` 退出零；直接原生运行 `f44e31` 退出零。此记录保存既有结果，编写时未重复运行客体。
