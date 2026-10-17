# 迅压 ARM 纯载荷固定断言验证

在受信任的 pit-mac，用集中同配系统及现成预置盘工具运行迅压原完整尾汇固定测试客体。客体保持全部固定断言，无参数，从私盘 `/程序/核验` 启动。

QEMU 真实句柄64861，PID23794，自然退出0。串口最终输出：

```text
迅压测试通过
yy基础库裸机子程序退出码：0
yy基础库裸机核验完成
设备退出清理完成
用户任务全部退出
```

固定测试的断验函数失败时读取空字节串位置0触发硬异常。此结果覆盖该完整固定客体在 ARM 纯载荷系统中的断言；正式交互桌面壳、各大客体与完整平台兼容验收继续推进。

## 冻结身份

远端自身工作树：`/Users/zhiboc/repos/yuyan-worktrees/基础库-迅压完善`。

|产物|SHA-256|
|---|---|
|yy迅压尾汇短测.wasm，206768字节|8a80dd777e49273862e3683121d388e340184646917e4f16b7dbf37cdfc127c6|
|yy迅压ARM私盘.img，64MiB|57b9ddce7a47eb3bafa22a72aa5e11f17e7d2d79addc75ab418992a96a58edd2|
|yy迅压ARM串口.log|dee9ddd8805c60e487b2ffe54d48f4e1be90523e3c8332b35d62cd00edc858b7|
|共享只读内核|bcbc1c639afbd34229127517ed3d29dac5788472a552297a66074862f44e0196|
|共享只读预置盘工具|1331cbacd427d7bbd094665d2a7cd9816f5c6408f7d41c679e86c63fe23e19b8|

## 实际复现命令

私盘生成真实退出0，使用预置盘工具树的完整稳定宿主；共享工具与内核全程只读。

```text
/opt/homebrew/bin/node /Users/zhiboc/repos/yuyan-worktrees/基础库-裸机预置工具/yy稳定节点宿主/宿主.cjs /Users/zhiboc/repos/yuyan-worktrees/基础库-裸机预置工具/yy基础库预置盘.wasm /Users/zhiboc/repos/yuyan-worktrees/基础库-迅压完善/yy迅压ARM私盘.img /程序/核验=/Users/zhiboc/repos/yuyan-worktrees/基础库-迅压完善/yy迅压尾汇短测.wasm
```

在自身工作树运行：

```text
/opt/homebrew/bin/qemu-system-aarch64 -accel tcg -m 512M -smp 1 -display none -monitor none -chardev stdio,id=yy_serial,mux=on,signal=off,logfile=yy迅压ARM串口.log,logappend=off -serial chardev:yy_serial -no-reboot -kernel /Users/zhiboc/repos/yuyan-worktrees/基础库-同配裸机验收/yy纯载荷ARM系统/豫言系统-臂六十四.镜像 -machine virt,gic-version=2,virtualization=off,secure=off,highmem-ecam=off,iommu=smmuv3,default-bus-bypass-iommu=off -cpu cortex-a53 -semihosting-config enable=on,target=native -drive file=/Users/zhiboc/repos/yuyan-worktrees/基础库-迅压完善/yy迅压ARM私盘.img,if=none,id=yy_disk,format=raw -device virtio-blk-pci,disable-legacy=on,addr=4,drive=yy_disk,iommu_platform=on,ats=off
```

集中内核的源链及装载壳范围见基础库同配裸机验收树《基础库载荷验证记录.汉语.md》。本次复用已验系统，未重新构造内核。
