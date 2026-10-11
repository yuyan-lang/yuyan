# ARM 硬件目标交接（2026-10-09 更新）

## 目标（用户 /goal 原话）

“内核任务、调度、内存管理改用底层豫言（内核态降级目标，AMD64 与 ARM64 共用），ARM64 支持多任务、多核，并在 HVF 下完成系统内造盘，造出的 ISO 与宿主逐字节相同。”用户随后说：“你先把那五步做完，我要看到完整的可自举的豫言操作系统。”步骤定义与逐日进度见同目录 `ARM硬件.md`。

## 现状

- 第 1–6 步完成：内核任务、调度、内存管理在豫言内核（`豫言操作系统/裸机/内核/`），两种架构共用；ARM64 有动态任务、多核与工具映像；ARM64 在 HVF 下的系统内造盘与宿主逐字节相同。
- x86 在 KVM 下的系统内造盘：修好 xHCI 存储等待预算与工具映像原生栈后，在 pit-mbp13 上造出的 ISO 也与宿主逐字节相同（见 `ARM硬件.md` 进度）。
- 最近一次复验在 yybs d2748d90c（提案 00005 让 AMD64 在系统里改用豫行工具之后）：ARM64 HVF 752 秒、x86 KVM 6062 秒，两处造出的 ISO 与三个豫行工具都与宿主逐字节相同。
- 第 7 步的 QEMU 部分完成：`启动盘 --架构 臂六十四` 造出 ARM64 UEFI 启动光盘 `yy豫言启动臂.iso`，在 QEMU 的 ARM64 UEFI 固件下以光盘与 U 盘两种接法进命令壳。剩实机（暂缓，需用户；苹果芯片的 Mac 不走 UEFI）。ARM64 启动盘还没有数据区（缺 ARM64 USB 存储驱动）。

## 测试方法（在 `~/repos/yuyan-worktrees/ARM硬件` 运行，`节点` 指 `node 豫言操作系统/宿主/节点/宿主.cjs`）

- 342 项裸机验证：`节点 yy豫构.wasm 文件 豫言操作系统/裸机/验证。豫 --输出 yy裸机验证.wasm -j 24`，再 `节点 yy裸机验证.wasm`（约 70 秒）。改裸机后端（降级器、手写内核）要先重建它；只改豫言内核不必（工具运行时现编内核）。
- x86 造盘：`YY_GC_INITIAL_HEAP_SIZE_MB=1024 node --max-old-space-size=24000 豫言操作系统/宿主/节点/宿主.cjs yy豫言系统.wasm 启动盘 --输出 目录 --预置 无 --工具链 --六十四位地址 --盘容量 2097152`（约 3–5 分钟，得 `yy豫言启动.iso` 即宿主参照与 `yy豫言启动盘.img`），之后 `git checkout -- 库/壳核心/拼音字库。豫`。数据盘 `/工具/` 只放 AMD64 豫行文件（豫构、编译器、豫言系统、源码格式化）与执行器的汇编树码 `yy执行器.树码`（2026-10-10 起）。数据盘是标准 ext4（2026-10-10 起），块数在造盘时按 `--盘容量` 定下，之后把盘文件扩大不会放大文件系统（挂载取超级块与设备容量中较小者）；要在系统里放下编译缓存，就在造盘时把 `--盘容量` 给到想要的大小。
- x86 系统内造盘：盘复制一份并稀疏扩到 8 GiB，`qemu-system-x86_64 -machine q35 -accel kvm -cpu host -m 24G -smp 4 -display none -monitor tcp:127.0.0.1:端口,server,nowait -serial pipe:前缀 -no-reboot -drive if=none,id=yy_boot,format=raw,file=盘 -device qemu-xhci,id=xhci -device usb-storage,bus=xhci.0,drive=yy_boot,bootindex=1 -boot order=c`（没有 KVM 而用纯模拟时把 `-accel kvm -cpu host` 换成 `-accel tcg`，默认的 qemu64 即可：编译器写缓存要取随机数，2026-10-11 起处理器没有 RDRAND 时退用任务号、时钟与调用计数混合的散列）；串口出现“命令壳已就绪”后一次写入下面的输入（2026-10-10 起壳里只有中文组字、没有中英文切换，直接送这段 UTF-8 文字即可；也可以送拼音 gjqdp、空格选首个候选「构建启动盘」、再回车）：
  - `「构建启动盘」`（2026-10-10 起一条命令：运行 `/工具/yy豫言系统 系统内造盘`，自己设 `YY_BUILD_LOG=0`、换到 `/源`，`--并行` 取处理器核数，即 `-smp` 的值；内存少时改输 `「运行」于「构建启动盘」于「--并行」于「1」`。它等同于旧的三行：设 `YY_BUILD_LOG` 为 0、切到 `/源`、运行 `/工具/yy豫言系统 启动盘 --输出 /输出 --预置 无 --工具链 --六十四位地址 --工具目录 /工具 --构 /工具/yy豫构 --编译器 /工具/yy4_bs --盘容量 2097152 --并行 核数 --只出光盘`；`/工具` 里的豫行文件直接放进新盘，不再预编）
  - `「文件」之「切换」于「/输出」`、`「文件」之「列出」`、`「退出」`
- ARM64：`yy豫言系统.wasm 构建 --架构 臂六十四 --输出 目录` 得内核镜像；`yy豫言系统.wasm 臂工具映像 --输出 目录 --六十四位地址` 得 `yy臂工具1..3.bin`（映像的客体是各工具从源码构建出的汇编树码 `目录/yy臂工具/名.树码`，2026-10-11 起取代工具 wasm，须与盘上 `/工具/` 里的同一批，否则认不出映像）；数据盘取 x86 合成镜像里光盘之后第一个 64 MiB 整数倍处起的数据区（光盘约 108 MB 时是 128 MiB：`dd bs=1m skip=128`，再 `truncate -s 8G`；数据区的扇区 2（字节 1024）起是 ext4 超级块，偏移 56 的魔数是 0xEF53）。ARM64 的输入同上，但三个工具写成 `/工具/yy豫言系统.wasm`、`/工具/yy豫构.wasm`、`/工具/yy4_bs.wasm`（ARM64 按客体的长度与样本匹配工具映像）。待办事项（2026-10-10；用户定 ARM 等提案 00007 收尾后再做）：2026-10-10 起数据盘 `/工具/` 不再放工具 wasm，只放 AMD64 豫行文件，ARM64 用新造的数据盘时认不出工具映像（执行器先查豫行文件，会把 AMD64 豫行文件交给服务 43），这条系统内造盘路径暂时走不通；恢复时要改成按豫行文件认映像，例如 `臂工具映像` 把头里的指纹取自对应的 AMD64 豫行文件（`--工具链` 造盘写出的 `输出/yy豫行工具/`），执行器先查映像再查豫行文件。复验旧路径只能用 2026-10-09 之前造的数据盘。QEMU：`qemu-system-aarch64 -machine virt,gic-version=3,virtualization=off,secure=off,highmem-ecam=off,iommu=smmuv3,default-bus-bypass-iommu=off -cpu host -accel hvf -m 24G -smp 4 -device loader,addr=0x40000000,data=<内存字节数>,data-len=8 -device loader,addr=0x40000008,data=<映像区字节数>,data-len=8`，每个映像 `-device loader,file=映像,addr=<地址>,force-raw=on`（按 2 MiB 对齐依次装在内存末尾），virtio 盘 `-device virtio-blk-pci,disable-legacy=on,addr=4,drive=yy_disk,iommu_platform=on,ats=off`，`-kernel 镜像`。
- ARM64 启动盘：`YY_GC_INITIAL_HEAP_SIZE_MB=1024 node --max-old-space-size=24000 豫言操作系统/宿主/节点/宿主.cjs yy豫言系统.wasm 启动盘 --架构 臂六十四 --输出 目录 --预置 无` 得 `yy豫言启动臂.iso`；QEMU：`qemu-system-aarch64 -machine virt,gic-version=3,virtualization=off,secure=off,highmem-ecam=off,iommu=smmuv3,default-bus-bypass-iommu=off -cpu host -accel hvf -m 2G -bios /opt/homebrew/share/qemu/edk2-aarch64-code.fd -display none -serial stdio -no-reboot`，光盘加 `-device virtio-scsi-pci -drive if=none,id=yy_cd,format=raw,media=cdrom,readonly=on,file=光盘 -device scsi-cd,drive=yy_cd`，U 盘加 `-device qemu-xhci -drive if=none,id=yy_usb,format=raw,file=光盘 -device usb-storage,drive=yy_usb`。固件是调试版，串口上先有一堆固件日志，引导程序印“Yuyan ARM64 UEFI boot”“Yuyan boot: starting kernel”后进内核。
- 从盘里取文件：数据盘是标准 ext4，把数据区取出（`dd`）后，可以用 e2fsprogs 的 `debugfs` 读，或让 Linux 挂载；宿主上也有最小读取器库 `持久盘读取。豫`（按路径列目录、读文件）。不必再扫描记录头。

## 坑

- 内核态底层豫言：无全局与内存；参数含闭包捕获不超过 4 个；「且」「或者」两边都求值；数字用阿拉伯数字或逐位中文。
- QEMU 监视器的文件名不能有汉字（行编辑会弄坏 UTF-8）；一次塞上万条监视器命令会把它堵住几十分钟。
- 在 ssh 远端用 `pgrep -f`、`pkill -f` 会匹配到自己所在 shell 的命令行，用 `pidof`。
- 8 GiB 稀疏盘用 `cp -c`（APFS 克隆）复制；普通 `cp` 会写满零，很慢。
- pit-mbp13 只有 8 GB 内存，24G 的客体跑起来会大量换页；KVM 下设备与宿主一卡顿，内核里按轮询次数计的超时就会提早到期。
- 动态任务陷阱时内核会打印“任务陷阱 槽 码 址 栈 回”，按址对照降级产物即可定位。
- UEFI 下的坑：edk2 不肯按地址整段要下跨内存表多项的区间；固件的栈与页表里不映射的页都可能落在内核目标区间里，所以引导程序读进缓冲、自备栈、关 MMU 后才拷。引导程序出异常时 edk2 只印异常地址，用 `-monitor tcp:…` 加 `info registers` 看 x5（ESR）、x6（FAR）可定位。
