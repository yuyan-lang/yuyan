// 汉语：网络状态、句柄及地址解析由共享豫言核心完成；此处只接系统库函数与线性内存地址。文言：网络之态、柄与地址之解，皆成于共用豫言之核；此惟接系统库函与线性存之址。
const 签名们 = {
  socket: {arguments: ['int32', 'int32', 'int32'], return: 'int32'},
  bind: {arguments: ['int32', 'pointer', 'uint32'], return: 'int32'},
  listen: {arguments: ['int32', 'int32'], return: 'int32'},
  accept: {arguments: ['int32', 'pointer', 'pointer'], return: 'int32'},
  connect: {arguments: ['int32', 'pointer', 'uint32'], return: 'int32'},
  shutdown: {arguments: ['int32', 'int32'], return: 'int32'},
  getsockname: {arguments: ['int32', 'pointer', 'pointer'], return: 'int32'},
  setsockopt: {arguments: ['int32', 'int32', 'int32', 'pointer', 'uint32'], return: 'int32'},
  getsockopt: {arguments: ['int32', 'int32', 'int32', 'pointer', 'pointer'], return: 'int32'},
  poll: {arguments: ['pointer', 'uint32', 'int32'], return: 'int32'},
  close: {arguments: ['int32'], return: 'int32'},
  read: {arguments: ['int32', 'pointer', 'uint64'], return: 'int64'},
  write: {arguments: ['int32', 'pointer', 'uint64'], return: 'int64'},
  clock_gettime: {arguments: ['int32', 'pointer'], return: 'int32'},
  getentropy: {arguments: ['pointer', 'uint64'], return: 'int32'},
  fcntl: {符号: '__fcntl', arguments: ['int32', 'int32', 'int64'], return: 'int32'},
  // 汉语：等待路径以 FIONREAD 取可读字节数；ioctl 是变参函数，苹果 ARM64 变参走栈，故同 fcntl 取定参桩 __ioctl。文言：候之径以 FIONREAD 取可读之数；ioctl 变参，苹果 ARM64 变参行于栈，故如 fcntl 取定参之桩 __ioctl。
  ioctl: {符号: '__ioctl', arguments: ['int32', 'uint64', 'pointer'], return: 'int32'},
  openat: {符号: '__openat', arguments: ['int32', 'pointer', 'int32', 'uint16'], return: 'int32'},
  signal: {arguments: ['int32', 'pointer'], return: 'pointer', 原址: [1]},
  wait4: {arguments: ['int32', 'pointer', 'int32', 'pointer'], return: 'int32'},
  waitpid: {arguments: ['int32', 'pointer', 'int32'], return: 'int32'},
  clock_getres: {arguments: ['int32', 'pointer'], return: 'int32'},
};

export function 创建传输控制协议系统调用({取内存} = {}) {
  if (process.platform !== 'darwin') return {可用: false, 状态: 58, 导入: {苹果: {}}, 关闭() {}};
  let 外部, 库, 正在符号 = '';
  const 原发 = process.emitWarning;
  try {
    process.emitWarning = function (警告, ...余) {
      if (/\bFFI\b/u.test(String(警告?.message ?? 警告))) return;
      return 原发.call(this, 警告, ...余);
    };
    外部 = process.getBuiltinModule?.('node:ffi');
  } catch { 外部 = null; }
  finally { process.emitWarning = 原发; }
  if (!外部) return {可用: false, 状态: 58, 导入: {苹果: {}}, 关闭() {}};
  try {
    库 = new 外部.DynamicLibrary('/usr/lib/libSystem.B.dylib');
    正在符号 = '__error';
    const 取误址 = 库.getFunction('__error', {arguments: [], return: 'pointer'});
    const 函们 = {};
    for (const [名, 签名] of Object.entries(签名们)) {
      正在符号 = 签名.符号 ?? 名;
      const 函 = 库.getFunction(正在符号, {arguments: 签名.arguments, return: 签名.return});
      函们[名] = (...参) => {
        const 参与 = 参.map((值, 位) => {
          const 型 = 签名.arguments[位];
          if (型 === 'pointer') {
            if (签名.原址?.includes(位)) return BigInt(值);
            if (值 === 0) return 0n;
            return 外部.getRawPointer(new Uint8Array(取内存().buffer, 值));
          }
          if (型 === 'uint64') return BigInt(值 >>> 0);
          return 型 === 'int64' ? BigInt(值) : Number(值);
        });
        // 汉语：失败后立即读取同线程 errno，与原生导入桩保持负误码约定。文言：败则即取同线 errno，与原生导入桩同返负误码。
        const 果 = 函(...参与);
        if (果 === -1 || (typeof 果 === 'bigint' && BigInt.asIntN(64, 果) === -1n)) return -外部.getInt32(取误址(), 0);
        return Number(果);
      };
    }
    return {可用: true, 状态: 0, 导入: {苹果: 函们}, 关闭() {库.close();}};
  } catch (错) {
    库?.close();
    if (正在符号) throw Error('共享传输控制协议系统符号载入失败：苹果.' + 正在符号, {cause: 错});
    return {可用: false, 状态: 58, 导入: {苹果: {}}, 关闭() {}};
  }
}

export function 创建传输控制协议能力({模块字节} = {}) {
  let 核 = null;
  const 系统 = 创建传输控制协议系统调用({取内存: () => 核.内存});
  if (系统.可用 && 模块字节) {
    try {
      const 模块 = new WebAssembly.Module(模块字节);
      const 导入 = {...系统.导入, 目标: {平台号: () => 3}};
      for (const 项 of WebAssembly.Module.imports(模块)) {
        if (typeof 导入[项.module]?.[项.name] === 'function') continue;
        if (项.module === '苹果' || 项.module === '目标') throw Error('共享传输控制协议缺少系统绑定：' + 项.module + '.' + 项.name);
        if (项.kind !== 'function') throw Error('共享传输控制协议未提供导入：' + 项.module + '.' + 项.name + '（' + 项.kind + '）');
        导入[项.module] ??= {};
        导入[项.module][项.name] = () => {throw Error('共享传输控制协议未提供平台入口：' + 项.module + '.' + 项.name);};
      }
      核 = new WebAssembly.Instance(模块, 导入).exports;
      if (!(核.内存 instanceof WebAssembly.Memory) || 核.初始化() !== 0) 核 = null;
    } catch (错) {系统.关闭(); throw 错;}
  }
  const 编码 = new TextEncoder(), 解码 = new TextDecoder();
  function 运行(号, 甲 = 0, 乙 = 0, 丙 = 0) {
    return 核 ? Number(核.执行(号, Number(甲), Number(乙), Number(丙))) : -58;
  }
  function 整果(号, 甲 = 0, 乙 = 0, 丙 = 0) {
    const 状态 = 运行(号, 甲, 乙, 丙);
    return [状态, 核 ? Number(核.结果值()) : 0];
  }
  function 主机果(号, 主机, 端口, 队长 = 0) {
    if (!核) return [-58, -1];
    const 字节 = 编码.encode(主机);
    if (字节.length > 65536) return [-7, -1];
    new Uint8Array(核.内存.buffer, 核.主机缓冲(), 字节.length).set(字节);
    return 整果(号, 字节.length, 端口, 队长);
  }
  function 读果(号, 柄, 最大) {
    if (!核) return [-58, new Uint8Array()];
    const 状态 = 运行(号, 柄, 最大);
    const 长 = Number(核.结果值());
    return [状态, 状态 === 0 && 长 > 0 ? new Uint8Array(核.内存.buffer, 核.字节缓冲(), 长).slice() : new Uint8Array()];
  }
  function 写果(号, 柄, 内容, 起) {
    if (!核) return [-58, 0];
    const 字节 = typeof 内容 === 'string' ? 编码.encode(内容) : 内容;
    if (字节.length > 核.字节上限()) return [-7, 0];
    new Uint8Array(核.内存.buffer, 核.字节缓冲(), 字节.length).set(字节);
    return 整果(号, 柄, 字节.length, 起);
  }
  const 能力 = {
    数据报_交换: (服务器, 端口, 超时, 查询) => {
      if (!核) return [-58, new Uint8Array()];
      const 字节 = typeof 服务器 === 'string' ? 编码.encode(服务器) : 服务器;
      if (字节.length > 65536 || 查询.length > 65535) return [-7, new Uint8Array()];
      new Uint8Array(核.内存.buffer, 核.主机缓冲(), 字节.length).set(字节);
      new Uint8Array(核.内存.buffer, 核.字节缓冲(), 查询.length).set(查询);
      const 状态 = Number(核.数据报交换(字节.length, Number(端口), Number(超时), 查询.length));
      return [状态, 状态 === 0 ? new Uint8Array(核.内存.buffer, 核.字节缓冲(), 核.结果值()).slice() : new Uint8Array()];
    },
    传输控制协议_监听: (主机, 端口, 队长) => 主机果(77, 主机, 端口, 队长),
    传输控制协议_开始连接: (主机, 端口) => 主机果(78, 主机, 端口),
    传输控制协议_完成连接: 柄 => 整果(79, 柄),
    传输控制协议_接受: 柄 => 整果(80, 柄),
    传输控制协议_读取: (柄, 最大) => 读果(81, 柄, 最大),
    传输控制协议_读取字节串: (柄, 最大) => 读果(82, 柄, 最大),
    传输控制协议_从字节序数写入: (柄, 内容, 起) => 写果(83, 柄, 内容, 起),
    传输控制协议_从字节序数写入字节串: (柄, 内容, 起) => 写果(84, 柄, 内容, 起),
    传输控制协议_等待: (柄, 关注, 超时) => 整果(85, 柄, 关注, 超时),
    传输控制协议_获取本地端口: 柄 => 整果(86, 柄),
    传输控制协议_设置无延迟: (柄, 开) => [运行(87, 柄, 开 ? 1 : 0), null],
    传输控制协议_关闭写入: 柄 => [运行(88, 柄), null],
    传输控制协议_关闭: 柄 => [运行(89, 柄), null],
    传输控制协议_错误消息: 状态 => {
      if (!核) return '传输控制协议资源暂不可用';
      运行(90, 状态);
      return 解码.decode(new Uint8Array(核.内存.buffer, 核.字节缓冲(), 核.结果值()));
    },
  };
  return {可用: 核 !== null, 能力, 读取当前Unix毫秒() {
    if (!核) throw Error('墙钟系统资源暂不可用');
    const 值 = 核.当前纪元毫秒();
    if (值 === -9223372036854775808n) throw Error('墙钟系统读取失败');
    return 值;
  }, 关闭: () => 系统.关闭()};
}
