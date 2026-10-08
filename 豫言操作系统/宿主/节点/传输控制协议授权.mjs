// 汉语：应用只使用已授地址端口和本能力登记的网络句柄；网络操作由共用豫言核心执行。文言：应用惟用所授地址端口及本能所录网柄；网之事由共用豫言之核行之。
export function 创建传输控制协议授权({授权, 原语}) {
  const 句柄们 = new Set();
  const 主机名 = 名 => String(名).toLowerCase().replace(/^\[|\]$/gu, '');
  function 端点已授(名, 端口, 监听 = false) {
    if (!Number.isInteger(Number(端口)) || Number(端口) < 0 || Number(端口) > 65535) return false;
    if (!监听 && 授权.全部来源) return true;
    for (const 来源 of 授权.源 ?? []) {
      const 网址 = new URL(来源);
      const 源端口 = Number(网址.port || (网址.protocol === 'https:' ? 443 : 80));
      if (主机名(网址.hostname) === 主机名(名) && 源端口 === Number(端口)) return true;
    }
    return false;
  }
  function 开句柄(名, 参数, 监听) {
    if (!端点已授(参数[0], 参数[1], 监听)) return [-1, -1];
    const 果 = 原语[名](...参数);
    if ((果[0] === 0 || 果[0] === 1) && Number(果[1]) >= 0) 句柄们.add(Number(果[1]));
    return 果;
  }
  const 空字节 = () => new Uint8Array();
  const 能力 = {
    传输控制协议_监听: (...参) => 开句柄('传输控制协议_监听', 参, true),
    传输控制协议_开始连接: (...参) => 开句柄('传输控制协议_开始连接', 参, false),
    传输控制协议_接受: 柄 => {
      if (!句柄们.has(Number(柄))) return [-8, -1];
      const 果 = 原语.传输控制协议_接受(柄);
      if (果[0] === 0 && Number(果[1]) >= 0) 句柄们.add(Number(果[1]));
      return 果;
    },
    传输控制协议_错误消息: 原语.传输控制协议_错误消息,
  };
  for (const 名 of ['完成连接', '读取', '读取字节串', '从字节序数写入', '从字节序数写入字节串', '等待', '获取本地端口', '设置无延迟', '关闭写入', '关闭']) {
    const 键 = '传输控制协议_' + 名;
    能力[键] = (柄, ...参) => {
      if (!句柄们.has(Number(柄))) return [-8, 名.startsWith('读取') ? 空字节() : 名 === '关闭' || 名 === '关闭写入' || 名 === '设置无延迟' ? null : 0];
      const 果 = 原语[键](柄, ...参);
      if (名 === '关闭' && 果[0] === 0) 句柄们.delete(Number(柄));
      return 果;
    };
  }
  能力.数据报_交换 = (服务器, 端口, 超时, 查询) => {
    if (!端点已授(服务器, 端口)) return [-1, 空字节()];
    return 原语.数据报_交换(服务器, 端口, 超时, 查询);
  };
  return {能力, 关闭() {
    for (const 柄 of 句柄们) 原语.传输控制协议_关闭(柄);
    句柄们.clear();
  }};
}
