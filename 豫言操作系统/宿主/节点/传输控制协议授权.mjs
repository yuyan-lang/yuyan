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
    开门: (...参) => 开句柄('开门', 参, true),
    叩门: (...参) => 开句柄('叩门', 参, false),
    迎: 柄 => {
      if (!句柄们.has(Number(柄))) return [-8, -1];
      const 果 = 原语.迎(柄);
      if (果[0] === 0 && Number(果[1]) >= 0) 句柄们.add(Number(果[1]));
      return 果;
    },
    误由: 原语.误由,
  };
  // 文言：余术须为所录之柄；柄无效则依术还其缺省之果。汉语：其余函数要求句柄已登记；句柄无效时按函数返回缺省结果（听、听帛为空字节，勿滞、缄口、别为 null，其余为 0）。
  for (const [名, 缺省] of [['入门', 0], ['听', 空字节], ['听帛', 空字节], ['续告', 0], ['续告帛', 0], ['守候', 0], ['门号', 0], ['勿滞', null], ['缄口', null], ['别', null]]) {
    能力[名] = (柄, ...参) => {
      if (!句柄们.has(Number(柄))) return [-8, typeof 缺省 === 'function' ? 缺省() : 缺省];
      const 果 = 原语[名](柄, ...参);
      if (名 === '别' && 果[0] === 0) 句柄们.delete(Number(柄));
      return 果;
    };
  }
  能力.投书 = (服务器, 端口, 超时, 查询) => {
    if (!端点已授(服务器, 端口)) return [-1, 空字节()];
    return 原语.投书(服务器, 端口, 超时, 查询);
  };
  return {能力, 关闭() {
    for (const 柄 of 句柄们) 原语.别(柄);
    句柄们.clear();
  }};
}
