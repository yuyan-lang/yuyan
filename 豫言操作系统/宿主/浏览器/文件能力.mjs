// 汉语：公共文件接口的浏览器宿主原语，复用句柄表与原生目录、Blob能力；宿主显式授目录。文言：公文件接口之浏览器原语，复用柄表与原生目录、物字之能；宿主明授目录。
import {创建句柄表} from './句柄.mjs';

const 错码 = 错 => 错?.码 ?? ({NotAllowedError: 1, NotFoundError: 4, TypeMismatchError: 7, TypeError: 7}[错?.name] ?? 8);
const 失败 = (码, 消息) => { throw Object.assign(Error(消息), {码}); };
export function 创建文件能力({目录 = new Map()} = {}) {
  const 柄表 = 创建句柄表();
  const 目录号们 = new Map();
  const 取柄 = (号, 种) => {
    let 柄;
    try { 柄 = 柄表.取得(号); } catch { 失败(3, '文件资源已失效'); }
    if (柄.种 !== 种) 失败(3, '文件资源种类不符');
    return 柄;
  };
  const 路径段 = 路 => {
    if (typeof 路 !== 'string' || 路.includes('\0') || 路.includes('\\') || 路.startsWith('/')) 失败(7, '相对路径无效');
    if (!路) return [];
    const 段们 = 路.split('/');
    if (段们.some(段 => !段 || 段 === '.' || 段 === '..')) 失败(7, '相对路径含无效段');
    return 段们;
  };
  const 取目录 = async (号, 段们) => {
    let 柄 = 取柄(号, '目录').值;
    for (const 段 of 段们) 柄 = await 柄.getDirectoryHandle(段, {create: false});
    return 柄;
  };
  const 执行 = async (术, 误果) => {
    try { return await 术(); } catch (错) { return 误果(错码(错), String(错?.message ?? 错)); }
  };
  return {
    取得目录: 名 => 执行(async () => {
      if (!目录.has(名)) 失败(1, '目录未获授权');
      const 值 = await 目录.get(名);
      if (值?.kind !== 'directory') 失败(2, '宿主未提供目录能力');
      if (!目录号们.has(名)) 目录号们.set(名, 柄表.登记({种: '目录', 值}));
      return [0, 目录号们.get(名)];
    }, (码, 文) => [码, 文]),
    打开: (号, 路, 可写) => 执行(async () => {
      const 段们 = 路径段(路);
      if (!段们.length) 失败(7, '文件路径为空');
      const 父 = await 取目录(号, 段们.slice(0, -1));
      if (可写) 失败(1, '当前浏览器目录仅授只读');
      const 文件 = await (await 父.getFileHandle(段们.at(-1), {create: false})).getFile();
      return [0, 柄表.登记({种: '文件', 值: 文件, 偏移: 0})];
    }, (码, 文) => [码, 文]),
    读取: (号, 上限) => 执行(async () => {
      const 柄 = 取柄(号, '文件');
      if (!Number.isSafeInteger(上限) || 上限 < 1) 失败(7, '读取上限无效');
      if (柄.偏移 >= 柄.值.size) return [9, new Uint8Array(), ''];
      const 字节 = new Uint8Array(await 柄.值.slice(柄.偏移, 柄.偏移 + 上限).arrayBuffer());
      柄.偏移 += 字节.length;
      return [0, 字节, ''];
    }, (码, 文) => [码, new Uint8Array(), 文]),
    关闭: 号 => 执行(async () => {
      取柄(号, '文件'); 柄表.释放(号); return [0, ''];
    }, (码, 文) => [码, 文]),
    列目录: (号, 路) => 执行(async () => {
      const 值 = await 取目录(号, 路径段(路));
      const 项们 = [];
      for await (const [名, 柄] of 值.entries()) 项们.push([名, 柄.kind === 'directory' ? 1 : 0]);
      项们.sort((甲, 乙) => 甲[0] < 乙[0] ? -1 : 甲[0] > 乙[0] ? 1 : 0);
      return [0, 项们, ''];
    }, (码, 文) => [码, [], 文]),
    查询信息: (号, 路) => 执行(async () => {
      const 段们 = 路径段(路);
      if (!段们.length) { 取柄(号, '目录'); return [0, 1, 0, '']; }
      const 父 = await 取目录(号, 段们.slice(0, -1));
      try {
        const 文件 = await (await 父.getFileHandle(段们.at(-1), {create: false})).getFile();
        return [0, 0, 文件.size, ''];
      } catch (错) {
        if (错?.name !== 'TypeMismatchError') throw 错;
        await 父.getDirectoryHandle(段们.at(-1), {create: false});
        return [0, 1, 0, ''];
      }
    }, (码, 文) => [码, 2, 0, 文]),
    // 汉语：待办事项：公共写入能力与可写授权另接；当前桌面只读。文言：待办事项：公写之能与可写之授后接；今桌面惟读。
    写入: () => [1, 0, '当前浏览器目录仅授只读'],
    开启写入: (号, 路) => 执行(async () => {
      取柄(号, '目录');
      if (!路径段(路).length) 失败(7, '文件路径为空');
      失败(1, '当前浏览器目录仅授只读');
    }, (码, 文) => [码, 文]),
    创建目录: (号, 路) => 执行(async () => {
      取柄(号, '目录');
      if (!路径段(路).length) 失败(7, '目录路径为空');
      失败(1, '当前浏览器目录仅授只读');
    }, (码, 文) => [码, 文]),
    清理: () => { for (const [号] of 柄表.条目()) 柄表.释放(号); 目录号们.clear(); },
  };
}
