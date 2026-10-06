// 「：汉语：对照夹具固定为裸机平台；未使用的原生系统导入若被调用即报错。文言：对照之具定为裸机，余系统之导入若被召则报错。：」
module.exports = function 裸机导入(代码, 平台) {
  const 模块 = 代码 instanceof WebAssembly.Module ? 代码 : new WebAssembly.Module(代码);
  const 导入 = {平台: {...平台, 墙钟秒: () => 0, 中断查询: () => 0, 提交帧: () => -1, 帧缓冲信息: () => 0}, 目标: {平台号: () => 1}};
  for (const 项 of WebAssembly.Module.imports(模块)) {
    导入[项.module] ??= {};
    导入[项.module][项.name] ??= () => {throw new Error(`裸机对照误调用：${项.module}.${项.name}`);};
  }
  return 导入;
};
