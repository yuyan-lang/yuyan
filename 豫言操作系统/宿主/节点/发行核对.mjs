// 汉语：复用实际启动器的装载核对，仅读产物和支持清单，不执行客体；构建不能把编译成功当作可启动。文言：复用实启动器之装载核对，惟读产物与支持清单，不行客体；毋以编译之成谓可启动。
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import {核对节点装载, 读取宿主支持} from './应用宿主.mjs';
try {
  if (!process.argv[2]) throw Error('须给节点发行目录');
  const 目录 = 路径.resolve(process.argv[2]);
  const 程序字节 = 文件系统.readFileSync(路径.join(目录,'程序.wasm'));
  const 程序模块 = new WebAssembly.Module(程序字节);
  const 清单 = JSON.parse(文件系统.readFileSync(路径.join(目录,'清单.json'),'utf8'));
  核对节点装载({程序模块,程序字节,清单,宿主支持:读取宿主支持()});
  console.log('节点发行装载核对通过');
} catch (错) {
  console.error('节点发行装载核对失败：'+String(错?.message ?? 错));
  process.exitCode=3;
}
