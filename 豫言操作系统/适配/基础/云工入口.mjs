// 文言：客器与清单先校，原宿主后受事件；汉语：先核对接口和 Wasm，再导出既有 Worker 事件入口。
import 程序模块 from './程序.wasm';
import 应用要求 from './应用要求.json';
import 宿主提供 from './宿主提供.json';
import 应用入口 from './启动接口.json';
import 投影 from './投影.json';
import 原入口 from './原入口.mjs';
import {核对首批接口装载} from './装载核对.mjs';

核对首批接口装载({程序模块, 应用要求, 宿主提供, 应用入口, 投影});
export {豫言持久对象} from './原入口.mjs';
export default 原入口;
