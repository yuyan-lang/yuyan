// 文言：隔离客器唯载豫言程序与空许可。汉语：Dynamic Worker 子入口只加载豫言 Wasm，环境不授平台绑定。
import 程序模块 from './程序.wasm';
import 值桥模块 from './值桥.wasm';
import {创建云工宿主} from './宿主.mjs';

export default 创建云工宿主({程序模块, 值桥模块, 许可: {}});
