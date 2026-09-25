// 文言：节点先核客器与公约，后造旧值桥之客；汉语：Node/V8 在实例化前核对同一份浏览器成品。
import {readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {创建豫言实例} from './值桥.mjs';
import {核对首批接口装载} from './装载核对.mjs';

const 目录 = fileURLToPath(new URL('.', import.meta.url));
const 读清单 = 名 => JSON.parse(readFileSync(目录 + 名, 'utf8'));
const 程序模块 = new WebAssembly.Module(readFileSync(目录 + '程序.wasm'));
const 值桥模块 = new WebAssembly.Module(readFileSync(目录 + '值桥.wasm'));
核对首批接口装载({
  程序模块,
  应用要求: 读清单('应用要求.json'),
  宿主提供: 读清单('宿主提供.json'),
  应用入口: 读清单('启动接口.json'),
  投影: 读清单('投影.json')
});
const {运行} = 创建豫言实例(程序模块, 值桥模块, {}, {
  输出: 文字 => process.stdout.write(文字)
});
await 运行();
