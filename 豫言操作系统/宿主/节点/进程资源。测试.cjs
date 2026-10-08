// 汉语：在真实苹果进程上核验共享核心导出与系统采样，须以实验外部函数接口旗运行。文言：于实苹果进程核共核之出与系统之采，须以实验外部函之旗行之。
'use strict';
const 断言 = require('node:assert/strict');
const 文件 = require('node:fs');
const 路径 = require('node:path');
const 宿主目录 = process.argv[2] ?? __dirname;
const 模块 = new WebAssembly.Module(文件.readFileSync(路径.join(宿主目录, 'yy进程资源共享.wasm')));
console.log('共享导出', WebAssembly.Module.exports(模块));
console.log('共享导入', WebAssembly.Module.imports(模块));
断言.ok(WebAssembly.Module.exports(模块).some(项 => 项.name === '内存' && 项.kind === 'memory'));
process.env.YY进程资源共享 = 路径.join(宿主目录, 'yy进程资源共享.wasm');
const {查询进程资源} = require(路径.join(宿主目录, '进程资源.cjs'));
const 果 = 查询进程资源(process.pid);
console.log('真实进程资源', 果);
断言.equal(果[0], 0);
断言.equal(果[1], true);
断言.ok(果[2] > 0n);
断言.equal(果[3], false);
console.log('真实进程资源测试通过');
