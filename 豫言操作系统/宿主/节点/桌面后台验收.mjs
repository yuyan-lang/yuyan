// 文言：行实桌面之 Wasm，用真 SDL 隐窗；记录已交之素，以 SDL 事注入验预览与返回，不取前台，不谓实鼠之验。
// 汉语：运行实际桌面 Wasm，使用真实 SDL 隐藏窗口；记录已提交像素，以 SDL 事件注入验证预览与返回，不抢前台，不代替物理鼠标验收。
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import assert from 'node:assert/strict';
import {createRequire} from 'node:module';
import {创建能力, 带型实现, 能力清理, 运行节点应用} from './应用宿主.mjs';

const 原参 = process.argv.slice(2);
const [发行目录, 依赖目录, 资源目录, 图像路径] = 原参.slice(0, 4).map(项 => 路径.resolve(项));
const 验引号 = 原参[4] === '引号';
const 验拼音 = 原参[4] === '拼音';
const 验模式 = 原参[4] === '模式';
const 验文件命令 = 原参[4] === '文件命令';
const 验命令行 = 原参[4] === '命令行' || 验引号 || 验拼音 || 验模式 || 验文件命令;
assert.ok(发行目录 && 依赖目录 && 资源目录 && 图像路径, '须给发行、原生依赖、资源目录及图像输出路径');
const 启动文 = 文件系统.readFileSync(路径.join(发行目录, '启动.mjs'), 'utf8');
const 桥文 = 启动文.match(/const 内嵌 = \{值桥: \[([\d,\s]+)\]/u);
assert.ok(桥文, '发行启动文件缺少内嵌值桥');
const 桥模块 = new WebAssembly.Module(Uint8Array.from(桥文[1].split(',').map(数 => Number(数))));
const 程序模块 = new WebAssembly.Module(文件系统.readFileSync(路径.join(发行目录, '程序.wasm')));
const 能力 = 创建能力({授权: {目录: new Map([['桌面', {路径: 资源目录, 可写: false}]]), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: 路径.join(发行目录, '程序.wasm'), 显示面: new Map([['主窗口', {宽: 900, 高: 640}]]), 原生依赖目录: 依赖目录, 显示面后台: true, 输出: (号, 字节) => process[号 === 2 ? 'stderr' : 'stdout'].write(字节), 张量线程数: 1});
const 显示原 = 能力[带型实现].诺节宿主.诺节显示操作;
let 帧数 = 0;
let 读文件数 = 0;
let 退出按下 = false;
let 超时 = false;
let 首帧已得, 预览已得;
const 首帧 = new Promise(成 => { 首帧已得 = 成; });
const 预览帧 = new Promise(成 => { 预览已得 = 成; });
const 打开原 = 能力[带型实现].诺节宿主.诺节文件打开;
能力[带型实现].诺节宿主.诺节文件打开 = (...参) => { 读文件数 += 1; return 打开原(...参); };
const 文 = 字节 => new TextDecoder().decode(字节);
const 记录显示 = async (...参) => {
  const 果 = await 显示原(...参);
  if (文(参[0]) === '提交' && Number(果[0]) === 0) {
    帧数 += 1;
    const 宽 = Number(参[2]), 高 = Number(参[3]), 像素 = 参[4];
    const 彩 = Buffer.alloc(宽 * 高 * 3);
    for (let 序 = 0; 序 < 宽 * 高; 序 += 1) {
      彩[序 * 3] = 像素[序 * 4]; 彩[序 * 3 + 1] = 像素[序 * 4 + 1]; 彩[序 * 3 + 2] = 像素[序 * 4 + 2];
    }
    文件系统.writeFileSync(图像路径, Buffer.concat([Buffer.from(`P6\n${宽} ${高}\n255\n`), 彩]));
    if (帧数 === 1) 首帧已得();
    if (读文件数 > 0) 预览已得();
  }
  return 果;
};
记录显示.异步 = true;
能力[带型实现].诺节宿主.诺节显示操作 = 记录显示;
try {
  const 运行任务 = 运行节点应用({程序模块, 值桥模块: 桥模块, 能力});
  const 定时 = setTimeout(() => {
    超时 = true;
    const SDL = createRequire(路径.join(依赖目录, '豫言原生依赖.cjs'))('@kmamal/sdl');
    for (const 窗 of SDL.video.windows) 窗.destroy();
  }, 120000);
  await 首帧;
  const SDL = createRequire(路径.join(依赖目录, '豫言原生依赖.cjs'))('@kmamal/sdl');
  const 窗 = SDL.video.windows[0];
  assert.equal(窗.visible, false);
  const 点按钮 = (横, 纵) => {
    窗.emit('mouseMove', {x: 横, y: 纵});
    窗.emit('mouseButtonDown', {x: 横, y: 纵, button: 1});
    窗.emit('mouseButtonUp', {x: 横, y: 纵, button: 1});
  };
  点按钮(295, 250);
  await 预览帧;
  if (验命令行) {
    点按钮(240, 28);
    await new Promise(成 => setTimeout(成, 200));
    if (验模式) {
      点按钮(300, 388);
      await new Promise(成 => setTimeout(成, 200));
    }
    点按钮(300, 450);
    if (验引号 || 验拼音 || 验模式) {
      // 汉语：逐字符经过真实输入控件，验嵌套转换；整段粘贴有意保持原文。文言：逐字经实输入控件，以验嵌套之化；整段粘贴意存原文。
      for (const 字 of (验模式 ? '「回显」于「hx1{}」' : 验拼音 ? 'hx1ni1hao1' : '{回显}于{{甲}乙}')) {
        窗.emit('textInput', {text: 字});
        await new Promise(成 => setTimeout(成, 100));
      }
    } else {
      窗.emit('textInput', {text: 验文件命令 ? '「文件」之「读取」于「说明.txt」' : '「加」于「12」于「3」'});
    }
    窗.emit('keyDown', {key: 'return'});
    窗.emit('keyUp', {key: 'return'});
    await new Promise(成 => setTimeout(成, 200));
    窗.emit('keyDown', {key: 'up'});
    窗.emit('keyUp', {key: 'up'});
    窗.emit('keyDown', {key: 'return'});
    窗.emit('keyUp', {key: 'return'});
    await new Promise(成 => setTimeout(成, 200));
    窗.emit('mouseWheel', {x: 450, y: 300, dx: 0, dy: -8});
    await new Promise(成 => setTimeout(成, 200));
  }
  退出按下 = true;
  点按钮(440, 28);
  const 退出码 = await 运行任务;
  clearTimeout(定时);
  assert.equal(退出码, 0);
  assert.ok(帧数 > 0, '桌面未成功提交画面');
  assert.ok(读文件数 > 0, '文件按钮未调用实际文件服务');
  assert.ok(退出按下 && !超时, '返回按钮未使主循环结束');
  console.log('真实SDL后台绘制、文件按钮预览' + (验命令行 ? '、命令事件注入流程' : '') + '与返回按钮通过；帧数：' + 帧数);
  if (验命令行) console.log('命令执行结果须复核实际绘制记录，帧数本身不证明命令执行。');
} finally {
  await 能力[能力清理]?.();
}
