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
const 验模式键 = 原参[4] === '模式键';
const 验模式 = 原参[4] === '模式' || 验模式键;
const 验文件命令 = 原参[4] === '文件命令';
const 验反复 = 原参[4] === '反复';
const 验重开 = 原参[4] === '重开';
const 验聚焦 = 原参[4] === '聚焦' || 验重开;
const 验命令行 = 原参[4] === '命令行' || 验引号 || 验拼音 || 验模式 || 验文件命令 || 验聚焦;
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
let 下一会帧 = null;
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
    if (下一会帧?.()) 下一会帧 = null;
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
  const 点按钮 = (横, 纵, 目标窗 = 窗) => {
    目标窗.emit('mouseMove', {x: 横, y: 纵});
    目标窗.emit('mouseButtonDown', {x: 横, y: 纵, button: 1});
    目标窗.emit('mouseButtonUp', {x: 横, y: 纵, button: 1});
  };
  点按钮(295, 250);
  await 预览帧;
  if (验命令行) {
    点按钮(240, 28);
    await new Promise(成 => setTimeout(成, 200));
    if (验模式) {
      if (验模式键) {
        // 汉语：以实际SDL按键映射切换，保留按钮模式供原路径回归。文言：以实SDL之键易式，存钮式以验旧路。
        窗.emit('keyDown', {key: 'f2'});
        窗.emit('keyUp', {key: 'f2'});
      } else 点按钮(300, 388);
      await new Promise(成 => setTimeout(成, 200));
    }
    if (!验聚焦) 点按钮(300, 450);
    if (验引号 || 验拼音 || 验模式) {
      // 汉语：逐字符经过真实输入控件，验嵌套转换；整段粘贴有意保持原文。文言：逐字经实输入控件，以验嵌套之化；整段粘贴意存原文。
      for (const 字 of (验模式 ? '「回显」于「hx1{}」' : 验拼音 ? 'hx1ni1hao1' : '{回显}于{{甲}乙}')) {
        窗.emit('textInput', {text: 字});
        await new Promise(成 => setTimeout(成, 100));
      }
    } else {
      窗.emit('textInput', {text: 验聚焦 ? '「回显」于「甲乙」' : 验文件命令 ? '「文件」之「读取」于「说明.txt」' : '「加」于「12」于「3」'});
    }
    if (验聚焦) {
      // 汉语：仅点击窗口切换按钮，不点击输入框；在文中留下光标，切回后插字验证焦点与位置。文言：惟点换窗之钮，不点输入框；留光标于文中，复窗插字以验焦与位。
      await new Promise(成 => setTimeout(成, 200));
      for (let 次 = 0; 次 < 2; 次 += 1) {
        窗.emit('keyDown', {key: 'left'});
        窗.emit('keyUp', {key: 'left'});
      }
      if (验重开) 点按钮(650, 196);
      else 点按钮(135, 28);
      await new Promise(成 => setTimeout(成, 200));
      if (验重开) 文件系统.copyFileSync(图像路径, 图像路径 + '.闭窗.ppm');
      点按钮(240, 28);
      await new Promise(成 => setTimeout(成, 200));
      窗.emit('textInput', {text: '丙'});
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
  if (验模式键) {
    // 汉语：再次按F2恢复中文，以逐字数字选词输出验模式往返。文言：复按F2归中文，逐字以数择词，验式之往返。
    窗.emit('keyDown', {key: 'f2'});
    窗.emit('keyUp', {key: 'f2'});
    await new Promise(成 => setTimeout(成, 200));
    for (const 字 of 'hx1ni1hao1') {
      窗.emit('textInput', {text: 字});
      await new Promise(成 => setTimeout(成, 100));
    }
    窗.emit('keyDown', {key: 'return'});
    窗.emit('keyUp', {key: 'return'});
    await new Promise(成 => setTimeout(成, 200));
  }
  const 第二会帧 = 验反复 ? new Promise(成 => {
    下一会帧 = () => { if (!窗.destroyed) return false; 成(); return true; };
  }) : null;
  点按钮(440, 28);
  if (验反复) {
    await 第二会帧;
    assert.ok(窗.destroyed, '第一轮返回未归还窗口');
    const 新窗 = SDL.video.windows.find(项 => !项.destroyed);
    assert.ok(新窗 && 新窗 !== 窗, '第二轮未取得新窗口');
    点按钮(295, 250, 新窗);
    await new Promise(成 => setTimeout(成, 200));
    点按钮(440, 28, 新窗);
    assert.equal(await 运行任务, 0);
    assert.ok(新窗.destroyed, '第二轮返回未归还窗口');
    console.log('同一Wasm程序两次桌面启停、归还窗口与返回调用方通过');
  }
  const 退出码 = await 运行任务;
  clearTimeout(定时);
  assert.equal(退出码, 0);
  assert.ok(窗.destroyed, '桌面返回后未归还本次SDL窗口');
  assert.ok(帧数 > 0, '桌面未成功提交画面');
  assert.ok(读文件数 > 0, '文件按钮未调用实际文件服务');
  assert.ok(退出按下 && !超时, '返回按钮未使主循环结束');
  console.log('真实SDL后台绘制、文件按钮预览' + (验命令行 ? '、命令事件注入流程' : '') + '与返回按钮通过；帧数：' + 帧数);
  if (验命令行) console.log('命令执行结果须复核实际绘制记录，帧数本身不证明命令执行。');
} finally {
  await 能力[能力清理]?.();
}
