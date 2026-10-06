// 汉语：正式启动器验收原壳公共命令与图形往返；预加载分支只观察真实SDL绘制并注入鼠标事件，不补接口或更改程序。文言：循正式启动器验原壳公命与图形往返；预载支惟观实SDL之绘而注鼠事，不补接口，不易程序。
import 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {createRequire, syncBuiltinESMExports} from 'node:module';
import {spawnSync as 同步启动} from 'node:child_process';
import assert from 'node:assert/strict';

if (路径.resolve(process.argv[1]) === import.meta.filename) {
  const [发行, 子发行, 依赖, 图像] = process.argv.slice(2).map(项 => 路径.resolve(项));
  assert.ok(发行 && 子发行 && 依赖 && 图像, '须给原壳发行、子发行、原生依赖及图像路径');
  const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy原壳正式-'));
  const 资源 = 路径.join(根, '公共命令'), 图资源 = 路径.join(根, '桌面');
  文件系统.mkdirSync(资源); 文件系统.mkdirSync(图资源);
  文件系统.writeFileSync(路径.join(图资源, '说明.txt'), '豫言文件预览验证');
  try {
    const 启动 = 路径.join(发行, '启动.mjs');
    const 命令们 = ['「回显」于「进入前」', '「文件」之「建目录」于「新目录」', '「设」于「豫言验收」于「环境甲」', '「运行」于「子验收」于「甲 乙」于「丙」', '中文输入', '「回显」于「返回后」', '「取消」于「豫言验收」', '「退出」'];
    const 果 = 同步启动(process.execPath, [启动, '--授权控制台', '主控制台', '--授权目录', '桌面=' + 资源, '--授权子程序', '子验收=' + 路径.join(子发行, '启动.mjs'), '--允许环境', '豫言验收'], {input: 命令们.join('\n') + '\n', encoding: 'utf8', timeout: 120000});
    assert.equal(果.status, 0, 果.stderr);
    assert.ok(果.stdout.includes('中文输入\n甲 乙，丙\n环境甲\n'), 果.stdout);
    assert.ok(果.stdout.includes('返回后') && 果.stdout.includes('壳已退出'), 果.stdout);
    assert.ok(文件系统.statSync(路径.join(资源, '新目录')).isDirectory());
    assert.equal(文件系统.readFileSync(路径.join(资源, '.环境'), 'utf8'), '');
    console.log('正式原壳目录创建、环境保存取消、子输入参数环境及继续会话通过');
    const 图果 = 同步启动(process.execPath, ['--import', import.meta.filename, 启动, '--授权控制台', '主控制台', '--授权只读目录', '桌面=' + 图资源, '--授权显示面', '主窗口=900x640', '--显示面后台', '--原生依赖目录', 依赖], {
      input: '「回显」于「进入前」\n「图形」\n「回显」于「返回后」\n「退出」\n', encoding: 'utf8', timeout: 120000,
      env: {...process.env, 豫言验收依赖: 依赖, 豫言验收资源: 图资源, 豫言验收图像: 图像},
    });
    assert.equal(图果.status, 0, 图果.stderr);
    assert.ok(图果.stdout.includes('进入前') && 图果.stdout.includes('返回后') && 图果.stdout.includes('壳已退出'), 图果.stdout);
    assert.ok(图果.stdout.includes('真实SDL预览与窗口归还通过'), 图果.stdout);
    assert.ok(文件系统.statSync(图像).size > 0);
    console.log('正式启动器默认命令行、图形指令、真实SDL预览及返回原会话通过');
  } finally { 文件系统.rmSync(根, {recursive: true, force: true}); }
} else {
  process.env.SDL_MAC_BACKGROUND_APP = '1';
  const SDL = createRequire(路径.join(process.env.豫言验收依赖, '豫言原生依赖.cjs'))('@kmamal/sdl');
  const 说明 = 文件系统.realpathSync(路径.join(process.env.豫言验收资源, '说明.txt'));
  let 已读说明 = false, 已预览 = false, 窗口 = null;
  const 原开 = 文件系统.openSync;
  文件系统.openSync = (...参) => { const 果 = 原开(...参); if (参[0] === 说明) 已读说明 = true; return 果; };
  syncBuiltinESMExports();
  const 原建 = SDL.video.createWindow.bind(SDL.video);
  SDL.video.createWindow = (...参) => {
    const 窗 = 原建(...参); 窗口 = 窗;
    assert.equal(窗.visible, false);
    const 点 = (横, 纵) => { 窗.emit('mouseMove', {x: 横, y: 纵}); 窗.emit('mouseButtonDown', {x: 横, y: 纵, button: 1}); 窗.emit('mouseButtonUp', {x: 横, y: 纵, button: 1}); };
    const 原画 = 窗.render.bind(窗); let 首帧 = true;
    窗.render = (...画参) => {
      const 果 = 原画(...画参);
      if (首帧) { 首帧 = false; setTimeout(() => 点(295, 250), 0); }
      if (已读说明 && !已预览) {
        已预览 = true;
        const [宽, 高, 步长, 格式, 像素] = 画参;
        assert.equal(格式, 'rgba32');
        const 彩 = Buffer.alloc(宽 * 高 * 3);
        for (let 序 = 0; 序 < 宽 * 高; 序++) { 彩[序 * 3] = 像素[序 * 4]; 彩[序 * 3 + 1] = 像素[序 * 4 + 1]; 彩[序 * 3 + 2] = 像素[序 * 4 + 2]; }
        文件系统.writeFileSync(process.env.豫言验收图像, Buffer.concat([Buffer.from(`P6\n${宽} ${高}\n255\n`), 彩]));
        setTimeout(() => 点(440, 28), 0);
      }
      return 果;
    };
    return 窗;
  };
  process.on('exit', 码 => { if (码 === 0) { assert.ok(已预览 && 窗口?.destroyed, '未实际预览或未归还SDL窗口'); console.log('真实SDL预览与窗口归还通过'); } });
}
