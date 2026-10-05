// 汉语：正式启动器运行原壳与真实SDL桌面，注入既有控件事件并核对磁盘结果。文言：正式启动器行原壳与实SDL桌面，注既有控件之事而核磁盘之果。
import 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {createRequire} from 'node:module';
import {spawnSync as 启动} from 'node:child_process';
import assert from 'node:assert/strict';

if (路径.resolve(process.argv[1]) === import.meta.filename) {
  const [发行, 依赖] = process.argv.slice(2).map(项 => 路径.resolve(项));
  const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy图形写入-'));
  try {
    const 果 = 启动(process.execPath, ['--import', import.meta.filename, 路径.join(发行, '启动.mjs'), '--授权目录', '桌面=' + 根, '--授权显示面', '主窗口=900x640', '--显示面后台', '--原生依赖目录', 依赖], {input: '「图形」\n「退出」\n', encoding: 'utf8', timeout: 120000, env: {...process.env, 豫言图形写入根: 根, 豫言图形写入依赖: 依赖}});
    assert.equal(果.status, 0, 果.stderr);
    assert.ok(果.stdout.includes('图形共享写入磁盘核对通过'), 果.stdout);
    assert.equal(文件系统.readFileSync(路径.join(根, '图形.txt'), 'utf8'), '短');
    console.log('正式原壳进入GUI执行中文覆盖、追加与截断并返回通过');
  } finally { 文件系统.rmSync(根, {recursive: true, force: true}); }
} else {
  process.env.SDL_MAC_BACKGROUND_APP = '1';
  const SDL = createRequire(路径.join(process.env.豫言图形写入依赖, '豫言原生依赖.cjs'))('@kmamal/sdl');
  const 原建 = SDL.video.createWindow.bind(SDL.video);
  let 已验 = false, 窗口;
  SDL.video.createWindow = (...参) => {
    const 窗 = 原建(...参); 窗口 = 窗;
    const 点 = (x, y) => { 窗.emit('mouseMove', {x, y}); 窗.emit('mouseButtonDown', {x, y, button: 1}); 窗.emit('mouseButtonUp', {x, y, button: 1}); };
    const 命令 = 文 => { 点(300, 450); 窗.emit('textInput', {text: 文}); 窗.emit('keyDown', {key: 'return'}); 窗.emit('keyUp', {key: 'return'}); };
    const 原画 = 窗.render.bind(窗); let 已始 = false;
    窗.render = (...画参) => {
      const 果 = 原画(...画参);
      if (!已始) {
        已始 = true;
        setTimeout(async () => {
          const 等 = () => new Promise(成 => setTimeout(成, 200));
          点(240, 28); await 等();
          命令('「文件」之「写入」于「图形.txt」于「中文甲」'); await 等();
          const 文路 = 路径.join(process.env.豫言图形写入根, '图形.txt');
          assert.equal(文件系统.readFileSync(文路, 'utf8'), '中文甲');
          命令('「文件」之「追加」于「图形.txt」于「乙」'); await 等();
          assert.equal(文件系统.readFileSync(文路, 'utf8'), '中文甲乙');
          命令('「文件」之「写入」于「图形.txt」于「短」'); await 等();
          assert.equal(文件系统.readFileSync(文路, 'utf8'), '短');
          已验 = true; 点(440, 28);
        }, 0);
      }
      return 果;
    };
    return 窗;
  };
  process.on('exit', 码 => { if (码 === 0) { assert.ok(已验 && 窗口?.destroyed); console.log('图形共享写入磁盘核对通过'); } });
}
