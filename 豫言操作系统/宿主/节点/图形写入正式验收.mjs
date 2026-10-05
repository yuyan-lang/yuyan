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
  const 只读 = process.argv[4] === '只读';
  const 图路 = 路径.resolve('yy图形只读建目录.ppm');
  try {
    const 果 = 启动(process.execPath, ['--import', import.meta.filename, 路径.join(发行, '启动.mjs'), '--授权控制台', '主控制台', 只读 ? '--授权只读目录' : '--授权目录', '桌面=' + 根, '--授权显示面', '主窗口=900x640', '--显示面后台', '--原生依赖目录', 依赖], {input: '「图形」\n「退出」\n', encoding: 'utf8', timeout: 120000, env: {...process.env, 豫言图形写入根: 根, 豫言图形写入依赖: 依赖, 豫言图形只读验收: 只读 ? '1' : '', 豫言图形只读像素: 图路}});
    assert.equal(果.status, 0, 果.stderr);
    assert.ok(果.stdout.includes('图形共享写入磁盘核对通过'), 果.stdout);
    if (只读) assert.ok(!文件系统.existsSync(路径.join(根, '图形目录')));
    else {
      assert.equal(文件系统.readFileSync(路径.join(根, '图形.txt'), 'utf8'), '短');
      assert.equal(文件系统.existsSync(路径.join(根, '图形目录', '内文.txt')), false);
      assert.equal(文件系统.readFileSync(路径.join(根, '图形目录', '迁移.txt'), 'utf8'), '目录内中文');
      assert.equal(文件系统.existsSync(路径.join(根, '图形目录', '副本.txt')), false);
    }
    console.log(只读 ? '正式GUI只读建目录拒绝与返回通过' : '正式GUI中文覆盖追加、目录创建导航、相对写入复制删除与返回通过');
  } finally { 文件系统.rmSync(根, {recursive: true, force: true}); }
} else {
  process.env.SDL_MAC_BACKGROUND_APP = '1';
  const SDL = createRequire(路径.join(process.env.豫言图形写入依赖, '豫言原生依赖.cjs'))('@kmamal/sdl');
  const 原建 = SDL.video.createWindow.bind(SDL.video);
  let 已验 = false, 窗口;
  SDL.video.createWindow = (...参) => {
    const 窗 = 原建(...参); 窗口 = 窗;
    const 点 = (x, y) => { 窗.emit('mouseMove', {x, y}); 窗.emit('mouseButtonDown', {x, y, button: 1}); 窗.emit('mouseButtonUp', {x, y, button: 1}); };
    let 输入纵 = 450;
    const 命令 = 文 => { 点(300, 输入纵); 窗.emit('textInput', {text: 文}); 窗.emit('keyDown', {key: 'return'}); 窗.emit('keyUp', {key: 'return'}); };
    const 原画 = 窗.render.bind(窗); let 已始 = false, 最近像素 = null, 最近画 = null;
    窗.render = (...画参) => {
      const 果 = 原画(...画参);
      最近画 = {宽:画参[0], 高:画参[1], 素:Buffer.from(画参[4])};
      if (process.env.豫言图形只读验收 === '1') {
        const [宽, 高, 步长, 格式, 像素] = 画参;
        assert.equal(格式, 'rgba32');
        const 彩 = Buffer.alloc(宽 * 高 * 3);
        for (let 序 = 0; 序 < 宽 * 高; 序++) { 彩[序 * 3] = 像素[序 * 4]; 彩[序 * 3 + 1] = 像素[序 * 4 + 1]; 彩[序 * 3 + 2] = 像素[序 * 4 + 2]; }
        最近像素 = Buffer.concat([Buffer.from(`P6\n${宽} ${高}\n255\n`), 彩]);
      }
      if (!已始) {
        已始 = true;
        setTimeout(async () => {
          const 等 = () => new Promise(成 => setTimeout(成, 200));
          点(240, 28); await 等();
          if (process.env.豫言图形只读验收 === '1') {
            命令('「文件」之「建目录」于「图形目录」'); await 等();
            assert.ok(!文件系统.existsSync(路径.join(process.env.豫言图形写入根, '图形目录')));
            assert.ok(最近像素); 文件系统.writeFileSync(process.env.豫言图形只读像素, 最近像素);
            已验 = true; 点(440, 28); return;
          }
          // 汉语：只改变真实SDL窗口尺寸，不注入任何输入，尺寸事件须独自触发新帧。文言：惟易实SDL窗尺，毋注输入，尺变之事须独发新帧。
          窗.setSize(1000,650); await 等(); await 等();
          assert.equal(最近画.宽,窗.pixelWidth); assert.equal(最近画.高,窗.pixelHeight);
          assert.equal(窗.width,1000); assert.equal(窗.height,650);
          // 汉语：先留命令草稿，再真实缩放；绘制范围扩展且缩放后仍可执行原草稿。文言：先存命令稿而实伸缩；绘域广而伸缩后犹可行旧稿。
          点(300, 输入纵); 窗.emit('textInput', {text:'「文件」之「写入」于「图形.txt」于「中文甲」'}); await 等();
          const 点色 = () => {
            const 横 = Math.floor(740 * 最近画.宽 / 窗.width), 纵 = Math.floor(300 * 最近画.高 / 窗.height);
            return [...最近画.素.subarray((纵 * 最近画.宽 + 横) * 4, (纵 * 最近画.宽 + 横) * 4 + 3)];
          };
          assert.notDeepEqual(点色(), [255,255,255]);
          窗.emit('mouseButtonDown', {x:646,y:463,button:1});
          窗.emit('mouseMove', {x:726,y:503});
          窗.emit('mouseButtonUp', {x:726,y:503,button:1}); await 等();
          assert.deepEqual(点色(), [255,255,255]);
          输入纵 = 490; 点(300, 输入纵); 窗.emit('keyDown', {key:'return'}); 窗.emit('keyUp', {key:'return'}); await 等();
          const 文路 = 路径.join(process.env.豫言图形写入根, '图形.txt');
          assert.equal(文件系统.readFileSync(文路, 'utf8'), '中文甲');
          命令('「文件」之「追加」于「图形.txt」于「乙」'); await 等();
          assert.equal(文件系统.readFileSync(文路, 'utf8'), '中文甲乙');
          命令('「文件」之「写入」于「图形.txt」于「短」'); await 等();
          assert.equal(文件系统.readFileSync(文路, 'utf8'), '短');
          命令('「文件」之「建目录」于「图形目录」'); await 等();
          assert.ok(文件系统.statSync(路径.join(process.env.豫言图形写入根, '图形目录')).isDirectory());
          命令('「文件」之「切换」于「图形目录」'); await 等();
          命令('「文件」之「写入」于「内文.txt」于「目录内中文」'); await 等();
          assert.equal(文件系统.readFileSync(路径.join(process.env.豫言图形写入根, '图形目录', '内文.txt'), 'utf8'), '目录内中文');
          命令('「文件」之「复制」于「内文.txt」于「副本.txt」'); await 等();
          assert.equal(文件系统.readFileSync(路径.join(process.env.豫言图形写入根, '图形目录', '副本.txt'), 'utf8'), '目录内中文');
          命令('「文件」之「删除」于「副本.txt」'); await 等();
          assert.equal(文件系统.existsSync(路径.join(process.env.豫言图形写入根, '图形目录', '副本.txt')), false);
          命令('「文件」之「建目录」于「空目录」'); await 等();
          命令('「文件」之「删除」于「空目录」'); await 等();
          assert.equal(文件系统.existsSync(路径.join(process.env.豫言图形写入根, '图形目录', '空目录')), false);
          命令('「文件」之「移动」于「内文.txt」于「迁移.txt」'); await 等();
          assert.equal(文件系统.existsSync(路径.join(process.env.豫言图形写入根, '图形目录', '内文.txt')), false);
          assert.equal(文件系统.readFileSync(路径.join(process.env.豫言图形写入根, '图形目录', '迁移.txt'), 'utf8'), '目录内中文');
          已验 = true; 点(440, 28);
        }, 0);
      }
      return 果;
    };
    return 窗;
  };
  process.on('exit', 码 => { if (码 === 0) { assert.ok(已验 && 窗口?.destroyed); console.log('图形共享写入磁盘核对通过'); } });
}
