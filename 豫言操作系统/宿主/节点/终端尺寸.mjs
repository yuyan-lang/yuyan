// 汉语：节点仅转发豫言原生服务的查询结果，服务继承真实标准输出终端。文言：节点惟递豫言原生服务之果，服务承真实常出终端。
import {spawnSync as 同步启动} from 'node:child_process';
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import {fileURLToPath} from 'node:url';

export function 读取标准终端尺寸() {
  const 候选 = [process.env.YY_TERMINAL_SIZE_SERVICE, 路径.join(路径.dirname(fileURLToPath(import.meta.url)), 'yy终端尺寸查询.exe'), 路径.resolve('yy终端尺寸查询.exe')];
  const 服务 = 候选.find(径 => 径 && 文件系统.existsSync(径));
  if (!服务) return [false, 0n, 0n];
  const 果 = 同步启动(服务, [], {stdio: ['ignore', 'inherit', 'pipe'], timeout: 10000, maxBuffer: 1048576});
  if (果.error || 果.status !== 0) return [false, 0n, 0n];
  const 数 = /^([0-9]+) ([0-9]+)\s*$/u.exec(果.stderr.toString('utf8'));
  if (!数 || BigInt(数[1]) <= 0n || BigInt(数[2]) <= 0n) return [false, 0n, 0n];
  return [true, BigInt(数[1]), BigInt(数[2])];
}
