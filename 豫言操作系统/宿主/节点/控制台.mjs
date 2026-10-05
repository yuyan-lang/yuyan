// 汉语：节点控制台逐字节读一行，复用公共授权等待管理，不预读下一行或子程序输入。文言：节点控制台逐字读一行，共公授权候之治，不先夺次行及子程序之入。
import {readSync, writeSync} from 'node:fs';
import {创建控制台能力} from '../浏览器/控制台.mjs';

export function 创建节点控制台({输入 = 0, 输出 = 1, 名称 = '主控制台'} = {}) {
  let 已尽 = false;
  const 字 = Buffer.alloc(1);
  const 读取行 = () => {
    if (已尽) return [false, ''];
    const 块们 = [];
    while (true) {
      const 数 = readSync(输入, 字, 0, 1, null);
      if (数 === 0) {已尽 = true; return 块们.length ? [true, Buffer.from(块们).toString('utf8')] : [false, ''];}
      if (字[0] === 10) return [true, Buffer.from(块们).toString('utf8').replace(/\r$/, '')];
      块们.push(字[0]);
    }
  };
  const 写文本 = 文 => {
    const 字节 = Buffer.from(文, 'utf8'); let 偏移 = 0;
    while (偏移 < 字节.length) {
      const 数 = writeSync(输出, 字节, 偏移, 字节.length - 偏移);
      if (数 === 0) throw Error('控制台写入没有进展');
      偏移 += 数;
    }
  };
  return 创建控制台能力(new Map([[名称, {读取行, 写文本}]]));
}
