// 古曰：名分两辞，约定入口独存。今释：全仓库文档遵循语言后缀，AGENTS.md 保留工具发现用途，VS Code 插件与历史目录按仓库规则排除。
import {test} from 'node:test';import assert from 'node:assert/strict';import {execFileSync} from 'node:child_process';import {existsSync} from 'node:fs';import {fileURLToPath} from 'node:url';import path from 'node:path';
const 根=fileURLToPath(new URL('../../',import.meta.url));
test('文档文件名使用汉语或文言后缀',()=>{const files=execFileSync('git',['ls-files','-z','--cached','--others','--exclude-standard'],{cwd:根,encoding:'utf8'}).split('\0').filter(Boolean);const bad=files.filter(p=>existsSync(path.join(根,p))&&/\.md$/i.test(p)&&p!=='AGENTS.md'&&!p.startsWith('yuyan-vscode/')&&!p.split('/').some(x=>x.endsWith('_v0'))&&!/\.(汉语|文言)\.md$/.test(p));assert.deepEqual(bad,[]);});
