import {test} from 'node:test';
import assert from 'node:assert/strict';
import {创建页面控制, 读取同源资源JSON数组项} from './宿主.mjs';

class 元素 {
  constructor(本地名, 标识, 类型='') { this.localName=本地名; this.id=标识; this.type=类型; this.listeners=new Map(); this.attributes=new Map(); this.hidden=false; }
  addEventListener(名, 函) { this.listeners.set(名, 函); }
  removeEventListener(名, 函) { if (this.listeners.get(名)===函) this.listeners.delete(名); }
  removeAttribute(名) { this.attributes.delete(名); if (名==='href') this.href=''; if (名==='download') this.download=''; }
  click() { this.listeners.get('click')?.({type:'click'}); }
  hasAttribute(名) { return this.attributes.has(名); }
  setAttribute(名, 值) { this.attributes.set(名, 值); }
}

class 录像器 {
  static isTypeSupported(型) { return 型==='video/webm;codecs=vp9,opus'; }
  constructor(流, 选项) { this.stream=流; this.mimeType=选项.mimeType; this.videoBitsPerSecond=选项.videoBitsPerSecond; this.state='inactive'; this.listeners=new Map(); 录像器.最后=this; }
  addEventListener(名, 函) { this.listeners.set(名, 函); }
  start() { this.state='recording'; }
  stop() { this.state='inactive'; this.listeners.get('stop')?.(); }
  产块(块) { this.listeners.get('dataavailable')?.({data:{size:块.length, type:this.mimeType}}); }
  报错(错) { this.listeners.get('error')?.({error:错}); this.stop(); }
}

const 流 = ({有音=true}={}) => {
  const 轨们=[{kind:'video',getSettings:()=>({width:3840,height:2160,frameRate:30}),addEventListener(){},stop(){this.已停=true;}}];
  if (有音) 轨们.push({kind:'audio',stop(){this.已停=true;}});
  return {轨们, getTracks(){return 轨们},getAudioTracks(){return 轨们.filter(轨=>轨.kind==='audio')},getVideoTracks(){return 轨们.filter(轨=>轨.kind==='video')}};
};

const 建宿主 = 请求 => {
  const 音频=new 元素('audio','音频');
  Object.assign(音频,{currentTime:1.25,duration:63.84,paused:true,ended:false,muted:false,volume:.28,readyState:4,play(){this.paused=false;return Promise.resolve()},pause(){this.paused=true}});
  const 播放钮=new 元素('button','播放','button');
  const 开始钮=new 元素('button','开始','button');
  const 停止钮=new 元素('button','停止','button');
  const 下载=new 元素('a','下载');
  const 节点=new Map([音频,播放钮,开始钮,停止钮,下载].map(元=>[元.id,元]));
  const 根={getElementById(标识){return 节点.get(标识)??null}};
  let 序=0;
  const 全局={
    navigator:{mediaDevices:{getDisplayMedia(约束){请求.约束=约束;return Promise.resolve(请求.显示流??流())}}},
    MediaRecorder:录像器,
    URL:{createObjectURL(){return 'blob:录制/'+(++序)},revokeObjectURL(网址){请求.已撤网址=网址}},
    Blob:class {constructor(块,{type}){this.块=块;this.type=type}}
  };
  const 句柄={上限:20,登记(){throw Error('测试未使用句柄')},取得(){throw Error('测试未使用句柄')},条目(){return []}};
  const 控制=创建页面控制({根,全局,路径:'https://例子.test/',网络(){},句柄,释放句柄全部(){}});
  return {控制:控制.界面操作表,音频,播放钮,开始钮,停止钮,下载,请求};
};

test('按需读取同源脚本 JSON 数组项并缓存解析结果', async () => {
  const 源文 = 'const 仓库选段=[{"高亮":"甲 ] 乙 \\" 丙"},{"高亮":"丁"}];';
  let 请求数 = 0;
  const 正文缓存 = new Map(), 数组缓存 = new Map();
  const 网络 = async () => { 请求数++; return new Response(源文, {status: 200}); };
  const 全列文 = await 读取同源资源JSON数组项('/演示.html', '仓库选段', -1, 'https://例子.test/', 网络, 正文缓存, 数组缓存);
  const 项文 = await 读取同源资源JSON数组项('/演示.html', '仓库选段', 1, 'https://例子.test/', 网络, 正文缓存, 数组缓存);
  assert.deepEqual(JSON.parse(全列文), [{高亮:'甲 ] 乙 " 丙'}, {高亮:'丁'}]);
  assert.deepEqual(JSON.parse(项文), {高亮:'丁'});
  assert.equal(请求数, 1);
  await assert.rejects(() => 读取同源资源JSON数组项('/演示.html', '未知数组', 0, 'https://例子.test/', 网络, 正文缓存, 数组缓存), /没有命名数组/);
});

test('网页媒体可读取音频时钟并由用户按钮切换播放', async () => {
  const 宿主=建宿主({});
  const 状态=JSON.parse(宿主.控制.读取音频状态('音频'));
  assert.equal(状态.时刻,1250);
  assert.equal(状态.时长,63840);
  assert.equal(状态.音量,280);
  宿主.控制.绑定音频按钮('音频','播放');
  宿主.播放钮.click();
  await Promise.resolve();
  assert.equal(宿主.音频.paused,false);
  宿主.播放钮.click();
  assert.equal(宿主.音频.paused,true);
  宿主.控制.设置音频时刻('音频','4000');
  assert.equal(宿主.音频.currentTime,4);
  宿主.音频.paused=true;
  宿主.音频.play=()=>Promise.reject(new Error('播放策略阻止'));
  宿主.播放钮.click();
  await Promise.resolve();
  await Promise.resolve();
  assert.equal(JSON.parse(宿主.控制.读取音频状态('音频')).错误,'播放策略阻止');
  宿主.控制.暂停音频('音频');
  assert.equal(宿主.音频.paused,true);
  assert.equal(JSON.parse(宿主.控制.读取音频状态('音频')).错误,'');
});

test('网页媒体录制只在绑定按钮点击后捕获，并完成下载链接', async () => {
  const 请求={};
  const 宿主=建宿主(请求);
  宿主.控制.绑定屏幕录制('开始',JSON.stringify({停止:'停止',下载:'下载',配置:{宽:3840,高:2160,帧率:30,码率:20000000,文件名:'测试录制'}}));
  assert.equal(请求.约束,undefined);
  宿主.开始钮.click();
  await Promise.resolve();
  await Promise.resolve();
  let 状态=JSON.parse(宿主.控制.读取录制状态('下载'));
  assert.equal(状态.状态,'录制中');
  assert.equal(状态.宽,3840);
  assert.equal(状态.音轨数,1);
  assert.equal(请求.约束.audio,true);
  assert.equal(请求.约束.video.width.ideal,3840);
  录像器.最后.产块('画面与声音');
  宿主.停止钮.click();
  状态=JSON.parse(宿主.控制.读取录制状态('下载'));
  assert.equal(状态.状态,'可下载');
  assert.equal(宿主.下载.href,'blob:录制/1');
  assert.equal(宿主.下载.download,'测试录制.webm');
  assert.equal(宿主.下载.hidden,false);
  宿主.开始钮.click();
  assert.equal(宿主.下载.hidden,true);
  assert.equal(宿主.下载.href,'');
  assert.equal(宿主.下载.download,'');
  await Promise.resolve();
  await Promise.resolve();
});

test('标签页捕获不含音轨时停止流并报告失败', async () => {
  const 请求={显示流:流({有音:false})};
  const 宿主=建宿主(请求);
  宿主.控制.绑定屏幕录制('开始',JSON.stringify({停止:'停止',下载:'下载',配置:{}}));
  宿主.开始钮.click();
  await Promise.resolve();
  await Promise.resolve();
  const 状态=JSON.parse(宿主.控制.读取录制状态('下载'));
  assert.equal(状态.状态,'失败');
  assert.match(状态.错误,/没有标签页音轨/);
  assert.ok(请求.显示流.轨们.every(轨=>轨.已停));
});

test('录制错误后不生成伪下载文件', async () => {
  const 宿主=建宿主({});
  宿主.控制.绑定屏幕录制('开始',JSON.stringify({停止:'停止',下载:'下载',配置:{}}));
  宿主.开始钮.click();
  await Promise.resolve();
  await Promise.resolve();
  录像器.最后.产块('错误前的数据');
  录像器.最后.报错(new Error('编码器故障'));
  const 状态=JSON.parse(宿主.控制.读取录制状态('下载'));
  assert.equal(状态.状态,'失败');
  assert.equal(状态.错误,'编码器故障');
  assert.equal(宿主.下载.hidden,true);
  assert.equal(宿主.下载.href,'');
});
