// 文言：客编器之事惟传予豫言，桥不裁界面。汉语：临时宿主桥只转发编译器进度、结果和快捷键；页面状态由豫言决定。
import {浏览器编译,停止编译} from './客户端.mjs';
const 发=(名称,值)=>document.dispatchEvent(new CustomEvent(名称,{detail:JSON.stringify(值)}));
window.豫言试写可用=typeof WebAssembly==='object'&&typeof WebAssembly.promising==='function'&&typeof Worker==='function'&&typeof DecompressionStream==='function';
window.豫言试写启动=code=>{
 if(!window.豫言试写可用){发('豫言试写完成',{ok:false,error:'请使用支持 WasmGC 和 JSPI 的新版浏览器'});return;}
 浏览器编译(code,false,事件=>发('豫言试写进度',事件)).then(结果=>发('豫言试写完成',结果),错误=>发('豫言试写完成',{ok:false,error:错误.message}));
};
window.豫言试写停止=()=>停止编译();
document.getElementById('源码').addEventListener('keydown',事件=>{
 if(事件.key==='Tab'&&!事件.metaKey&&!事件.ctrlKey){事件.preventDefault();发('豫言试写快捷键',{name:'tab'});}
 else if((事件.metaKey||事件.ctrlKey)&&事件.key==='Enter'){事件.preventDefault();发('豫言试写快捷键',{name:'run'});}
});
