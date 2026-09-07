// 古曰：取名须审，不以客辞作路径。今释：下载链接仅接受单个 zip 文件名，不发送上传凭据。
let 查询序=0;
document.getElementById('包查询').addEventListener('submit',async e=>{e.preventDefault();const seq=++查询序,名=document.getElementById('包名').value.trim(),提示=document.getElementById('包提示'),链接=document.getElementById('包下载'),发布者=document.getElementById('包发布者');链接.hidden=true;发布者.hidden=true;if(!/^[\p{L}\p{N}][\p{L}\p{N}._-]*\.zip$/u.test(名)||名.includes('..')){文案(提示,'请输入单个 .zip 包文件名，不能包含路径。','请书单一 .zip 包名，不可含路径。');return;}文案(提示,'正在查询…','正查询…');try{const r=await fetch('/api/packages/'+encodeURIComponent(名),{cache:'no-store'}),data=await r.json();if(seq!==查询序)return;if(!r.ok)throw Error(data.error||'查询失败');document.getElementById('发布者名称').textContent=data.publisher.name;标识(document.getElementById('包验证'),data.verification);发布者.hidden=false;链接.href=data.downloadUrl;链接.hidden=false;文案(提示,'已找到此包，可查看发布者状态并下载。','已得其包，可观发布者而取之。');}catch(e){if(seq===查询序)文案(提示,e.message,e.message);}});

// 古曰：私符不藏于浏览器久库。今释：登录使用 HttpOnly 会话，上传令牌仅暂存在当前页面，不写入 localStorage。
const 元素=id=>document.getElementById(id);let 注册模式=true,当前账户=null,验证凭据=null,重置凭据=null;
function 文案(el,han,wen){el.dataset.han=han;el.dataset.wen=wen;el.textContent=window.豫言界面?.语言==='wen'?wen:han;}
function 标识(el,value){el.textContent=value==='verified'?'verified':'unverified';el.dataset.verified=String(value==='verified');}
function 状态(han,wen=han){文案(元素('账户状态'),han,wen);}
async function 请求账户(action,data){const r=await fetch('/api/account/'+action,data===undefined?{cache:'no-store'}:{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(data)});const result=await r.json();if(!r.ok)throw new Error(result.error||'账户请求失败');return result;}
async function 刷账户(){const {user}=await 请求账户('session');当前账户=user;元素('账户表单').hidden=!!user;元素('账户面板').hidden=!user;if(user){标识(元素('账户验证'),user.verification);文案(元素('邮箱状态'),user.emailVerified?'邮箱已验证':'邮箱尚未验证，暂不能上传包',user.emailVerified?'邮箱已验':'邮箱未验，未可上传');元素('重发验证').hidden=!!user.emailVerified;元素('生成上传令牌').hidden=!user.emailVerified;}元素('账户身份').textContent=user?user.email+' · '+user.name:'';元素('上传令牌').value='';元素('令牌区域').hidden=true;更新验证提示();}
async function 执行账户(fn){const buttons=[...document.querySelectorAll('.账户区 button')];buttons.forEach(b=>b.disabled=true);状态('正在处理…','正处理…');try{await fn();}catch(e){状态(e.message);}finally{buttons.forEach(b=>b.disabled=false);更新验证提示();}}
元素('切换账户模式').addEventListener('click',()=>{注册模式=!注册模式;元素('接受区域').hidden=!注册模式;元素('接受条款').required=注册模式;元素('账户密码').minLength=注册模式?15:1;元素('账户密码').autocomplete=注册模式?'new-password':'current-password';文案(元素('账户提交'),注册模式?'创建账户':'登录',注册模式?'立户':'登入');文案(元素('切换账户模式'),注册模式?'已有账户？登录':'没有账户？注册',注册模式?'已有户？入之':'尚无户？立之');状态('');});
元素('账户表单').addEventListener('submit',e=>{e.preventDefault();执行账户(async()=>{const result=await 请求账户(注册模式?'register':'login',{email:元素('账户邮箱').value,password:元素('账户密码').value,acceptTerms:元素('接受条款').checked,termsVersion:'2026-09-07'});元素('账户密码').value='';await 刷账户();状态(注册模式?(result.emailSent?'账户已创建，请查收验证邮件。':'账户已创建，但邮件未能发送，请点击重发验证邮件。'):'已登录。',注册模式?(result.emailSent?'户已立，请收验信。':'户已立，验信未发，请再发之。'):'已登入。');});});
元素('生成上传令牌').addEventListener('click',()=>执行账户(async()=>{const {token}=await 请求账户('token',{});元素('上传令牌').value=token;元素('令牌区域').hidden=false;状态('新令牌已生成，旧令牌已撤销。','新符已生，旧符已废。');}));
元素('撤销上传令牌').addEventListener('click',()=>执行账户(async()=>{await 请求账户('revoke',{});await 刷账户();状态('全部上传令牌已撤销。','上传诸符已废。');}));
元素('退出账户').addEventListener('click',()=>执行账户(async()=>{await 请求账户('logout',{});await 刷账户();状态('已退出登录。','已退出。');}));
元素('复制上传令牌').addEventListener('click',()=>执行账户(async()=>{await navigator.clipboard.writeText(元素('上传令牌').value);状态('令牌已复制。','符已复制。');}));


// 古曰：券不留于史，须亲按而用。今释：立即移除 URL fragment；邮件扫描器的 GET 不验证邮箱或重置密码。
function 更新验证提示(){
  if(!验证凭据)return;
  元素('确认邮箱').disabled=!当前账户;
  文案(元素('验证步骤'),当前账户?'已登录。请点击下方按钮完成邮箱验证；身份认证标识不会改变。':'已收到验证链接。请先使用注册时的邮箱和密码登录，再点击确认验证。无需重新注册。',当前账户?'已登入，请按下钮验邮箱；身份之记如故。':'已得验信之券。请以立户邮箱密码登入，乃按确认；无须复立户。');
  if(!当前账户&&注册模式)元素('切换账户模式').click();
}
function 读取邮件链接(){
  const 参数=new URLSearchParams(location.hash.slice(1)),verify=参数.get('verify'),reset=参数.get('reset');
  if(!verify&&!reset)return;
  验证凭据=verify;重置凭据=reset;
  history.replaceState(null,'',location.pathname+location.search);
  元素('确认邮箱区').hidden=!verify;元素('重置表单').hidden=!reset;
  更新验证提示();
  const 面板=元素(verify?'确认邮箱区':'重置表单');面板.scrollIntoView({block:'start'});面板.focus({preventScroll:true});
}
window.addEventListener('hashchange',读取邮件链接);
元素('重发验证').addEventListener('click',()=>执行账户(async()=>{await 请求账户('resend-verification',{});状态('验证邮件已发送，请检查收件箱及垃圾邮件。','验信已发，请察信箱。');}));
元素('确认邮箱').addEventListener('click',()=>执行账户(async()=>{await 请求账户('verify-email',{token:验证凭据});验证凭据=null;元素('确认邮箱区').hidden=true;await 刷账户();状态('邮箱验证完成，现在可以生成上传令牌。身份认证标识保持不变。','邮箱已验，可生上传之符；身份之记如故。');}));
元素('忘记密码').addEventListener('click',()=>{元素('找回表单').hidden=false;元素('找回邮箱').value=元素('账户邮箱').value;元素('找回邮箱').focus();});
元素('找回表单').addEventListener('submit',e=>{e.preventDefault();执行账户(async()=>{await 请求账户('forgot-password',{email:元素('找回邮箱').value});状态('若此邮箱已注册，将收到密码重置邮件，请检查收件箱及垃圾邮件。','若此邮箱已有户，将得更密之信，请察信箱。');});});
元素('重置表单').addEventListener('submit',e=>{e.preventDefault();执行账户(async()=>{if(元素('新密码').value!==元素('确认密码').value)throw Error('两次输入的密码不一致');await 请求账户('reset-password',{token:重置凭据,password:元素('新密码').value});重置凭据=null;元素('新密码').value='';元素('确认密码').value='';元素('重置表单').hidden=true;await 刷账户();if(注册模式)元素('切换账户模式').click();状态('密码已重置，旧会话和上传令牌已撤销，请重新登录。','密已更，旧会话及上传之符皆废，请复登入。');});});

读取邮件链接();
刷账户().catch(()=>状态('账户服务暂不可用，请稍后重试。','账户未通，请稍后再试。'));
