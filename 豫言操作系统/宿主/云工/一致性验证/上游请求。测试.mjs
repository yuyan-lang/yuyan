import {test as 测试} from 'node:test';
import 断言 from 'node:assert/strict';
import {造宿主, 跑} from '../../../适配/网页上游/一致性验证/测试/桩.mjs';

// 「：汉语：用拒绝 error 模式的网络桩复现 Workers，并验证 GitHub 方法、标头与正文。文言：网络桩拒 error，以拟 Workers；验 GitHub 之法、头与体。：」
测试('GitHub 上游：Workers 模式与写入参数', async () => {
  const 请求们 = [];
  const 网络 = async (网址, 选项) => {
    断言.equal(选项.redirect, 'manual');
    请求们.push({网址, 选项});
    return new Response('{}', {headers: {'content-type': 'application/json'}});
  };
  const 宿主 = 造宿主({网络});
  for (const 方法 of ['POST', 'PATCH', 'PUT']) {
    const 果 = await 跑({op: 'up', url: 'https://api.example.com/json', method: 方法, body: '{"sha":"测试"}', timeout: 300000, headers: [['Content-Type', 'application/json'], ['User-Agent', 'yuyan-cloud'], ['X-GitHub-Api-Version', '2026-03-10']], read: 'none'}, {}, 宿主);
    断言.equal(果.状态, 200, 果.文);
    const 请 = 请求们.at(-1).选项;
    断言.equal(请.method, 方法);
    断言.equal(请.body, '{"sha":"测试"}');
    断言.equal(请.headers['User-Agent'], 'yuyan-cloud');
    断言.equal(请.headers['X-GitHub-Api-Version'], '2026-03-10');
  }
});

测试('静态授权上游：重定向不跟随且释放正文', async () => {
  let 请求数 = 0;
  let 已取消 = false;
  const 网络 = async (网址, 选项) => {
    请求数++;
    断言.equal(选项.redirect, 'manual');
    return new Response(new ReadableStream({cancel() { 已取消 = true; }}), {status: 302, headers: {location: 'https://elsewhere.example.com/'}});
  };
  const 果 = await 跑({op: 'up', url: 'https://api.example.com/redirect', read: 'none'}, {}, 造宿主({网络}));
  断言.match(果.文, /请求失败:.*上游重定向被拒绝/);
  断言.equal(请求数, 1);
  断言.equal(已取消, true);
});
