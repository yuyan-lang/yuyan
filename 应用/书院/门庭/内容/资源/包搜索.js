/* 古曰：市列众包，寻之以名。今释：docs 站包列表的搜索与种类筛选。这是 docs 站唯一的一段脚本，手写原生 JavaScript，不经编译器、不联网：按搜索框里输入的字比对包名、所有者与简介（静态渲染时写在卡片的 data-搜寻 上），三个勾选框按种类（卡片的 data-kind：lib、exe、test）过滤，状态行照主站包市场的说法报已展示几个包，没有结果时显示主站同样的提示。 */
(function () {
  'use strict';
  var 文言 = document.documentElement.getAttribute('lang') === 'lzh' || location.pathname.indexOf('/文言/') === 0;
  var 搜索 = document.getElementById('市场搜索');
  var 输入 = document.getElementById('搜索词');
  var 列表 = document.getElementById('市场列表');
  var 状态 = document.getElementById('市场状态');
  var 框们 = document.querySelectorAll('.包过滤框');
  if (!搜索 || !输入 || !列表 || !状态 || !框们.length) {
    return;
  }
  var 卡们 = [];
  for (var 序 = 0; 序 < 列表.children.length; 序 += 1) {
    var 卡 = 列表.children[序];
    if (卡.tagName === 'LI') {
      卡们.push(卡);
    }
  }
  function 种类已选() {
    var 种 = { lib: false, exe: false, test: false };
    for (var 位 = 0; 位 < 框们.length; 位 += 1) {
      var 名 = 框们[位].getAttribute('data-filter');
      if (种.hasOwnProperty(名)) {
        种[名] = 框们[位].checked;
      }
    }
    return 种;
  }
  function 有包文(数) {
    return 文言
      ? '已陈 ' + 数 + ' 包，各示近颁之版。'
      : '已展示 ' + 数 + ' 个包，每个包展示最近发布的版本。';
  }
  function 过滤() {
    var 词 = 输入.value.replace(/^\s+|\s+$/g, '').toLowerCase();
    var 种 = 种类已选();
    var 见 = 0;
    for (var 位 = 0; 位 < 卡们.length; 位 += 1) {
      var 卡 = 卡们[位];
      var 文 = (卡.getAttribute('data-搜寻') || '').toLowerCase();
      var 合词 = 词 === '' || 文.indexOf(词) >= 0;
      var 合种 = 种[卡.getAttribute('data-kind')] !== false;
      if (合词 && 合种) {
        卡.hidden = false;
        见 += 1;
      } else {
        卡.hidden = true;
      }
    }
    if (见 > 0) {
      状态.textContent = 有包文(见);
    } else if (卡们.length === 0) {
      状态.textContent = 文言 ? '未得其包。' : '尚无公开包。可前往个人管理发布第一个包。';
    } else {
      状态.textContent = 文言 ? '未得其包。' : '没有找到匹配的包。';
    }
  }
  输入.addEventListener('input', 过滤);
  搜索.addEventListener('submit', function (事) {
    事.preventDefault();
    过滤();
  });
  for (var 位 = 0; 位 < 框们.length; 位 += 1) {
    框们[位].addEventListener('change', 过滤);
  }
  过滤();
})();
