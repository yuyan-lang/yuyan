// 文言：手柄能力之单测：不需浏览器与 Wasm，惟以伪导航供 getGamepads 与接拔之事，验数、号、名、态、枢、连、候、振。
// 汉语：手柄能力的单测：不需要浏览器与 Wasm 产物，用假的导航对象提供 getGamepads 与接入、拔出的监听，验数、号、名、态、枢、连、候、振。
// 运行：node --test 豫言操作系统/宿主/浏览器/操纵。测试.mjs
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {创建手柄能力} from './操纵.mjs';

// 文言：造一伪柄：钮值依序、首枢四分之一负、有振动之器。汉语：造一个假手柄：按钮值按序给、第一个轴为负四分之一、带振动执行器。
const 造柄 = (号, 名, 振能 = true) => ({
  index: 号,
  id: 名,
  connected: true,
  mapping: 'standard',
  buttons: [
    {pressed: true, value: 1},
    {pressed: false, value: 0},
    {pressed: false, value: 0},
    {pressed: false, value: 0},
    {pressed: false, value: 0},
    {pressed: false, value: 0},
    {pressed: true, value: 0.5},
    {pressed: false, value: 0},
    ...Array.from({length: 9}, () => ({pressed: false, value: 0})),
  ],
  axes: [-0.25, 0.5, 0, 0],
  ...(振能 ? {vibrationActuator: {playEffect: () => Promise.resolve('complete')}} : {}),
});

const 造全局 = () => {
  const 听 = new Map();
  return {
    听,
    addEventListener: (名, 函) => { 听.set(名, 函); },
  };
};

test('有手柄时读状态与名字', () => {
  const 柄 = 造柄(0, '甲柄');
  const 导航 = {getGamepads: () => [柄, null]};
  const 全局 = 造全局();
  const 能力 = 创建手柄能力(导航, 全局);
  assert.equal(能力.数(), 1);
  assert.equal(能力.号(0), 0);
  assert.equal(能力.名(0), '甲柄');
  assert.equal(能力.钮(0, 0), 1000);
  assert.equal(能力.钮(0, 6), 500);
  assert.equal(能力.钮(0, 99), 0);
  assert.equal(能力.枢(0, 0), -250);
  assert.equal(能力.枢(0, 1), 500);
  assert.equal(能力.枢(0, 99), 0);
  assert.deepEqual(能力.态列(0).slice(0, 2), [1000, 0]);
  assert.deepEqual(能力.枢列(0), [-250, 500, 0, 0]);
  assert.equal(能力.连(0), true);
  assert.deepEqual(能力.候(100), [0, '', 0]);
  assert.equal(能力.振(0, 500, 200), 0);
  assert.equal(能力.振(9, 500, 200), 3);
});

test('接拔事件进队列', () => {
  const 柄 = 造柄(2, '乙柄');
  let 接着 = false;
  const 导航 = {getGamepads: () => 接着 ? [null, null, 柄] : [null, null, null]};
  const 全局 = 造全局();
  const 能力 = 创建手柄能力(导航, 全局);
  assert.deepEqual(能力.候(0), [0, '', 0]);
  接着 = true;
  全局.听.get('gamepadconnected')({gamepad: 柄});
  assert.deepEqual(能力.候(0), [1, '乙柄', 2]);
  assert.equal(能力.连(2), true);
  接着 = false;
  全局.听.get('gamepaddisconnected')({gamepad: 柄});
  assert.deepEqual(能力.候(0), [2, '乙柄', 2]);
  assert.equal(能力.连(2), false);
  assert.deepEqual(能力.态列(2), null);
});

test('没有手柄接口时答不支持', () => {
  const 能力 = 创建手柄能力({}, 造全局());
  assert.equal(能力.数(), -1);
  assert.deepEqual(能力.候(0), [3, '', 0]);
  assert.equal(能力.振(0, 500, 200), 3);
  assert.equal(能力.名(0), '');
});

test('没有振动能力的手柄答二', () => {
  const 柄 = 造柄(0, '无振柄', false);
  const 能力 = 创建手柄能力({getGamepads: () => [柄]}, 造全局());
  assert.equal(能力.振(0, 500, 200), 2);
});
