// 文言：手柄之能：以 navigator.getGamepads 取页中已接之柄，接拔之事入待取之列；振以 vibrationActuator 发之。
//   钮以千分报（零至一千），枢以千分报（负一千至一千），皆依常式十七钮、四枢。两工厂只依注入之导航与全局，可脱浏览器而单测。
// 汉语：手柄能力的浏览器实现：用 navigator.getGamepads 取页面里已接的手柄，接入与拔出进待取队列；振动用 vibrationActuator。
//   按钮与扳机以千分报（零至一千），摇杆轴以千分报（负一千至一千），按标准布局十七钮、四枢。
//   工厂只依赖注入的导航与全局对象，可以脱离浏览器单测（见同目录 操纵。测试.mjs）。
export function 创建手柄能力(导航 = globalThis.navigator, 全局 = globalThis) {
  const 有接口 = !!导航 && typeof 导航.getGamepads === 'function';
  const 事列 = [];
  const 名记 = new Map();
  const 柄们 = () => {
    if (!有接口) return [];
    const 列 = 导航.getGamepads();
    return 列 ? Array.from(列).filter(柄 => 柄 && 柄.connected !== false) : [];
  };
  const 号之柄 = 号 => 柄们().find(柄 => Number(柄.index) === Number(号)) ?? null;
  // 文言：以千分报，夹于所给之界。汉语：折算为千分整数，夹在给定范围内。
  const 千分 = (值, 低, 高) => Math.max(低, Math.min(高, Math.round(Number(值 ?? 0) * 1000)));
  const 接 = 事 => {
    const 柄 = 事?.gamepad;
    if (!柄 || 柄.connected === false) return;
    const 号 = Number(柄.index), 名 = String(柄.id ?? '');
    名记.set(号, 名);
    事列.push([1, 名, 号]);
  };
  const 拔 = 事 => {
    const 柄 = 事?.gamepad;
    if (!柄) return;
    const 号 = Number(柄.index);
    事列.push([2, 名记.get(号) ?? String(柄.id ?? ''), 号]);
    名记.delete(号);
  };
  if (有接口 && typeof 全局?.addEventListener === 'function') {
    全局.addEventListener('gamepadconnected', 接);
    全局.addEventListener('gamepaddisconnected', 拔);
  }
  return {
    // 文言：已接之柄数；无其接口则负一。汉语：已接手柄数，宿主没有手柄能力时为 -1。
    数: () => 有接口 ? 柄们().length : -1,
    // 文言：第序之柄号，无则负一。汉语：按枚举次序取手柄号，没有为 -1。
    号: 序 => {
      const 柄 = 柄们()[Number(序)];
      return 柄 ? Number(柄.index) : -1;
    },
    // 文言：按号取器名，接时记之。汉语：按号取设备名（接入时记下，拔出后仍可读到）。
    名: 号 => {
      const 柄 = 号之柄(号);
      return 柄 ? String(柄.id ?? '') : (名记.get(Number(号)) ?? '');
    },
    // 文言：一号一序之钮，以千分报；柄不在或序越界则零。汉语：按号与序取一个按钮的千分值，手柄不在或下标越界得 0。
    钮: (号, 序) => {
      const 柄 = 号之柄(号);
      if (!柄) return 0;
      const 钮 = (柄.buttons ?? [])[Number(序)];
      return 钮 ? 千分(钮.value ?? (钮.pressed ? 1 : 0), 0, 1000) : 0;
    },
    // 文言：一号一序之枢，以千分报；柄不在或序越界则零。汉语：按号与序取一个摇杆轴的千分值，手柄不在或下标越界得 0。
    枢: (号, 序) => {
      const 柄 = 号之柄(号);
      if (!柄) return 0;
      const 值 = (柄.axes ?? [])[Number(序)];
      return 值 === undefined ? 0 : 千分(值, -1000, 1000);
    },
    // 文言：十七钮之全列，惟备试与批量之需。汉语：整套十七个按钮的千分值，备用测试与将来的批量取。
    态列: 号 => {
      const 柄 = 号之柄(号);
      if (!柄) return null;
      const 钮们 = 柄.buttons ?? [];
      return Array.from({length: 17}, (_, 序) => {
        const 钮 = 钮们[序];
        return 钮 ? 千分(钮.value ?? (钮.pressed ? 1 : 0), 0, 1000) : 0;
      });
    },
    // 文言：四枢之全列，惟备试与批量之需。汉语：整套四个摇杆轴的千分值，备用测试与将来的批量取。
    枢列: 号 => {
      const 柄 = 号之柄(号);
      if (!柄) return null;
      const 轴们 = 柄.axes ?? [];
      return Array.from({length: 4}, (_, 序) => 千分(轴们[序] ?? 0, -1000, 1000));
    },
    // 文言：柄犹接否。汉语：手柄是否接着。
    连: 号 => !!号之柄(号),
    // 文言：取一接拔之事；数 0 无变、一接、二拔、三无接口。汉语：取一件热插拔事件，第一项 0 无变化、1 接入、2 拔出、3 没有手柄能力；后两项是器名与手柄号。
    候: 毫秒 => {
      if (!有接口) return [3, '', 0];
      if (事列.length > 0) return 事列.shift();
      return [0, '', 0];
    },
    // 文言：振之：成返零，无其能返二，柄不在返三。汉语：振动，成功返回 0，没有振动能力返回 2，手柄已拔出返回 3。
    振: (号, 强, 毫) => {
      const 柄 = 号之柄(号);
      if (!柄) return 3;
      const 作 = 柄.vibrationActuator;
      if (!作 || typeof 作.playEffect !== 'function') return 2;
      const 时 = Number(毫) > 0 ? Number(毫) : 200;
      const 力 = Math.max(0, Math.min(1, Number(强) / 1000));
      try {
        作.playEffect('dual-rumble', {duration: 时, strongMagnitude: 力, weakMagnitude: 力}).catch(() => {});
        return 0;
      } catch { return 2; }
    },
  };
}
