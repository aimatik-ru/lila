import { h, type VNode } from 'snabbdom';

import type { Prop } from 'lib';
import { bind, rangeConfig } from 'lib/view';

export function settingNodes(
  colors: Prop<boolean>,
  clarity: Prop<number>,
  timer: Prop<number>,
  redraw: () => void,
): VNode[] {
  return [colorsSetting(colors, redraw), claritySetting(clarity, redraw), timerSetting(timer, redraw)];
}

export function colorsSetting(colors: Prop<boolean>, redraw: () => void): VNode {
  return h('div.voice-choices', [
    'Подписи:',
    h(
      'span.btn-rack',
      ['Colors', 'Numbers'].map(pref =>
        h(
          `span.btn-rack__btn`,
          {
            class: { active: colors() === (pref === 'Colors') },
            hook: bind('click', () => colors(pref === 'Colors'), redraw),
          },
          pref === 'Colors' ? 'Цвета' : 'Номера',
        ),
      ),
    ),
  ]);
}

export function claritySetting(clarity: Prop<number>, redraw: () => void): VNode {
  return h('div.voice-setting', [
    h('label', { attrs: { for: 'voice-clarity' } }, 'Чёткость'),
    h('input#voice-clarity', {
      attrs: { type: 'range', min: 0, max: 2, step: 1 },
      hook: rangeConfig(clarity, val => {
        clarity(val);
        redraw();
      }),
    }),
    h('div.range_value', ['Нечётко', 'Средне', 'Чётко'][clarity()]),
  ]);
}

export function timerSetting(timer: Prop<number>, redraw: () => void): VNode {
  return h('div.voice-setting', [
    h('label', { attrs: { for: 'voice-timer' } }, 'Таймер'),
    h('input#voice-timer', {
      attrs: { type: 'range', min: 0, max: 5, step: 1 },
      hook: rangeConfig(timer, val => {
        timer(val);
        redraw();
      }),
    }),
    h('div.range_value', ['Выкл.', '1,5 с', '2 с', '2,5 с', '3 с', '5 с'][timer()]),
  ]);
}
