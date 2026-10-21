import { h } from 'snabbdom';

import { numberFormat } from 'lib/i18n';
import { onInsert, spinnerHtml } from 'lib/view';
import { userLink } from 'lib/view/userLink';

import type Ctrl from './ctrl';
import { registerFormHandler } from './insight.refresh';

const shareStates = ['никому', 'только друзьям', 'всем'];

export function tutor() {
  return h(
    'a.tutor-link',
    {
      attrs: { href: '/tutor' },
    },
    [
      h('img', {
        attrs: { src: site.asset.flairSrc('nature.octopus-howard') },
      }),
      h('span', [h('strong', 'Попробуйте «Тьютора»'), h('em', 'Сравните себя с другими!')]),
    ],
  );
}

export function info(ctrl: Ctrl) {
  const shareText = 'Доступно: ' + shareStates[ctrl.user.shareId] + '.';
  return h('div.info.box', [
    h('div.top', userLink(ctrl.user)),
    h('div.content', [
      h('p', ['Аналитика по рейтинговым партиям: ', h('strong', numberFormat(ctrl.user.nbGames)), '.']),
      h(
        'p.share',
        ctrl.own
          ? h(
              'a',
              {
                attrs: {
                  href: '/account/preferences/privacy#shareYourInsightsData',
                  target: '_blank',
                },
              },
              shareText,
            )
          : shareText,
      ),
    ]),
    h(
      'div.refresh',
      ctrl.env.user.stale
        ? h('div.insight-stale', [
            h('p', 'Появились новые партии для анализа!'),
            h(
              'form.insight-refresh',
              {
                attrs: {
                  action: `/insights/refresh/${ctrl.env.user.id}`,
                  method: 'post',
                },
                hook: onInsert(registerFormHandler),
              },
              [
                h('button.button.button-thin', 'Обновить аналитику'),
                h(
                  'div.crunching.none',
                  {
                    hook: onInsert(el => el.insertAdjacentHTML('afterbegin', spinnerHtml)),
                  },
                  [h('br'), h('p', h('strong', 'Обрабатываем данные специально для вас!'))],
                ),
              ],
            ),
          ])
        : null,
    ),
  ]);
}
