import { h, type Hooks, type VNode } from 'snabbdom';

import { requestIdleCallbackSafe } from 'lib';
import { throttle } from 'lib/async';
import { licon } from 'lib/licon';
import type { Gamebook, TreeNode } from 'lib/tree/types';
import { bind, type MaybeVNodes, onInsert, icon } from 'lib/view';

import type AnalyseCtrl from '@/ctrl';

export const running = (ctrl: AnalyseCtrl): boolean =>
  !!ctrl.study &&
  ctrl.study.data.chapter.gamebook &&
  !ctrl.gamebookPlay() &&
  ctrl.study.vm.gamebookOverride !== 'analyse';

export function render(ctrl: AnalyseCtrl): VNode {
  const study = ctrl.study!;
  const isMyMove = ctrl.turnColor() === ctrl.data.orientation;
  const isCommented = (ctrl.node.comments || []).some(c => c.text.length > 2);
  const hasVariation = ctrl.tree.parentNode(ctrl.path).children.length > 1;

  let content: MaybeVNodes;

  const commentHook: Hooks = bind(
    'click',
    () => {
      study.commentForm.start(study.vm.chapterId, ctrl.path, ctrl.node);
      study.vm.toolTab('comments');
      requestIdleCallbackSafe(
        () =>
          $('#comment-text').each(function (this: HTMLTextAreaElement) {
            this.focus();
          }),
        500,
      );
    },
    ctrl.redraw,
  );

  if (!ctrl.path) {
    if (isMyMove)
      content = [
        h('div.legend.todo.clickable', { hook: commentHook, class: { done: isCommented } }, [
          icon(licon.BubbleSpeech)(),
          h('p', 'Помогите ученику найти первый ход: напишите комментарий.'),
        ]),
        renderHint(ctrl),
      ];
    else
      content = [
        h('div.legend.clickable', { hook: commentHook }, [
          icon(licon.BubbleSpeech)(),
          h('p', 'Начните интерактивный урок с комментария'),
        ]),
        h('div.legend.todo', { class: { done: !!ctrl.node.children[0] } }, [
          icon(licon.PlayTriangle)(),
          h('p', 'Сделайте на доске первый ход соперника.'),
        ]),
      ];
  } else if (ctrl.onMainline) {
    if (isMyMove)
      content = [
        h('div.legend.todo.clickable', { hook: commentHook, class: { done: isCommented } }, [
          icon(licon.BubbleSpeech)(),
          h('p', 'Объясните ход соперника и помогите ученику найти следующий ход: напишите комментарий.'),
        ]),
        renderHint(ctrl),
      ];
    else
      content = [
        h('div.legend.clickable', { hook: commentHook }, [
          icon(licon.BubbleSpeech)(),
          h(
            'p',
            'Можно прокомментировать верный ход ученика или оставить поле пустым, ' +
              'чтобы сразу перейти к следующему ходу.',
          ),
        ]),
        hasVariation
          ? null
          : h('div.legend.clickable', { hook: bind('click', ctrl.navigate.prev, ctrl.redraw) }, [
              icon(licon.PlayTriangle)(),
              h('p', 'Добавьте варианты, чтобы объяснить, почему другие ходы неверны.'),
            ]),
        renderDeviation(ctrl),
      ];
  } else
    content = [
      h('div.legend.todo.clickable', { hook: commentHook, class: { done: isCommented } }, [
        icon(licon.BubbleSpeech)(),
        h('p', 'Объясните в комментарии, почему этот ход неверен'),
      ]),
      h('div.legend', [h('p', 'Или сделайте его главной линией, если это верный ход.')]),
    ];

  return h(
    'div.gamebook-edit',
    { hook: onInsert(() => site.asset.loadCssPath('analyse.gamebook.edit')) },
    content,
  );
}

function renderDeviation(ctrl: AnalyseCtrl): VNode {
  const field = 'deviation';
  return h('div.deviation', [
    h('div.legend.todo', { class: { done: nodeGamebookValue(ctrl.node, field).length > 2 } }, [
      icon(licon.BubbleSpeech)(),
      h('p', 'Если сыгран любой другой неверный ход:'),
    ]),
    h('textarea', {
      attrs: { placeholder: 'Объясните, почему все остальные ходы неверны' },
      hook: textareaHook(ctrl, field),
    }),
  ]);
}

const renderHint = (ctrl: AnalyseCtrl): VNode =>
  h('div.hint', [
    h('div.legend', [icon(licon.InfoCircle)(), h('p', 'Подсказка для ученика по запросу (необязательно):')]),
    h('textarea', {
      attrs: { placeholder: 'Дайте ученику подсказку, чтобы он нашёл верный ход' },
      hook: textareaHook(ctrl, 'hint'),
    }),
  ]);

const saveNode = throttle(500, (ctrl: AnalyseCtrl, gamebook: Gamebook) => {
  ctrl.socket.send('setGamebook', {
    path: ctrl.path,
    ch: ctrl.study!.vm.chapterId,
    gamebook,
  });
  ctrl.redraw();
});

const nodeGamebookValue = (node: TreeNode, field: 'deviation' | 'hint'): string =>
  node.gamebook?.[field] || '';

function textareaHook(ctrl: AnalyseCtrl, field: 'deviation' | 'hint'): Hooks {
  const value = nodeGamebookValue(ctrl.node, field);
  return {
    insert(vnode: VNode) {
      const el = vnode.elm as HTMLInputElement;
      el.value = value;
      el.oninput = () => {
        const node = ctrl.node;
        node.gamebook = node.gamebook || {};
        node.gamebook[field] = el.value.trim();
        saveNode(ctrl, node.gamebook);
      };
      vnode.data!.path = ctrl.path;
    },
    postpatch(old: VNode, vnode: VNode) {
      if (old.data!.path !== ctrl.path) (vnode.elm as HTMLInputElement).value = value;
      vnode.data!.path = ctrl.path;
    },
  };
}
