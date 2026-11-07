import { toggle } from 'lib';
import { displayColumns, isTouchDevice } from 'lib/device';
import { playable } from 'lib/game';
import { licon } from 'lib/licon';
import { storage } from 'lib/storage';
import {
  bind,
  dataIcon,
  hl,
  type LooseVNode,
  type VNode,
  boardMenu as menuDropdown,
  boolPrefXhrToggle,
} from 'lib/view';
import { cmnToggleWrap } from 'lib/view/cmn-toggle';

import type RoundController from '../ctrl';

// Логик: «Поделиться и экспортировать», как в мобильном приложении Lichess.
let copiedGameId: string | undefined;

const gameUrl = (ctrl: RoundController): string => {
  const d = ctrl.data;
  return `${location.origin}/${d.game.id}${d.player.spectator ? '' : `/${d.player.color}`}`;
};

const canShare = (data?: ShareData): boolean =>
  typeof navigator.share === 'function' && (!data || !navigator.canShare || navigator.canShare(data));

const shareGame = async (ctrl: RoundController): Promise<void> => {
  const url = gameUrl(ctrl);
  if (canShare()) {
    await navigator.share({ title: 'Партия на площадке «Логик»', url }).catch(() => {});
    return;
  }
  await navigator.clipboard?.writeText(url).catch(() => {});
  copiedGameId = ctrl.data.game.id;
  ctrl.redraw();
  setTimeout(() => {
    copiedGameId = undefined;
    ctrl.redraw();
  }, 2000);
};

const sharePgn = async (ctrl: RoundController): Promise<void> => {
  const id = ctrl.data.game.id;
  const pgn = await fetch(`/game/export/${id}?clocks=0&evals=0`)
    .then(r => (r.ok ? r.text() : ''))
    .catch(() => '');
  if (!pgn) return;
  const file = new File([pgn], `logik-${id}.pgn`, { type: 'text/plain' });
  if (canShare({ files: [file] })) await navigator.share({ files: [file] }).catch(() => {});
  else await navigator.share({ text: pgn }).catch(() => {});
};

const shareSection = (ctrl: RoundController): VNode => {
  const d = ctrl.data;
  const finished = !playable(d);
  const copied = copiedGameId === d.game.id;
  return hl('section.board-menu__share', { key: 'logic-share' }, [
    hl(
      'button.button.text',
      {
        key: 'logic-share-link',
        attrs: { type: 'button', ...dataIcon(copied ? licon.Checkmark : licon.Link) },
        hook: bind('click', () => void shareGame(ctrl)),
      },
      copied ? 'Ссылка скопирована' : canShare() ? 'Поделиться партией' : 'Скопировать ссылку',
    ),
    finished &&
      canShare() &&
      hl(
        'button.button.text',
        {
          key: 'logic-share-pgn',
          attrs: { type: 'button', ...dataIcon(licon.Clipboard) },
          hook: bind('click', () => void sharePgn(ctrl)),
        },
        'Поделиться PGN',
      ),
    finished &&
      hl(
        'a.button.text',
        {
          key: 'logic-pgn-download',
          attrs: {
            href: `/game/export/${d.game.id}.pgn?clocks=0&evals=0`,
            download: `logik-${d.game.id}.pgn`,
            ...dataIcon(licon.Download),
          },
        },
        'Скачать PGN',
      ),
  ]);
};

export default function (ctrl: RoundController): LooseVNode {
  return menuDropdown(ctrl.redraw, ctrl.menu, menu => {
    const d = ctrl.data;
    const spectator = d.player.spectator;
    const portraitMobile = displayColumns() === 1 && isTouchDevice();
    const swapClockStorage = storage.boolean('swapClock');
    return [
      hl('section', [
        menu.flip(i18n.site.flipBoard, ctrl.flip, () => {
          ctrl.flipNow();
          ctrl.menu.toggle();
        }),
      ]),
      shareSection(ctrl),
      hl('section', [
        menu.zenMode(true),
        menu.blindfold(
          toggle(ctrl.blindfold(), v => ctrl.blindfold(v)),
          !spectator,
        ),
        'vibrate' in navigator &&
          cmnToggleWrap({
            id: 'haptics',
            name: 'Вибрация при ходе',
            checked: ctrl.vibration(),
            change: ctrl.vibration,
            redraw: ctrl.redraw,
          }),
        !portraitMobile &&
          cmnToggleWrap({
            id: 'streamer',
            name: 'Режим стримера',
            checked: ctrl.streamer(),
            change: ctrl.streamerMode,
            redraw: ctrl.redraw,
          }),
        portraitMobile &&
          cmnToggleWrap({
            id: 'swapClock',
            name: 'Часы слева',
            checked: swapClockStorage.get(),
            change: swapClockStorage.set,
            redraw: ctrl.redraw,
          }),

        menu.voiceInput(boolPrefXhrToggle('voice', !!ctrl.voiceMove), !spectator),
        !portraitMobile &&
          menu.keyboardInput(boolPrefXhrToggle('keyboardMove', !!ctrl.keyboardMove), !spectator),
        !spectator && (d.pref.submitMove || ctrl.voiceMove)
          ? menu.confirmMove(ctrl.confirmMoveToggle)
          : undefined,
      ]),
      hl('section.board-menu__links', [
        hl(
          'a',
          { attrs: { target: '_blank', href: '/account/preferences/display' } },
          i18n.preferences.display,
        ),
        hl(
          'a',
          { attrs: { target: '_blank', href: '/account/preferences/game-behavior ' } },
          i18n.preferences.gameBehavior,
        ),
      ]),
    ];
  });
}
