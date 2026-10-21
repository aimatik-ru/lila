import { h } from 'snabbdom';

import { licon } from 'lib/licon';
import { bind, dataIcon } from 'lib/view';

import type Ctrl from './ctrl';
import type { Preset } from './interfaces';

export default function (ctrl: Ctrl) {
  return h(
    'div.box.presets',
    (ctrl.ui.asMod ? modPresets : basePresets).map(p =>
      h(
        'a.preset.text',
        {
          class: { active: ctrl.makeUrl(p.dimension, p.metric, p.filters) === ctrl.makeCurrentUrl() },
          attrs: dataIcon(licon.Target),
          hook: bind('click', () => ctrl.setQuestion(p)),
        },
        p.name,
      ),
    ),
  );
}

const basePresets: Preset[] = [
  {
    name: 'Против кого я набираю больше рейтинга: слабых или сильных соперников?',
    dimension: 'opponentStrength',
    metric: 'ratingDiff',
    filters: {},
  },
  {
    name: 'Как быстро я хожу каждой фигурой в пуле и блице?',
    dimension: 'piece',
    metric: 'movetime',
    filters: {
      variant: ['bullet', 'blitz'],
    },
  },
  {
    name: 'Как часто я выигрываю любимыми дебютами за белых?',
    dimension: 'openingVariation',
    metric: 'result',
    filters: {
      variant: ['bullet', 'blitz', 'rapid', 'classical', 'correspondence'],
      color: ['white'],
    },
  },
  {
    name: 'Как часто я наказываю зевки соперника в каждой стадии партии?',
    dimension: 'phase',
    metric: 'awareness',
    filters: {},
  },
  {
    name: 'Набираю ли я рейтинг, когда не делаю короткую рокировку?',
    dimension: 'myCastling',
    metric: 'ratingDiff',
    filters: {
      myCastling: ['2', '3'],
    },
  },
  {
    name: 'Чем заканчиваются партии, когда я меняю ферзей?',
    dimension: 'queenTrade',
    metric: 'result',
    filters: {
      queenTrade: ['true'],
    },
  },
  {
    name: 'Какой средний рейтинг у моих соперников в каждом виде шахмат?',
    dimension: 'variant',
    metric: 'opponentRating',
    filters: {},
  },
  {
    name: 'Насколько точно я хожу каждой фигурой в дебюте?',
    dimension: 'piece',
    metric: 'accuracy',
    filters: {
      phase: ['1'],
    },
  },
];

const modPresets: Preset[] = [
  {
    name: 'ACPL by date',
    dimension: 'date',
    metric: 'acpl',
    filters: {},
  },
  {
    name: 'Blurs by date',
    dimension: 'date',
    metric: 'blurs',
    filters: {},
  },
  {
    name: 'ACPL by blur',
    dimension: 'blur',
    metric: 'acpl',
    filters: {},
  },
  {
    name: 'Blurs by result',
    dimension: 'result',
    metric: 'blurs',
    filters: {},
  },
  {
    name: 'ACPL by time variance',
    dimension: 'timeVariance',
    metric: 'acpl',
    filters: {},
  },
  {
    name: 'Blur by time variance',
    dimension: 'timeVariance',
    metric: 'blurs',
    filters: {},
  },
  {
    name: 'Time variance by date',
    dimension: 'date',
    metric: 'timeVariance',
    filters: {},
  },
];
