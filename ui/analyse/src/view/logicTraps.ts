// Логик: ловушки в дебюте ставятся прямо с доски анализа.
// Админ делает ходы до опасного включительно и сохраняет подсказку. Список хранится в настройке
// logicTraps, API — /dev/logic-colors?traps=1 (GET &json=1, POST &op=add|del), остальным там 403.
import { escapeHtml } from 'lib';
import { licon } from 'lib/licon';
import { confirm, domDialog, bind, dataIcon, hl, type VNode } from 'lib/view';
import * as xhr from 'lib/xhr';

import type AnalyseCtrl from '@/ctrl';

interface Trap {
  index: number;
  line: string;
  hint: string;
  block: boolean;
  uci: string;
}

const api = '/dev/logic-colors?traps=1';
const startBoard = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR';

let traps: Trap[] = [];
let allowed: boolean | undefined; // undefined — ещё не спрашивали сервер

const load = (ctrl: AnalyseCtrl): void => {
  if (allowed !== undefined) return;
  allowed = false;
  if (!document.body.dataset.user) return;
  xhr.json<Trap[]>(`${api}&json=1`).then(
    list => {
      traps = list;
      allowed = true;
      ctrl.redraw();
    },
    () => {},
  );
};

// SAN → русская запись с номерами, как на сервере (LogicTraps.lineOf).
const ruPiece: Record<string, string> = { K: 'Кр', N: 'К', B: 'С', R: 'Л', Q: 'Ф' };
const ruLine = (sans: string[]): string =>
  sans
    .map((san, i) => {
      const ru = (ruPiece[san[0]] ?? san[0]) + san.slice(1);
      return i % 2 === 0 ? `${i / 2 + 1}.${ru}` : ru;
    })
    .join(' ');

// Ответ сервера — свежий список; при ошибке текст из {"error": ...}.
const post = async (url: string, body?: FormData): Promise<Trap[]> => {
  const res = await xhr.jsonAnyResponse(url, { method: 'post', body });
  const data = await res.json().catch(() => null);
  if (!res.ok || !Array.isArray(data)) throw new Error(data?.error || 'Не сохранилось, попробуйте ещё раз');
  return data;
};

const sansOf = (ctrl: AnalyseCtrl): string[] | undefined => {
  if (ctrl.data.game.variant.key !== 'standard') return;
  if (ctrl.tree.root.fen.split(' ')[0] !== startBoard) return;
  const sans = ctrl.nodeList.slice(1).map(n => n.san || '');
  return sans.length && sans.every(Boolean) ? sans : undefined;
};

export function button(ctrl: AnalyseCtrl): VNode | false {
  load(ctrl);
  if (!allowed) return false;
  return hl(
    'button',
    {
      hook: bind('click', () => {
        ctrl.actionMenu(false);
        ctrl.redraw();
        open(ctrl);
      }),
      attrs: dataIcon(licon.Shield),
    },
    'Ловушка на этот ход',
  );
}

const listHtml = (list: Trap[]): string =>
  list.length
    ? `<ul class="logic-traps__list">${list
        .map(
          t => `<li>
  <div><strong>${escapeHtml(t.line)}</strong> <em>${t.block ? 'запрет' : 'предупреждение'}</em></div>
  ${t.hint ? `<div>${escapeHtml(t.hint)}</div>` : ''}
  <button type="button" class="button button-empty button-red" data-del="${t.index}">Удалить</button>
</li>`,
        )
        .join('')}</ul>`
    : '<p>Ловушек пока нет. Сделайте ходы на доске и сохраните первую.</p>';

async function open(ctrl: AnalyseCtrl): Promise<void> {
  const sans = sansOf(ctrl);
  const line = sans && ruLine(sans);
  const current = line ? traps.find(t => t.line === line && t.uci === ctrl.node.uci) : undefined;
  const form = line
    ? `<p>Опасный ход — последний: <strong>${escapeHtml(line)}</strong></p>
<label><input type="radio" name="block" value="0"${current?.block ? '' : ' checked'}> Предупредить: ученик может подтвердить ход</label><br>
<label><input type="radio" name="block" value="1"${current?.block ? ' checked' : ''}> Запретить: ход сделать нельзя</label>
<textarea name="hint" rows="4" style="width:100%;margin:1em 0" placeholder="Что увидит ученик, например: после 4...К:e4 5.К:e4 d5 чёрные вилкой отыграют фигуру">${escapeHtml(current?.hint || '')}</textarea>
<button type="button" class="button" data-save>${current ? 'Обновить ловушку' : 'Сохранить ловушку'}</button>`
    : '<p>Сделайте на доске ходы от начальной позиции до опасного включительно, затем откройте это окно снова.</p>';
  const dlg = await domDialog({
    htmlText: `<div class="logic-traps" style="max-width:36em">
<h2>Ловушка на ход</h2>
${form}
<p class="logic-traps__error error none"></p>
<h2>Ловушки сейчас</h2>
<div class="logic-traps__wrap">${listHtml(traps)}</div>
<p><a href="${api}">Все ловушки списком</a></p>
</div>`,
    modal: true,
    show: true,
  });
  const view = dlg.view;
  const error = (msg: string) => {
    const el = view.querySelector('.logic-traps__error');
    if (el) {
      el.textContent = msg;
      el.classList.remove('none');
    }
  };
  const refresh = (list: Trap[]) => {
    traps = list;
    const wrap = view.querySelector('.logic-traps__wrap');
    if (wrap) wrap.innerHTML = listHtml(list);
  };
  const failText = (e: unknown) => (e instanceof Error ? e.message : String(e));
  view.querySelector('[data-save]')?.addEventListener('click', async () => {
    if (!sans) return;
    const block = (view.querySelector('input[name=block]:checked') as HTMLInputElement | null)?.value === '1';
    const hint = (view.querySelector('textarea[name=hint]') as HTMLTextAreaElement).value;
    try {
      refresh(
        await post(`${api}&op=add`, xhr.form({ moves: sans.join(' '), hint, block })),
      );
      dlg.close();
    } catch (e) {
      error(failText(e));
    }
  });
  view.addEventListener('click', async e => {
    const btn = (e.target as HTMLElement).closest('[data-del]');
    if (!btn) return;
    if (!(await confirm('Удалить ловушку? Ученики перестанут видеть подсказку на этом ходу.', 'Удалить', 'Оставить')))
      return;
    try {
      refresh(await post(`${api}&op=del&i=${btn.getAttribute('data-del')}`));
    } catch (err) {
      error(failText(err));
    }
  });
}
