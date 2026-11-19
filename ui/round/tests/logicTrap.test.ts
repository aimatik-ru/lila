import assert from 'node:assert/strict';
import { describe, test } from 'node:test';

import { settleWithin } from '../src/logicTrap';

describe('settleWithin', () => {
  test('ждёт быстрый запрос', async () => {
    let done = false;
    await settleWithin(new Promise<void>(r => setTimeout(() => ((done = true), r()), 10)), 500);
    assert.equal(done, true);
  });

  test('не ждёт зависший запрос дольше лимита', async () => {
    const started = Date.now();
    await settleWithin(new Promise(() => {}), 50);
    assert.ok(Date.now() - started < 400);
  });

  test('ошибка запроса не мешает ходу', async () => {
    await settleWithin(Promise.reject(new Error('net')), 500);
  });
});
