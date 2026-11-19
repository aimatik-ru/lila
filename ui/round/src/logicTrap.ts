// Логик: ждать запрос не дольше limitMs. Ошибка и зависшая сеть не должны задерживать ход:
// часы игрока идут, а возврат времени за раздумье — только приятное дополнение.
export const settleWithin = (request: Promise<unknown>, limitMs: number): Promise<void> =>
  new Promise<void>(resolve => {
    const timer = setTimeout(resolve, limitMs);
    const settle = () => {
      clearTimeout(timer);
      resolve();
    };
    request.then(settle, settle);
  });

// Сервер учитывает возврат времени, только пока очередь хода за игроком,
// поэтому подтверждённый ход ждёт запрос, но не дольше этого лимита.
export const refundWaitMs = 1500;
