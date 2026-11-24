package lila.team

import java.time.Instant

// «Логик»: правила тура Кубка без обращения к базе — их проверяют тесты.
// Тур идёт так: «Ждём» → «Составы открыты» (за formMin до старта) → старт круга
// (за closeSec до начала боёв составы закрываются, проходит жеребьёвка, создаются бои)
// → «Бои идут» → «Круг сыгран». Бои создаёт только старт круга.
object Cup:

  // составы закрываются за 2 минуты до начала боёв; бой стартует не раньше чем через 90 с
  val closeSec = 120L
  val minLeadSec = 90L

  // состояние тура (документ kubok_cup/_id:"run")
  case class Run(
      round: Option[Int] = None, // круг, который сейчас стартует или идёт: составы закрыты
      started: Boolean = false, // бои круга созданы и составы записаны
      holdRound: Option[Int] = None, // перенос старта круга кнопками «Отложить» и «Запустить сейчас»
      holdAt: Option[Instant] = None,
      played: Set[Int] = Set.empty // круги, бои которых закончились
  ):
    def locked = round.isDefined
    def startOf(r: Int, planned: Instant): Instant =
      holdAt.filter(_ => holdRound.contains(r)).getOrElse(planned)

  enum Phase:
    case Idle // нет даты круга или автоматика выключена
    case Waiting(round: Int, startAt: Instant)
    case Rosters(round: Int, startAt: Instant)
    case Starting(round: Int)
    case Playing(round: Int)
    case Decide(round: Int) // круг сыгран, но у части пар нет победителя
    case Done // все круги сыграны

  // следующий круг: первый несыгранный, у которого есть дата
  def nextRound(dates: Map[Int, Instant], run: Run): Option[Int] =
    dates.keys.toList.sorted.find(r => !run.played(r))

  def phase(
      now: Instant,
      dates: Map[Int, Instant],
      run: Run,
      formMin: Int,
      undecided: Int => Boolean // в круге r остались пары без победителя
  ): Phase =
    run.round match
      case Some(r) => if run.started then Phase.Playing(r) else Phase.Starting(r)
      case None =>
        run.played.maxOption.filter(undecided) match
          case Some(r) => Phase.Decide(r)
          case None =>
            nextRound(dates, run) match
              case None => if run.played.nonEmpty then Phase.Done else Phase.Idle
              case Some(r) =>
                val at = run.startOf(r, dates(r))
                if now.isBefore(at.minusSeconds(formMin * 60L)) then Phase.Waiting(r, at)
                else Phase.Rosters(r, at)

  // пора закрывать составы и стартовать круг
  def dueToStart(now: Instant, startAt: Instant): Boolean =
    !now.isBefore(startAt.minusSeconds(closeSec))

  // Старт пропущен больше чем на 30 минут (площадка не работала или дата в прошлом):
  // сама площадка круг не начинает, решает администратор кнопкой «Запустить сейчас»
  val missSec = 1800L
  def missed(now: Instant, startAt: Instant): Boolean = now.isAfter(startAt.plusSeconds(missSec))

  // ключ сетки → круг: "2.1" — пара круга 2, "out2" — товарищеский бой круга 2
  def keyRound(key: String): Option[Int] =
    if key.startsWith("out") then key.drop(3).toIntOption else key.takeWhile(_ != '.').toIntOption

  // время начала боёв: плановое, но не раньше чем через minLeadSec
  def battleStart(now: Instant, startAt: Instant): Instant =
    val earliest = now.plusSeconds(minLeadSec)
    if startAt.isBefore(earliest) then earliest else startAt

  // Жеребьёвка нужна, если сетки нет или в ней не те команды, что сейчас с игроками.
  // Сыгранные пары не трогаем никогда: тогда сетку меняет только администратор.
  def needsDraw[K](slots: List[Option[K]], nonEmpty: Set[K], hasResults: Boolean): Boolean =
    !hasResults && nonEmpty.sizeIs >= 2 && slots.flatten.toSet != nonEmpty

  // Опоздавшие на свободные места, новых команд не создаём. Каждый (от сильного к слабому)
  // идёт в команду, где меньше всего игроков, а среди них — где сумма рейтингов меньше.
  // Возвращает (кто → в какую команду, кому места не хватило).
  def placeLate[K, A](teams: List[(K, List[Int])], late: List[(A, Int)], max: Int): (List[(A, K)], List[A]) =
    val (placed, _, rest) = late
      .sortBy(-_._2)
      .foldLeft((List.empty[(A, K)], teams.toVector, List.empty[A])):
        case ((placed, ts, rest), p) =>
          val free = ts.indices.filter(i => ts(i)._2.size < max)
          if free.isEmpty then (placed, ts, rest :+ p._1)
          else
            val i = free.minBy(i => (ts(i)._2.size, ts(i)._2.sum))
            (placed :+ (p._1 -> ts(i)._1), ts.updated(i, ts(i)._1 -> (ts(i)._2 :+ p._2)), rest)
    (placed, rest)

  // Перенос отмеченных учеников в команду (members — её нынешний состав, у новой команды пусто):
  // либо помещаются все, либо не переносим никого. Текст ошибки или None.
  def moveError[U](picked: Set[U], members: List[U], max: Int): Option[String] =
    val coming = picked -- members
    val free = (max - members.distinct.size).max(0)
    if picked.isEmpty then Some("Отметьте хотя бы одного ученика")
    else if coming.sizeIs <= free then None
    else if free == 0 then Some(s"В команде уже $max игроков. Сначала уберите кого-нибудь из неё.")
    else Some(s"Отмечено учеников: ${coming.size}, а свободных мест в команде: $free. Никто не перенесён.")

  // Ученики, которые числятся в нескольких командах: ученик → команды
  def duplicates[K, U](rosters: List[(K, List[U])]): Map[U, List[K]] =
    rosters
      .flatMap((k, us) => us.distinct.map(_ -> k))
      .groupBy(_._1)
      .collect { case (u, ks) if ks.sizeIs > 1 => u -> ks.map(_._2) }

  // План старта круга: какие пары играют свой бой и какие команды с игроками остались без пары
  // (они играют один товарищеский бой). pairs — пары круга, у которых обе команды известны.
  // Пара, где у одной команды нет игроков: вторая проходит дальше без боя (walkovers).
  case class Plan[K](pairs: List[(String, K, K)], walkovers: List[(String, K)], friendly: List[K])

  def startPlan[K](pairs: List[(String, K, K)], nonEmpty: List[K]): Plan[K] =
    val withPlayers = nonEmpty.toSet
    val playing = pairs.filter((_, a, b) => withPlayers(a) && withPlayers(b))
    val walkovers = pairs.flatMap: (k, a, b) =>
      if withPlayers(a) && !withPlayers(b) then List(k -> a)
      else if withPlayers(b) && !withPlayers(a) then List(k -> b)
      else Nil
    val busy = (playing.flatMap((_, a, b) => List(a, b)) ++ walkovers.map(_._2)).toSet
    Plan(playing, walkovers, nonEmpty.filterNot(busy))
