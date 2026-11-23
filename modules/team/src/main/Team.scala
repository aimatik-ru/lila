package lila.team

import reactivemongo.api.bson.Macros.Annotations.Key
import scalalib.ThreadLocalRandom

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

import lila.core.team.{ Access, LightTeam, TeamData }

case class Team(
    @Key("_id") id: TeamId, // also the url slug
    name: String,
    password: Option[String],
    intro: Option[String],
    description: Markdown,
    descPrivate: Option[Markdown],
    nbMembers: Int,
    enabled: Boolean,
    open: Boolean,
    createdAt: Instant,
    createdBy: UserId,
    chat: Access,
    forum: Access,
    hideMembers: Option[Boolean],
    flair: Option[Flair],
    ofClas: Option[Boolean] = None
):

  inline def slug = id
  inline def disabled = !enabled

  def isChatFor(f: Access.type => Access) =
    chat == f(Access)

  def isForumFor(f: Access.type => Access) =
    forum == f(Access)

  def publicMembers: Boolean = !hideMembers.has(true)

  def passwordMatches(pw: String) =
    password.forall(teamPw => MessageDigest.isEqual(teamPw.getBytes(UTF_8), pw.getBytes(UTF_8)))

  def light = LightTeam(id, name, flair)

  def data = TeamData(id, name, description, nbMembers, createdBy)

  def daysOld = daysBetween(createdAt, nowInstant)

  def notable = open && (nbMembers > 10 || (nbMembers > 1 && daysOld > 7))

  def automodText = s"$name\n${~intro}\n$description"

  def isClas = ~ofClas
  def acceptsMembers = !isClas
  // Кубковая команда «Команда N»: админ создаёт с номером, капитан даёт название
  def isCup = id.value.startsWith(Team.cupIdPrefix)
  def isCupFull = isCup && nbMembers >= Team.cupMaxMembers
  def doesTeamMessages = !isClas

object Team:

  case class WithLeaders(team: Team, leaders: List[TeamMember]):
    export team.*
    def hasAdminCreator = leaders.exists(l => l.is(team.createdBy) && l.hasPerm(_.Admin))

  case class IdAndLeaderIds(id: TeamId, leaderIds: Set[UserId])

  case class WithMyLeadership(team: Team, amLeader: Boolean):
    export team.*

  case class WithPublicLeaderIds(team: Team, publicLeaders: List[UserId])

  private val firstLeader = UserId("aniskov_artyom")

  case class TeamShow(
      team: Team,
      leaders: List[TeamMember],
      member: Option[TeamMember],
      myRequest: Option[TeamRequest],
      requests: List[RequestWithUser],
      update: Option[TeamUpdate[?, UserId]]
  ):
    val mine = member.isDefined
    val ledByMe = member.exists(_.perms.nonEmpty)
    // logic: руководитель клуба Артём Анисков идёт первым, остальные — по дате вступления
    val publicLeaders = leaders.filter(_.hasPerm(_.Public)).sortBy(l => !l.is(firstLeader))
    def havePerm(perm: TeamSecurity.Permission.Selector) = member.exists(_.hasPerm(perm))

  import chess.variant.Variant
  val variants: Map[Variant.LilaKey, LightTeam] = Variant.list.all.view.collect {
    case v if v.exotic =>
      val name = s"Логик ${v.name}"
      v.key -> LightTeam(nameToId(name), name, none)
  }.toMap

  val maxLeaders = Max(10)
  val maxJoin = Max(50)
  val verifiedMaxJoin = Max(150)

  def maxJoin(u: User): Max =
    if u.isVerified then verifiedMaxJoin
    else
      maxJoin.map:
        _.atMost(15 + daysBetween(u.createdAt, nowInstant) / 7)

  opaque type IdsStr = String
  object IdsStr:

    private val separator = ' '

    val empty: IdsStr = ""

    def apply(ids: Iterable[TeamId]): IdsStr = ids.mkString(separator.toString)

    extension (idsStr: IdsStr)
      def contains(teamId: TeamId) =
        idsStr == teamId.value ||
          idsStr.startsWith(s"$teamId$separator") ||
          idsStr.endsWith(s"$separator$teamId") ||
          idsStr.contains(s"$separator$teamId$separator")
      def toArray: Array[TeamId] = TeamId.from(idsStr.split(IdsStr.separator))
      def toList = idsStr.nonEmpty.so(toArray.toList)
      def toSet = idsStr.nonEmpty.so(toArray.toSet)
      def size = idsStr.count(_ == separator) + 1
      def nonEmpty = idsStr.nonEmpty

  def make(
      id: TeamId,
      name: LightTeam.TeamName,
      password: Option[String],
      intro: Option[String],
      description: Markdown,
      descPrivate: Option[Markdown],
      open: Boolean,
      createdBy: UserId
  ): Team = new Team(
    id = id,
    name = name,
    password = password,
    intro = intro,
    description = description,
    descPrivate = descPrivate,
    nbMembers = 1,
    enabled = true,
    open = open,
    createdAt = nowInstant,
    createdBy = createdBy,
    chat = Access.None,
    forum = Access.None,
    hideMembers = none,
    flair = none
  )

  val cupIdPrefix = "kubok-"
  val cupMaxMembers = 5
  private val cupNameRegex = """(?iu)^\s*команда\s*№?\s*(\d{1,3})\s*$""".r

  def cupIdOf(name: String): Option[TeamId] = name match
    case cupNameRegex(n) => TeamId(s"$cupIdPrefix${n.toInt}").some
    case _ => none

  def idFor(name: String): TeamId = cupIdOf(name).getOrElse(nameToId(name))

  // «Логик»: описание обязательно (от 30 знаков) — текст кубка по умолчанию
  val cupDescription =
    "Команда Кубка Логика. Состав и капитана определит жеребьёвка перед стартом. Название капитан придумает вместе с командой."

  // «Логик»: равные по силе команды. Игроки идут от сильного к слабому, каждый — в самую
  // малочисленную команду, а среди них в ту, где сумма рейтингов меньше. Команд столько, чтобы
  // сели все (округление вверх): 57 игроков по 5 → 12 команд, 9 по 5 и 3 по 4.
  def cupDraft[A](players: List[(A, Int)], size: Int): List[List[(A, Int)]] =
    val s = size.atLeast(1)
    cupDraftInto(players, (players.size + s - 1) / s)

  // «Логик»: разложить игроков ровно на `teams` команд (до cupMaxMembers в каждой), лишние — запасные
  def cupDraftInto[A](players: List[(A, Int)], teams: Int): List[List[(A, Int)]] =
    if teams <= 0 || players.isEmpty then Nil
    else
      players
        .sortBy(-_._2)
        .take(teams * cupMaxMembers)
        .foldLeft(Vector.fill(teams)(List.empty[(A, Int)])): (acc, p) =>
          val i = acc.indices.minBy(i => (acc(i).size, acc(i).map(_._2).sum))
          acc.updated(i, acc(i) :+ p)
        .toList

  // «Логик»: опоздавшие без пересборки. Каждый (от сильного к слабому) идёт на свободное место —
  // в команду, где меньше всего игроков, а среди них в ту, где сумма рейтингов меньше.
  // Кому места не хватило — новые команды по cupDraft. Возвращает (кто → в какую, новые составы).
  def cupAddLate[K, A](
      teams: List[(K, List[Int])],
      late: List[(A, Int)]
  ): (List[(A, K)], List[List[(A, Int)]]) =
    val (placed, _, rest) = late
      .sortBy(-_._2)
      .foldLeft((List.empty[(A, K)], teams.toVector, List.empty[(A, Int)])):
        case ((placed, ts, rest), p) =>
          val free = ts.indices.filter(i => ts(i)._2.size < cupMaxMembers)
          if free.isEmpty then (placed, ts, rest :+ p)
          else
            val i = free.minBy(i => (ts(i)._2.size, ts(i)._2.sum))
            (placed :+ (p._1 -> ts(i)._1), ts.updated(i, ts(i)._1 -> (ts(i)._2 :+ p._2)), rest)
    (placed, cupDraft(rest, cupMaxMembers))

  // «Логик»: состав по одному игроку из каждой турнирной группы A–E (значок logic.group-*).
  // Команд столько, сколько игроков в самой малочисленной непустой группе; в группе сильнейший
  // идёт в команду с меньшей суммой. Капитан — сильнейший в команде.
  val cupGroups = List("A", "B", "C", "D", "E")

  def cupGroupOf(flair: Option[String]): Option[String] =
    flair
      .filter(_.startsWith("logic.group-"))
      .flatMap(_.stripPrefix("logic.group-").headOption)
      .map(_.toUpper.toString)
      .filter(cupGroups.contains)

  def cupDraftByGroups[A](players: List[(A, Int, Option[String])], fixed: Int = 0): List[List[(A, Int)]] =
    val byGroup = cupGroups
      .map(g => players.filter(_._3.contains(g)).map(p => (p._1, p._2)).sortBy(-_._2))
      .filter(_.nonEmpty)
    // fixed > 0: ровно столько команд; если в группе игроков меньше, часть команд будет без этой группы
    val teams = if fixed > 0 then fixed else byGroup.map(_.size).minOption.getOrElse(0)
    if teams == 0 then Nil
    else
      byGroup
        .foldLeft(Vector.fill(teams)(List.empty[(A, Int)])): (acc, grp) =>
          // из группы — не больше одного в команду: сначала в меньшие, среди них в слабейшую
          grp
            .take(teams)
            .foldLeft((acc, Set.empty[Int])): (st, p) =>
              val (a, used) = st
              val i = a.indices.filterNot(used).minBy(i => (a(i).size, a(i).map(_._2).sum))
              (a.updated(i, a(i) :+ p), used + i)
            ._1
        .toList
        .map(_.sortBy(-_._2))

  // «Логик»: сетка плей-офф кубка. slots — команды в порядке посева по сетке, None — пустое место
  // (сильнейшие проходят круг без игры). Победителя пары берём из её «Командного боя».
  enum CupSide:
    case Waiting, Empty
    case Of(id: TeamId)

  case class CupMatch(round: Int, index: Int, a: CupSide, b: CupSide, winner: Option[TeamId]):
    def key = s"$round.$index"
    def pair: Option[(TeamId, TeamId)] = (a, b) match
      case (CupSide.Of(x), CupSide.Of(y)) => Some(x -> y)
      case _ => None

  // [1] → [1,2] → [1,4,2,3] → [1,8,4,5,2,7,3,6]: первый встречает последнего, сильные не сходятся рано
  def cupSeedOrder(n: Int): List[Int] =
    Iterator
      .iterate(List(1))(xs => xs.flatMap(s => List(s, xs.size * 2 + 1 - s)))
      .dropWhile(_.size < n)
      .next()

  def cupSlots(seeded: List[TeamId]): List[Option[TeamId]] =
    val n = Iterator.iterate(2)(_ * 2).dropWhile(_ < seeded.size).next()
    cupSeedOrder(n).map(s => seeded.lift(s - 1))

  def cupBracket(slots: List[Option[TeamId]], played: String => Option[TeamId]): List[List[CupMatch]] =
    import CupSide.*
    def mk(r: Int, i: Int, a: CupSide, b: CupSide) =
      val winner = (a, b) match
        case (Of(x), Empty) => Some(x)
        case (Empty, Of(y)) => Some(y)
        case (Of(x), Of(y)) => played(s"$r.$i").filter(w => w == x || w == y)
        case _ => None
      CupMatch(r, i, a, b, winner)
    def side(m: CupMatch): CupSide =
      m.winner.fold(if m.a == Empty && m.b == Empty then Empty else Waiting)(Of(_))
    def next(prev: List[CupMatch]): List[List[CupMatch]] =
      if prev.sizeIs <= 1 then List(prev)
      else
        prev :: next(
          prev
            .grouped(2)
            .toList
            .zipWithIndex
            .map: (ms, i) =>
              mk(prev.head.round + 1, i, side(ms.head), ms.lift(1).fold(Empty)(side))
        )
    val first = slots
      .grouped(2)
      .toList
      .zipWithIndex
      .map: (xs, i) =>
        def s(o: Option[Option[TeamId]]) = o.flatten.fold(Empty)(Of(_))
        mk(0, i, s(xs.headOption), s(xs.lift(1)))
    if first.isEmpty then Nil else next(first)

  // бой пары для показа в сетке (модуль команд не знает турниров)
  case class CupTour(id: String, startsAt: Instant, started: Boolean, finished: Boolean)

  // «Логик»: бой текущего круга для блока «Сейчас играют»: счёт команд по местам
  case class CupLive(
      key: String, // ключ сетки: "круг.пара" или "out<круг>" (товарищеский бой)
      id: String,
      startsAt: Instant,
      finishesAt: Instant,
      started: Boolean,
      finished: Boolean,
      scores: List[(TeamId, Int)],
      playing: Int // партий идёт сейчас (по игрокам за доской / 2)
  )

  // «Логик»: пульт тура для администратора
  case class CupPult(
      phase: Cup.Phase,
      nbRounds: Int,
      auto: Boolean,
      closeAt: Option[Instant], // когда составы закроются сами
      missed: Boolean,
      warnings: List[String],
      undecided: List[CupMatch], // пары сыгранного круга без победителя
      tours: Map[String, String], // ключ сетки → id боя текущего круга
      log: List[(Instant, String, String)]
  )

  def cupRoundName(round: Int, nbRounds: Int) = nbRounds - round match
    case 1 => "Финал"
    case 2 => "1/2 финала"
    case k => s"1/${1 << (k - 1)} финала"

  // «Логик»: шаблон кубка. Кубок в каждый момент один; круги идут раз в everyDays дней от start.
  // За connectMin минут до круга ученики подключаются, за formMin площадка (auto) сама собирает
  // команды из тех, кто онлайн, проводит жеребьёвку и создаёт бои первого круга. Команды — на весь
  // кубок; бои следующих кругов создаются сами, как только в паре известны обе команды.
  case class CupSettings(
      n: Int = 1,
      name: String = "Кубок Логика",
      start: Option[String] = None, // datetime-local, МСК — старт первого круга
      everyDays: Int = 7,
      connectMin: Int = 30,
      formMin: Int = 20,
      auto: Boolean = true,
      size: Int = cupMaxMembers, // игроков в команде; 0 — по одному из каждой группы A–E
      teams: Int = 0, // 0 — сколько получится
      perf: String = "rapid",
      clock: String = "5",
      inc: Int = 3,
      minutes: Int = 40,
      leaders: Int = 5,
      rated: Boolean = true,
      formed: Boolean = false, // команды этого кубка уже собраны
      since: Option[Instant] = None, // когда начат этот кубок (для поиска его боёв)
      by: Option[String] = None // админ, от имени которого площадка создаёт бои
  ):
    def title = s"$name №$n"

  // «Логик»: прошедший кубок в истории — снимок команд, сетки и всех боёв на момент закрытия
  case class CupArchiveTeam(id: String, name: String, members: List[String], captain: Option[String])
  case class CupArchiveBattle(
      id: String,
      name: String,
      startsAt: Instant,
      finished: Boolean,
      standing: List[(String, Int)] // id команды → очки, по местам
  )
  case class CupArchive(
      n: Int,
      name: String,
      closedAt: Instant,
      dates: Map[Int, String],
      teams: List[CupArchiveTeam],
      slots: List[Option[TeamId]],
      tours: Map[String, String],
      winners: Map[String, TeamId],
      battles: List[CupArchiveBattle],
      label: Option[String] = None, // своя подпись вместо «Кубок Логика №N» (например, тестовый тур)
      rules: Option[String] = None // регламент кубка (HTML страницы kubok-rules на момент закрытия)
  ):
    def title = label.getOrElse(s"$name №$n")

  def nameToId(name: String) =
    val slug = scalalib.StringOps.slug(name)
    // if most chars are not latin, go for random slug
    if slug.lengthIs > (name.length / 2) then TeamId(slug) else randomId()

  private[team] def randomId() = TeamId(ThreadLocalRandom.nextString(8))
