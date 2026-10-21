package lila.clas

import com.roundeights.hasher.Algo
import play.api.i18n.Lang
import play.api.libs.json.*
import scalalib.actor.AsyncActorSequencers
import scalalib.data.LazyFu

import lila.common.Bus
import lila.core.config.Secret
import lila.core.id.ClasId
import lila.core.misc.clas.{ ClasTeamConfig, ClasTeamUpdate }
import lila.core.user.{ FlagCode, Profile }
import lila.db.dsl.{ *, given }

/* Вход учеников шахматной школы «Логик» по подписанной ссылке из бота MAX.
 *
 * Сайт kalugalogic.ru подписывает HMAC-SHA256 полезную нагрузку: ученик сайта, его
 * группы и тренеры групп. При первом входе создаётся детский аккаунт (managed, как
 * у учеников класса), группа сайта превращается в класс, тренер группы
 * подключается к классу по почте своего аккаунта. Дальше вход идёт в тот же аккаунт.
 * Паролей ученик не знает: новая ссылка — новый вход.
 *
 * Ник собирает сайт (две буквы имени + две фамилии + месяц и год рождения: ilef1085).
 * ФИО и данные с сайта (дата рождения, разряд, ФШР) лежат в карточке ученика в классе —
 * её видят только тренеры; в открытый профиль ребёнка идут только флаг, город и рейтинг ФШР.
 *
 * По разряду с сайта ученик попадает в одну из закрытых команд (Tier): «До 2 юн.» или
 * «1 юн. и выше». Команды ведёт TeamClasSync, как команды классов: состав целиком
 * пересобирается из clas_logic_student, тренеры классов «Логика» — руководители команды
 * и могут делать турниры «только для команды».
 */
final class ClasLogic(
    secret: Option[Secret],
    ownerName: UserStr,
    colls: ClasColls,
    clasApi: ClasApi,
    signup: ClasSignup,
    userRepo: lila.user.UserRepo,
    nameGenerator: NameGenerator
)(using Executor, Scheduler):

  import ClasLogic.*
  import BsonHandlers.given

  private val signer = secret.map(s => Algo.hmac(s.value))

  // одноразовость: ссылку нельзя открыть второй раз, пока она не протухла
  private val usedNonces = lila.memo.CacheApi.scaffeine
    .expireAfterWrite(maxLifetime + 5.minutes)
    .build[String, Boolean]()

  private val sequencer = AsyncActorSequencers[Int](
    maxSize = Max(8),
    expiration = 1.minute,
    timeout = 20.seconds,
    name = "clas.logic",
    lila.mon.asyncActorMonitor.highCardinality
  )

  def enabled = signer.isDefined

  /* Проверяет подпись и срок, гасит одноразовый ключ. */
  def consume(token: String): Option[Payload] =
    for
      s <- signer
      parts = token.split('.')
      if parts.length == 2
      body = parts(0)
      if s.sha256(body).hash_=(parts(1))
      payload <- parse(body)
      now = nowSeconds
      if payload.exp > now && payload.exp <= now + maxLifetime.toSeconds
      if usedNonces.getIfPresent(payload.n).isEmpty
    yield
      usedNonces.put(payload.n, true)
      payload

  /* Запрос статистики для кабинета на сайте: hex HMAC-SHA256 строки "stats:<sid>:<exp>". */
  def statsSigned(sid: Int, exp: Long, sig: String): Boolean =
    signer.exists: s =>
      val now = nowSeconds
      sid > 0 && exp > now && exp <= now + maxLifetime.toSeconds && s.sha256(s"stats:$sid:$exp").hash_=(sig)

  def userIdOf(sid: Int): Fu[Option[UserId]] =
    colls.logicStudent.primitiveOne[UserId](bdoc("_id" -> sid), "userId")

  /* Аккаунт ученика: найденный по ученику сайта или новый. None — аккаунт закрыт. */
  def login(p: Payload): Fu[Option[User]] = sequencer(p.sid):
    userRepo
      .enabledById(ownerName)
      .orFail(s"logic.login.owner $ownerName not found")
      .flatMap: owner =>
        given Me = Me(owner)
        val groups = if p.groups.isEmpty then List(noGroup) else p.groups
        for
          clases <- groups.sequentially(clasOf)
          user <- userOf(p, clases)
          _ <- user.so(u => clases.sequentiallyVoid(ensureStudent(u, p, _)))
          _ <- user.so(syncProfile(_, p))
          _ <- user.so(_ => syncTier(p))
        yield user

  private def userOf(p: Payload, clases: List[Clas])(using Me): Fu[Option[User]] =
    userIdOf(p.sid)
      .flatMap:
        case Some(userId) => userRepo.enabledById(userId)
        case None => clases.headOption.so(createUser(p, _))

  private def createUser(p: Payload, clas: Clas)(using Me): Fu[Option[User]] =
    given Lang = Lang("ru")
    for
      name <- pickName(p.login)
      student <- signup.one.create(clas, ClasForm.CreateStudent(name, Student.RealName(p.name)))
      _ <- colls.logicStudent.insert.one(
        bdoc("_id" -> p.sid, "userId" -> student.student.userId, "createdAt" -> nowInstant)
      )
      user <- userRepo.byId(student.student.userId)
    yield user

  private def pickName(login: Option[String])(using Lang): Fu[UserName] =
    def free(names: List[String]): Fu[UserName] = names match
      case Nil => nameGenerator().orFail("no free username")
      case name :: rest =>
        userRepo.existsSec(UserName(name)).flatMap(if _ then free(rest) else fuccess(UserName(name)))
    val base = login.map(_.trim.toLowerCase).filter(l => l.length <= 26 && validLogin.matches(l))
    free(base.toList.flatMap(b => b :: (2 to 9).map(i => s"$b-$i").toList))

  /* В открытый профиль ребёнка — только флаг, город и рейтинг ФШР; ФИО туда не кладём. */
  private def syncProfile(user: User, p: Payload): Funit =
    val prof = user.profile | Profile()
    val next = prof.copy(
      flag = prof.flag.orElse(FlagCode("RU").some),
      location = prof.location.orElse("Калуга".some),
      rcfRating = p.rcf.orElse(prof.rcfRating)
    )
    (next != prof).so(userRepo.setProfile(user.id, next))

  /* Разряд поменялся (или пропал) — пересобрать обе команды. */
  private def syncTier(p: Payload)(using me: Me): Funit =
    val tier = p.tier.flatMap(Tier.byKey)
    colls.logicStudent
      .primitiveOne[String](bdoc("_id" -> p.sid), "tier")
      .flatMap: prev =>
        (prev != tier.map(_.key)).so:
          colls.logicStudent.update
            .one(bdoc("_id" -> p.sid), tier.fold(unset("tier"))(t => set("tier" -> t.key)))
            .flatMap(_ => Tier.values.toList.sequentiallyVoid(tierTeamSync))

  private def tierTeamSync(t: Tier)(using me: Me): Funit =
    for
      clasIds <- colls.logicGroup.distinctEasy[ClasId, List]("clasId", emptyBdoc)
      teachers <- colls.clas.distinctEasy[UserId, List]("teachers", $inIds(clasIds))
    yield
      val leaders = NonEmptyList(me.userId, teachers.filterNot(_ == me.userId).distinct)
      val students =
        LazyFu(() => colls.logicStudent.distinctEasy[UserId, List]("userId", bdoc("tier" -> t.key)))
      val config = ClasTeamConfig(t.teamName, leaders, students)
      Bus.pub(ClasTeamUpdate(t.clasId, config.some)(using me.some))

  private def clasOf(g: Group)(using me: Me): Fu[Clas] =
    for
      linked <- colls.logicGroup
        .primitiveOne[ClasId](bdoc("_id" -> g.id), "clasId")
        .flatMap(_.so(clasApi.clas.byId))
      clas <- linked.fold(createClas(g))(c => fuccess(c))
      teacher <- g.teacher.flatMap(EmailAddress.from).so(e => userRepo.byEmail(e.normalize))
      synced <- syncClas(clas, g, teacher.filter(_.enabled.yes))
    yield synced

  private def createClas(g: Group)(using me: Me): Fu[Clas] =
    val data = ClasForm.ClasData(
      name = clasName(g),
      desc = "Группа шахматной школы «Логик»",
      teachers = me.username.value,
      canMsg = false,
      hasTeam = false
    )
    for
      clas <- clasApi.clas.create(data)
      _ <- colls.logicGroup.insert.one(bdoc("_id" -> g.id, "clasId" -> clas.id))
    yield clas

  /* Название группы на сайте могли поменять, тренера — назначить позже. */
  private def syncClas(clas: Clas, g: Group, teacher: Option[User]): Fu[Clas] =
    val name = clasName(g)
    val newTeacher = teacher.map(_.id).filterNot(clas.teachers.toList.contains)
    val synced = clas.copy(
      name = name,
      teachers = newTeacher.fold(clas.teachers)(clas.teachers.append),
      archived = none
    )
    if synced == clas then fuccess(clas)
    else
      newTeacher.foreach(clasApi.isTeacher.add)
      colls.clas
        .update
        .one(
          bid(clas.id),
          set("name" -> synced.name, "teachers" -> synced.teachers.toList) ++ unset("archived")
        )
        .inject(synced)

  /* Карточка ученика в классе: ФИО и данные с сайта обновляются при каждом входе. */
  private def ensureStudent(user: User, p: Payload, clas: Clas)(using me: Me): Funit =
    val id = Student.makeId(user.id, clas.id)
    val realName = Student.RealName(p.name.trim.take(100))
    colls.student
      .byId[Student](id.value)
      .flatMap:
        case Some(s) =>
          val notes = mergeNotes(p.info, s.notes)
          (s.isArchived || s.realName != realName || s.notes != notes).so:
            colls.student.update
              .one(bid(id), set("realName" -> realName, "notes" -> notes) ++ unset("archived"))
              .void
        case None =>
          colls.student.insert
            .one(
              Student
                .make(user, clas, me.userId, realName, managed = true)
                .copy(notes = mergeNotes(p.info, ""))
            )
            .void

object ClasLogic:

  val maxLifetime = 30.minutes

  case class Group(id: Int, name: String, teacher: Option[String])
  case class Payload(
      sid: Int,
      name: String,
      groups: List[Group],
      exp: Long,
      n: String,
      login: Option[String] = None,
      info: Option[String] = None,
      rcf: Option[Int] = None,
      tier: Option[String] = None
  )

  // разрядные команды: ключ приходит с сайта (kl_play_rank_tier)
  enum Tier(val key: String, val teamName: String):
    case Junior extends Tier("u2", "До 2 юн.")
    case Senior extends Tier("1j", "1 юн. и выше")
    def clasId = ClasId(s"logic-$key")
  object Tier:
    def byKey(key: String) = values.find(_.key == key)

  private val validLogin = lila.user.nameRules.newUsernameRegex

  // Сверху — данные с сайта (перезаписываются), под чертой — заметки тренера (не трогаем).
  private val notesMark = "———"

  private[clas] def mergeNotes(info: Option[String], current: String): String =
    info.filter(_.trim.nonEmpty).fold(current): i =>
      val teacher = current.indexOf(notesMark) match
        case -1 => current.trim
        case k => current.drop(k + notesMark.length).trim
      s"${i.trim.take(5000)}\n$notesMark\n$teacher".trim

  private val noGroup = Group(0, "Без группы", none)

  private given Reads[Group] = Json.reads[Group]
  private given Reads[Payload] = Json.reads[Payload]

  private def clasName(g: Group) = g.name.trim.take(100) match
    case n if n.length >= 3 => n
    case n => s"Группа $n"

  private def nowSeconds = nowInstant.toEpochMilli / 1000

  private[clas] def parse(body: String): Option[Payload] =
    scala.util
      .Try(String(java.util.Base64.getUrlDecoder.decode(body), "UTF-8"))
      .toOption
      .flatMap(s => scala.util.Try(Json.parse(s)).toOption)
      .flatMap(_.asOpt[Payload])
      .filter(p => p.sid > 0 && p.name.trim.nonEmpty && p.n.length >= 16)
