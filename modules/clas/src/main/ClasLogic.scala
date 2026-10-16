package lila.clas

import com.roundeights.hasher.Algo
import play.api.i18n.Lang
import play.api.libs.json.*
import scalalib.actor.AsyncActorSequencers

import lila.core.config.Secret
import lila.core.id.ClasId
import lila.db.dsl.{ *, given }

/* Вход учеников шахматной школы «Логик» по подписанной ссылке из бота MAX.
 *
 * Сайт kalugalogic.ru подписывает HMAC-SHA256 полезную нагрузку: ученик сайта, его
 * группы и тренеры групп. При первом входе создаётся детский аккаунт (managed, как
 * у учеников класса), группа сайта превращается в класс, тренер группы
 * подключается к классу по почте своего аккаунта. Дальше вход идёт в тот же аккаунт.
 * Паролей ученик не знает: новая ссылка — новый вход.
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
        yield user

  private def userOf(p: Payload, clases: List[Clas])(using Me): Fu[Option[User]] =
    colls.logicStudent
      .primitiveOne[UserId](bdoc("_id" -> p.sid), "userId")
      .flatMap:
        case Some(userId) => userRepo.enabledById(userId)
        case None => clases.headOption.so(createUser(p, _))

  private def createUser(p: Payload, clas: Clas)(using Me): Fu[Option[User]] =
    given Lang = Lang("ru")
    for
      name <- nameGenerator().orFail("no free username")
      student <- signup.one.create(clas, ClasForm.CreateStudent(name, Student.RealName(p.name)))
      _ <- colls.logicStudent.insert.one(
        bdoc("_id" -> p.sid, "userId" -> student.student.userId, "createdAt" -> nowInstant)
      )
      user <- userRepo.byId(student.student.userId)
    yield user

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

  private def ensureStudent(user: User, p: Payload, clas: Clas)(using me: Me): Funit =
    val id = Student.makeId(user.id, clas.id)
    colls.student
      .byId[Student](id.value)
      .flatMap:
        case Some(s) if s.isArchived => colls.student.unsetField(bid(id), "archived").void
        case Some(_) => funit
        case None =>
          colls.student.insert
            .one(Student.make(user, clas, me.userId, Student.RealName(p.name), managed = true))
            .void

object ClasLogic:

  val maxLifetime = 30.minutes

  case class Group(id: Int, name: String, teacher: Option[String])
  case class Payload(sid: Int, name: String, groups: List[Group], exp: Long, n: String)

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
