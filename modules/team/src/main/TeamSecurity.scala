package lila
package team

import cats.derived.*

import lila.core.perm.Granter
import lila.memo.CacheApi.*
import lila.mon.extensions.*

object TeamSecurity:
  enum Permission(val name: String, val desc: String) derives Eq:
    case Public extends Permission("Публичный", "Виден как лидер на странице команды")
    case Settings extends Permission("Настройки", "Меняет настройки и описание")
    case Tour extends Permission("Турниры", "Создаёт командные турниры, управляет ими и вступает в них")
    case Comm extends Permission("Модерация", "Модерирует форум и чаты")
    case Request extends Permission("Заявки", "Принимает и отклоняет заявки на вступление")
    case PmAll extends Permission("Рассылки", "Отправляет участникам новости команды")
    case Kick extends Permission("Исключение", "Исключает участников из команды")
    case Admin extends Permission("Админ", "Управляет правами лидеров")
    def key = toString.toLowerCase
  object Permission:
    type Selector = Permission.type => Permission
    val byKey = values.mapBy(_.key)

  case class LeaderData(name: UserStr, perms: Set[Permission])
  def tableStr(data: List[LeaderData]) =
    data.map(d => s"${d.name} (${d.perms.map(_.key).mkString(" ")})").mkString("\n")

  case class NewPermissions(user: UserId, perms: Set[Permission])

final class TeamSecurity(memberRepo: TeamMemberRepo, userApi: lila.core.user.UserApi, cached: TeamCached)(
    using Executor
):

  import TeamSecurity.*

  def setPermissions(t: Team.WithLeaders, data: List[LeaderData])(using by: Me): Fu[List[NewPermissions]] =
    for
      _ <- memberRepo.unsetAllPerms(t.id)
      _ <- memberRepo.setAllPerms(t.id, data)
    yield
      val changes = data.flatMap: d =>
        t.leaders
          .find(_.user.is(d.name))
          .filter(_.perms != d.perms && d.perms.nonEmpty)
          .map: l =>
            NewPermissions(l.user, d.perms)
      t.leaders.map(_.user).foreach(cached.nbRequests.invalidate)
      data.map(_.name.id).foreach(cached.nbRequests.invalidate)
      logger.info(s"valid setLeaders ${t.id} by ${by.userId}: ${tableStr(data)}")
      changes.toList

  def addLeader(team: Team.WithLeaders, name: UserStr): Funit =
    memberRepo.setPerms(team.id, name.id, Set(Permission.Public))

  object form:
    import play.api.data.*
    import play.api.data.Forms.*

    def addLeader(t: Team.WithLeaders)(using Me): Form[UserStr] = Form:
      single:
        "name" -> lila.common.Form.username.historicalField
          .verifying(
            s"Лидеров может быть не больше ${Team.maxLeaders}",
            _ => t.leaders.sizeIs < Team.maxLeaders.value
          )
          .verifying(
            "Нельзя сделать «Логик» лидером",
            n => Granter(_.ManageTeam) || n.isnt(UserId.lichess)
          )
          .verifying(
            "Этот пользователь уже лидер команды",
            n => !t.leaders.exists(_.is(n))
          )
          .verifying(
            "Этот пользователь не состоит в команде",
            n => memberRepo.exists(t.id, n).await(1.second, "team member exists")
          )

    private val permissionsForm = mapping(
      "name" -> lila.common.Form.username.historicalField,
      "perms" -> seq(nonEmptyText)
        .transform[Set[Permission]](
          _.flatMap(Permission.byKey.get).toSet,
          _.toList.map(_.key)
        )
    )(LeaderData.apply)(unapply)

    def permissions(t: Team.WithLeaders)(using me: Me): Form[List[LeaderData]] = Form(
      single("leaders" -> list(permissionsForm))
        .verifying(
          "Нельзя сделать «Логик» лидером",
          Granter(_.ManageTeam) ||
            !_.exists(_.name.is(UserId.lichess)) ||
            t.leaders.exists(_.is(UserId.lichess))
        )
        .verifying(
          "Хотя бы один лидер должен управлять правами",
          _.exists(_.perms(Permission.Admin))
        )
        .verifying(
          "Через форму прав нельзя добавлять или удалять лидеров",
          _.map(_.name.id).toSet == t.leaders.map(_.user).toSet
        )
        .verifying(
          s"Лидеров может быть не больше ${Team.maxLeaders}",
          _.sizeIs <= Team.maxLeaders.value
        )
        .verifying(
          "Имя лидера указано дважды",
          d => d.map(_.name).distinct.sizeIs == d.size
        )
        .verifying(
          "Нельзя лишить прав создателя команды",
          d =>
            Granter(_.ManageTeam) || !t.hasAdminCreator || d.exists: l =>
              l.name.is(t.createdBy) && l.perms(Permission.Admin)
        )
        .verifying(
          "Детские аккаунты не могут управлять правами лидеров",
          d =>
            userApi
              .filterKid(d.filter(_.perms(Permission.Admin)).map(_.name))
              .await(1.second, "team leader kids")
              .isEmpty
        )
        .verifying(
          "Админов может быть не больше 3",
          _.count(_.perms(Permission.Admin)) <= 3
        )
    ).fill(t.leaders.map(m => LeaderData(m.user.into(UserStr), m.perms)))
