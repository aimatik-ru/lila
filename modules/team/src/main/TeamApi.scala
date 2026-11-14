package lila.team

import java.time.Period
import scalalib.actor.AsyncActorSequencers
import play.api.mvc.RequestHeader

import lila.common.Bus
import lila.core.perm.Granter
import lila.core.team.*
import lila.core.timeline as tl
import lila.core.userId.UserSearch
import lila.core.notify.{ NotifyApi, NotificationContent }
import lila.db.dsl.{ *, given }
import lila.memo.CacheApi.*

final class TeamApi(
    teamRepo: TeamRepo,
    memberRepo: TeamMemberRepo,
    requestRepo: TeamRequestRepo,
    updateApi: TeamUpdateApi,
    userApi: lila.core.user.UserApi,
    cached: TeamCached,
    notifyApi: NotifyApi,
    chatApi: lila.core.chat.ChatApi,
    spam: lila.core.security.SpamApi
)(using Executor, Scheduler)
    extends lila.core.team.TeamApi:

  import BSONHandlers.given

  export teamRepo.{ filterHideForum, onUserDelete, deleteNewlyCreatedBy, creatorOf }

  private val workQueue = AsyncActorSequencers[TeamId](
    maxSize = Max(8),
    expiration = 30.seconds,
    timeout = 5.seconds,
    name = "team",
    lila.mon.asyncActorMonitor.highCardinality
  )

  def team(id: TeamId) = teamRepo.byId(id)

  def teamEnabled(id: TeamId) = teamRepo.enabled(id)

  def leaderTeam(id: TeamId) = teamRepo.coll.byId[LightTeam](id, bdoc("name" -> true))

  def lightsByTourLeader[U: UserIdOf](leader: U): Fu[List[LightTeam]] =
    memberRepo.teamsLedBy(leader, Some(_.Tour)).flatMap(teamRepo.lightsByIds)

  def lightsOf[U: UserIdOf](member: U): Fu[List[LightTeam]] =
    cached.teamIdsList(member).flatMap(teamRepo.lightsByIds)

  def forumAccessOf(id: TeamId) = cached.forumAccess.get(id)

  def request(id: TeamRequest.ID) = requestRepo.coll.byId[TeamRequest](id)

  def show(team: Team)(using me: Option[Me]): Fu[Team.TeamShow] = for
    leaders <- memberRepo.leaders(team.id)
    member <- me.soUse(memberOf(team.id))
    requests <- (team.enabled && member.exists(_.hasPerm(_.Request))).so(requestsWithUsers(team))
    myRequest <- member.isEmpty.so(me.so(m => requestRepo.find(team.id, m.userId)))
    update <- ((team.enabled && member.isDefined) || Granter.opt(_.ManageTeam))
      .so(updateApi.teamLatest(team.id))
  yield Team.TeamShow(team, leaders, member, myRequest, requests, update)

  def create(setup: TeamSetup)(using me: Me): Fu[Team] =
    val cupId = Team.cupIdOf(setup.name)
    val bestId = cupId.getOrElse(Team.nameToId(setup.name))
    for
      exists <- chatApi.exists(bestId.into(ChatId))
      id = if exists && cupId.isEmpty then Team.randomId() else bestId
      made = Team
        .make(
          id = id,
          name = setup.name,
          password = setup.password,
          intro = setup.intro,
          description = setup.description,
          descPrivate = setup.descPrivate.filter(_.value.nonEmpty),
          open = setup.isOpen,
          createdBy = me
        )
        .copy(chat = setup.chat, forum = setup.forum)
      // в кубковую команду админ не вступает: место — детям, капитаном станет первый вступивший
      team = if made.isCup then made.copy(nbMembers = 0) else made
      _ <- teamRepo.coll.insert.one(team)
      _ <- (!team.isCup).so:
        memberRepo.add(team.id, me.userId, TeamSecurity.Permission.values.toSet).void
    yield
      cached.invalidateTeamIds(me.userId)
      Bus.pub(TeamCreate(team.data))
      lila.common.Bus.pub(tl.Propagate(tl.TeamCreate(me.userId, team.id)).toFollowersOf(me.userId))
      team

  private[team] def createQuietly(team: Team)(using me: Me): Fu[Team] = for
    _ <- teamRepo.coll.insert.one(team)
    _ <- memberRepo.add(team.id, me.userId, TeamSecurity.Permission.values.toSet)
  yield
    cached.invalidateTeamIds(me.userId)
    team

  def update(old: Team, edit: TeamEdit)(using me: Me): Fu[String] = update:
    old.copy(
      name = edit.name.filter(_ => old.isCup).fold(old.name)(_.trim),
      password = edit.password,
      intro = edit.intro,
      description = edit.description.map(spam.replace),
      descPrivate = edit.descPrivate.map(_.map(spam.replace)),
      open = edit.isOpen,
      chat = edit.chat,
      forum = edit.forum,
      hideMembers = Some(edit.hideMembers),
      flair = edit.flair
    )

  def update(team: Team)(using me: Me): Fu[String] =
    import reactivemongo.api.bson.*
    for
      blocklist <- blocklist.get(team)
      _ <- teamRepo.coll.update.one(bid(team.id), bsonWriteDoc(team) ++ bdoc("blocklist" -> blocklist))
      isLeader <- hasPerm(team.id, _.Settings)
    yield
      cached.forumAccess.invalidate(team.id)
      cached.lightCache.invalidate(team.id)
      Bus.pub(TeamUpdate(team.data, byMod = !isLeader))
      team.automodText

  def mine(using me: MyId): Fu[List[Team.WithMyLeadership]] =
    cached.teamIdsList(me).flatMap(teamRepo.byIdsSortPopular).flatMap(memberRepo.addMyLeadership)

  def countTeamsOf(me: Me) =
    cached.teamIds(me).dmap(_.size)

  def hasJoinedTooManyTeams(using me: Me) =
    countTeamsOf(me).dmap(Team.maxJoin(me) < _)

  // «Логик»: публичная доска кубка — команды kubok-N, их участники и капитаны
  def cupBoard: Fu[List[(Team, List[UserId], Set[UserId])]] =
    teamRepo.coll
      .list[Team](bdoc("_id" -> bdoc("$regex" -> s"^${Team.cupIdPrefix}")) ++ teamRepo.enabledSelect, 200)
      .map(_.sortBy(_.id.value.stripPrefix(Team.cupIdPrefix).toIntOption.getOrElse(Int.MaxValue)))
      .flatMap:
        _.sequentially: team =>
          for
            members <- memberRepo.userIdsByTeam(team.id)
            leaders <- memberRepo.leaderIds(team.id)
          yield (team, members, leaders)

  // «Логик»: сетка плей-офф кубка — один документ: посев (slots, "" = пустое место)
  // и бои пар (tours: "круг.пара:id турнира").
  private lazy val cupBracketColl = teamRepo.coll.db.collection("kubok_bracket")

  def cupBracketGet: Fu[Option[(List[Option[TeamId]], Map[String, String])]] =
    cupBracketColl
      .one[Bdoc](bdoc("_id" -> "kubok"))
      .map(_.map: d =>
        val slots =
          d.getAsOpt[List[String]]("slots").getOrElse(Nil).map(s => Option.when(s.nonEmpty)(TeamId(s)))
        val tours = d
          .getAsOpt[List[String]]("tours")
          .getOrElse(Nil)
          .flatMap(_.split(':') match
            case Array(k, v) => Some(k -> v)
            case _ => None)
          .toMap
        slots -> tours)

  // плановые даты кругов при повторной жеребьёвке сохраняются
  def cupBracketDraw(slots: List[Option[TeamId]]): Funit =
    cupBracketColl.update
      .one(
        bdoc("_id" -> "kubok"),
        bdoc("$set" -> bdoc("slots" -> slots.map(_.fold("")(_.value)), "tours" -> List.empty[String])),
        upsert = true
      )
      .void

  // плановые даты кругов: "круг=yyyy-MM-ddTHH:mm" (московское время, как в datetime-local)
  def cupBracketDates: Fu[Map[Int, String]] =
    cupBracketColl
      .one[Bdoc](bdoc("_id" -> "kubok"))
      .map(
        _.flatMap(_.getAsOpt[List[String]]("dates"))
          .getOrElse(Nil)
          .flatMap(_.split('=') match
            case Array(k, v) => k.toIntOption.map(_ -> v)
            case _ => None)
          .toMap
      )

  def cupBracketSetDates(dates: Map[Int, String]): Funit =
    cupBracketColl.update
      .one(
        bdoc("_id" -> "kubok"),
        bdoc("$set" -> bdoc("dates" -> dates.toList.sortBy(_._1).map((k, v) => s"$k=$v"))),
        upsert = true
      )
      .void

  def cupBracketSetTour(key: String, tourId: String): Funit =
    cupBracketGet.flatMap:
      _.so: (_, tours) =>
        cupBracketColl.update
          .one(
            bdoc("_id" -> "kubok"),
            bdoc("$set" -> bdoc("tours" -> (tours + (key -> tourId)).map((k, v) => s"$k:$v").toList))
          )
          .void

  def cupBracketReset: Funit = cupBracketColl.delete.one(bdoc("_id" -> "kubok")).void

  // «Логик»: кто уже играет в открытых командах кубка — в автосбор их не берём
  def cupMemberIds: Fu[Set[UserId]] = for
    teams <- teamRepo.coll
      .list[Team](bdoc("_id" -> bdoc("$regex" -> s"^${Team.cupIdPrefix}")) ++ teamRepo.enabledSelect, 500)
    ids <- teams.nonEmpty.so:
      memberRepo.coll.distinctEasy[UserId, Set]("user", bdoc("team" -> bdoc("$in" -> teams.map(_.id))))
  yield ids

  // «Логик»: автосбор = переигровка составов. Все участники снимаются со всех команд кубка,
  // составы ложатся в существующие команды по номеру (1, 2, …), недостающие создаются
  // со следующих номеров. Капитан — первый в составе (самый сильный). Лишние старые команды
  // остаются пустыми. Возвращает (заполнено существующих, создано новых).
  def refillCupTeams(rosters: List[List[UserId]])(using me: Me): Fu[(Int, Int)] =
    val clean = rosters.map(_.distinct).filter(_.nonEmpty)
    val seen = scala.collection.mutable.Set.empty[UserId]
    val uniq = clean.map(_.filter(seen.add)).filter(_.nonEmpty) // игрок — только в одной команде
    for
      all <- teamRepo.coll.list[Team](bdoc("_id" -> bdoc("$regex" -> s"^${Team.cupIdPrefix}")), 1000)
      num = (t: Team) => t.id.value.stripPrefix(Team.cupIdPrefix).toIntOption
      existing = all.filter(_.enabled).filter(num(_).isDefined).sortBy(num(_).getOrElse(0))
      _ <- existing.sequentiallyVoid: team =>
        memberRepo.userIdsByTeam(team.id).flatMap(_.sequentiallyVoid(doQuit(team, _)))
      next = all.flatMap(num).maxOption.fold(1)(_ + 1)
      created <- uniq
        .drop(existing.size)
        .zipWithIndex
        .sequentially: (_, i) =>
          val team = Team
            .make(
              id = TeamId(s"${Team.cupIdPrefix}${next + i}"),
              name = s"Команда ${next + i}",
              password = none,
              intro = none,
              description = Markdown(Team.cupDescription),
              descPrivate = none,
              open = true,
              createdBy = me
            )
            .copy(nbMembers = 0, chat = Access.Members)
          teamRepo.coll.insert
            .one(team)
            .inject:
              Bus.pub(TeamCreate(team.data))
              team
      targets = existing ++ created
      _ <- uniq
        .zip(targets)
        .sequentiallyVoid: (roster, team) =>
          for
            _ <- roster.sequentiallyVoid(doJoin(team, _))
            _ <- roster.headOption.so: captain =>
              import TeamSecurity.Permission.*
              memberRepo.setPerms(team.id, captain, Set(Public, Settings, Kick))
          yield ()
    yield (uniq.size.atMost(existing.size), created.size)

  // «Логик»: номер, имя и состав каждой команды кубка — для страницы автосбора
  def cupCurrentTeam: Fu[Map[UserId, String]] =
    cupBoard.map(_.flatMap((t, ms, _) => ms.map(_ -> t.name)).toMap)

  // «Логик»: опоздавшие без пересборки — нынешние составы не трогаем. into: игрок → команда со
  // свободным местом; newRosters: новые команды со следующих номеров, капитан — первый в составе.
  // Возвращает (добавлено в существующие, создано новых команд).
  def addLateCupPlayers(into: List[(UserId, TeamId)], newRosters: List[List[UserId]])(using
      me: Me
  ): Fu[(Int, Int)] =
    for
      inCup <- cupMemberIds
      added <- into
        .filterNot((u, _) => inCup(u))
        .sequentially: (u, id) =>
          teamRepo
            .byId(id)
            .flatMap:
              case Some(team) if team.enabled && !team.isCupFull =>
                for
                  _ <- doJoin(team, u)
                  leaders <- memberRepo.leaders(team.id)
                  _ <- leaders.isEmpty.so:
                    import TeamSecurity.Permission.*
                    memberRepo.setPerms(team.id, u, Set(Public, Settings, Kick))
                yield 1
              case _ => fuccess(0)
      rosters = newRosters.map(_.filterNot(inCup).distinct).filter(_.nonEmpty)
      all <- teamRepo.coll.list[Team](bdoc("_id" -> bdoc("$regex" -> s"^${Team.cupIdPrefix}")), 1000)
      next = all.flatMap(_.id.value.stripPrefix(Team.cupIdPrefix).toIntOption).maxOption.fold(1)(_ + 1)
      _ <- rosters.zipWithIndex.sequentiallyVoid: (roster, i) =>
        val team = Team
          .make(
            id = TeamId(s"${Team.cupIdPrefix}${next + i}"),
            name = s"Команда ${next + i}",
            password = none,
            intro = none,
            description = Markdown(Team.cupDescription),
            descPrivate = none,
            open = true,
            createdBy = me
          )
          .copy(nbMembers = 0, chat = Access.Members)
        for
          _ <- teamRepo.coll.insert.one(team)
          _ = Bus.pub(TeamCreate(team.data))
          _ <- roster.sequentiallyVoid(doJoin(team, _))
          _ <- roster.headOption.so: captain =>
            import TeamSecurity.Permission.*
            memberRepo.setPerms(team.id, captain, Set(Public, Settings, Kick))
        yield ()
    yield (added.sum, rosters.size)

  def hasTeams(me: User): Fu[Boolean] = cached.teamIds(me.id).map(_.nonEmpty)

  def joinedTeamIdsOfUserAsSeenBy(of: User)(using me: Me): Fu[List[TeamId]] =
    cached
      .teamIdsList(of.id)
      .map(_.take(Team.maxJoin(of).value))
      .flatMap: allIds =>
        if Granter(_.AccountInfo) then fuccess(allIds)
        else
          allIds.nonEmpty.so:
            teamRepo.filterHideMembers(allIds).flatMap { hiddenIds =>
              if hiddenIds.isEmpty then fuccess(allIds)
              else
                cached.teamIds(me.userId).map { viewerTeamIds =>
                  allIds.filter: id =>
                    !hiddenIds(id) || viewerTeamIds.contains(id)
                }
            }

  def countCreatedRecently(me: User): Fu[Int] =
    teamRepo.countCreatedSince(me.id, Period.ofWeeks(1))

  def requestsWithUsers(team: Team): Fu[List[RequestWithUser]] =
    requestRepo.findActiveByTeam(team.id, 50).flatMap(requestsWithUsers)

  def declinedRequestsWithUsers(team: Team): Fu[List[RequestWithUser]] =
    requestRepo.findDeclinedByTeam(team.id, 50).flatMap(requestsWithUsers)

  def requestsWithUsers(user: User): Fu[List[RequestWithUser]] = for
    requestManagers <- memberRepo.leadersOf(user, _.Request)
    teamIds = requestManagers.map(_.team)
    requests <- requestRepo.findActiveByTeams(teamIds)
    withUsers <- requestsWithUsers(requests)
  yield withUsers

  private def requestsWithUsers(requests: List[TeamRequest]): Fu[List[RequestWithUser]] =
    userApi
      .listWithPerfs(requests.map(_.user), includeClosed = false)
      .map: users =>
        RequestWithUser.combine(requests, users.filter(_.enabled.yes))

  def join(team: Team, request: Option[String], password: Option[String])(using me: Me): Fu[Requesting] =
    workQueue(team.id):
      if !team.acceptsMembers then fuccess(Requesting.Closed)
      else
        blocklist
          .has(team, me.userId)
          .flatMap:
            if _ then fuccess(Requesting.Blocklist)
            else if team.isCup then joinCup(team, ~password)
            else if team.open then
              if team.passwordMatches(~password)
              then doJoin(team, me.userId).inject(Requesting.Joined)
              else fuccess(Requesting.NeedPassword)
            else motivateOrJoin(team, request)

  // Кубковая команда: не больше 5 игроков, первый вступивший — капитан (меняет название и код входа)
  private def joinCup(stale: Team, password: String)(using me: Me): Fu[Requesting] =
    teamRepo
      .byId(stale.id)
      .flatMap:
        case None => fuccess(Requesting.Closed)
        case Some(team) if team.isCupFull => fuccess(Requesting.Closed)
        case Some(team) if !team.passwordMatches(password) => fuccess(Requesting.NeedPassword)
        case Some(team) =>
          for
            _ <- doJoin(team, me.userId)
            leaders <- memberRepo.leaders(team.id)
            _ <- leaders.isEmpty.so:
              import TeamSecurity.Permission.*
              memberRepo.setPerms(team.id, me.userId, Set(Public, Settings, Kick))
          yield Requesting.Joined

  private def motivateOrJoin(team: Team, msg: Option[String])(using Me) =
    msg.fold(fuccess[Requesting](Requesting.NeedRequest)): txt =>
      createRequest(team, txt).inject(Requesting.Joined)

  def requestable(teamId: TeamId)(using Me): Fu[Option[Team]] = for
    teamOption <- teamEnabled(teamId)
    able <- teamOption.so(requestable)
  yield teamOption.ifTrue(able)

  def requestable(team: Team)(using me: Me): Fu[Boolean] = for
    belongs <- isMember(team.id)
    requested <- requestRepo.exists(team.id, me)
  yield !team.isClas && !belongs && !requested

  def createRequest(team: Team, msg: String)(using me: Me): Funit =
    requestable(team).flatMapz:
      val request = TeamRequest.make(
        team = team.id,
        user = me,
        message = if me.marks.troll then TeamRequest.defaultMessage else msg
      )
      for _ <- requestRepo.coll.insert.one(request) yield cached.nbRequests.invalidate(team.createdBy)

  def cancelRequestOrQuit(team: Team)(using me: Me): Funit =
    requestRepo
      .cancel(team.id, me)
      .flatMap:
        if _ then funit
        else quit(team, me)

  def processRequest(team: Team, request: TeamRequest, decision: String): Funit = workQueue(team.id) {
    if decision == "decline"
    then requestRepo.coll.updateField(bid(request.id), "declined", true).void
    else if decision == "accept"
    then
      for
        _ <- requestRepo.remove(request.id)
        _ <- doJoin(team, request.user)
        _ <- notifyApi.notifyOne(request.user, NotificationContent.TeamJoined(team.id, team.name))
      yield ()
    else funit
  }.addEffect: _ =>
    cached.nbRequests.invalidate(team.createdBy)

  def deleteRequestsByUserId(userId: UserId): Funit =
    requestRepo
      .getByUserId(userId)
      .flatMap:
        _.sequentiallyVoid: request =>
          requestRepo.remove(request.id) >>
            memberRepo
              .leaders(request.team, Some(_.Request))
              .map:
                _.map(_.user).foreach(cached.nbRequests.invalidate)

  private[team] def doJoin(team: Team, userId: UserId): Funit =
    isMember(team.id)(using userId.into(MyId)).not.flatMapz:
      for
        done <- memberRepo.add(team.id, userId)
        _ <- done.so(teamRepo.incMembers(team.id, 1))
      yield
        cached.invalidateTeamIds(userId)
        lila.common.Bus.pub(tl.Propagate(tl.TeamJoin(userId, team.id)).toFollowersOf(userId))
        Bus.pub(JoinTeam(team.id, userId))

  private[team] def addMembers(team: Team, userIds: List[UserId]): Funit =
    userIds
      .sequentially: userId =>
        userApi
          .enabledById(userId)
          .flatMapz: user =>
            memberRepo
              .add(team.id, user.id)
              .addEffect: done =>
                if done then cached.invalidateTeamIds(user.id)
      .flatMap: inserts =>
        teamRepo.incMembers(team.id, inserts.count(identity))

  def teamsOf(username: UserStr) =
    cached.teamIdsList(username.id).flatMap(teamsByIds)

  def teamsByIds(ids: List[TeamId]) =
    teamRepo.coll.byIds[Team, TeamId](ids, _.sec)

  def quit(team: Team, userId: UserId): Funit = workQueue(team.id):
    doQuit(team, userId)

  private def doQuit(team: Team, userId: UserId): Funit =
    for
      res <- memberRepo.remove(team.id, userId)
      _ <- (res.n == 1).so:
        teamRepo.incMembers(team.id, -1)
    yield
      Bus.pub(LeaveTeam(teamId = team.id, userId = userId))
      cached.invalidateTeamIds(userId)

  def quitAllOnAccountClosure(userId: UserId): Fu[List[TeamId]] = for
    teamIds <- cached.teamIdsList(userId)
    _ <- memberRepo.removeByUser(userId)
    _ <- requestRepo.removeByUser(userId)
    _ <- teamIds.sequentially(teamRepo.incMembers(_, -1))
    _ = cached.invalidateTeamIds(userId)
  yield teamIds

  def searchMembersAs(teamId: TeamId, term: UserSearch, nb: Int, showHidden: Boolean)(using
      me: Option[MyId]
  ): Fu[List[UserId]] =
    team(teamId).flatMapz: team =>
      val canSee = fuccess(team.publicMembers) >>| me.ifTrue(showHidden).soUse(cached.isMember(teamId))
      canSee.flatMapz:
        memberRepo.coll.primitive[UserId](
          selector = memberRepo.teamQuery(teamId) ++ bdoc("user".regexStart(term.value)),
          sort = sort.desc("user"),
          nb = nb,
          field = "user"
        )

  def kick(team: Team, userId: UserId)(using me: Me): Funit = workQueue(team.id):
    for
      kicked <- memberRepo.get(team.id, userId)
      myself <- memberRepo.get(team.id, me)
      allowed = userId.isnt(team.createdBy) && kicked.exists: kicked =>
        Granter(_.ManageTeam) || myself.exists: myself =>
          kicked.perms.isEmpty || myself.hasPerm(_.Admin)
      _ <- allowed.so:
        // create a request to set declined in order to prevent kicked use to rejoin
        val request = TeamRequest.make(team.id, userId, "Исключён из команды", declined = true)
        for
          _ <- requestRepo.coll.update.one(bid(request.id), request, upsert = true)
          _ <- doQuit(team, userId)
        yield Bus.pub(KickFromTeam(teamId = team.id, teamName = team.name, userId = userId))
    yield ()

  def kickMembers(team: Team, users: List[UserId])(using me: Me, req: RequestHeader): Funit =
    val client = lila.common.HTTPRequest.printClient(req)
    logger.info:
      s"kick members ${users.size} by ${me.username} from play.kalugalogic.ru/team/${team.slug} $client | ${users.mkString(" ")}"
    users.sequentiallyVoid(kick(team, _))

  object blocklist:
    def set(team: Team, list: String): Funit =
      teamRepo.coll.updateOrUnsetField(bid(team.id), "blocklist", list.nonEmpty.option(list)).void
    def get(team: Team): Fu[String] =
      teamRepo.coll
        .primitiveOne[String](bid(team.id), "blocklist")
        .dmap(~_)
    def has(team: Team, user: UserId): Fu[Boolean] =
      get(team).map: list =>
        UserStr.from(list.split("\n")).exists(_.is(user))

  def toggleEnabled(team: Team, explain: String)(using me: Me): Funit =
    isCreatorGranted(team, _.Admin).flatMap: activeCreator =>
      if Granter(_.ManageTeam) || me.is(team.createdBy) || !activeCreator
      then
        logger.info(s"toggleEnabled ${team.id}: ${!team.enabled} by @${me}: $explain")
        if team.enabled then
          for
            _ <- teamRepo.disable(team)
            _ <- invalidateTeamIdsOfMembers(team.id)
            _ <- requestRepo.removeByTeam(team.id)
          yield ()
        else
          for _ <- teamRepo.enable(team)
          yield Bus.pub(TeamUpdate(team.data, byMod = Granter(_.ManageTeam)))
      else memberRepo.setPerms(team.id, me, Set.empty)

  def invalidateTeamIdsOfMembers(team: TeamId): Funit =
    for users <- memberRepo.userIdsByTeam(team)
    yield users.foreach(cached.invalidateTeamIds)

  def idAndLeaderIds(teamId: TeamId): Fu[Option[Team.IdAndLeaderIds]] =
    memberRepo
      .leaderIds(teamId)
      .map: ids =>
        ids.nonEmpty.option(Team.IdAndLeaderIds(teamId, ids))

  def teamsLedBy[U: UserIdOf](leader: U): Fu[List[Team]] = for
    ids <- memberRepo.teamsLedBy(leader, None)
    teams <- teamRepo.byIdsSortPopular(ids)
  yield teams

  export teamRepo.cursor
  export memberRepo.{ publicLeaderIds, leaderIds, isSubscribed, subscribe, filterUserIdsInTeam }
  export cached.isMember

  // delete forever, with members but not forums
  def delete(team: Team, by: User, explain: String): Funit = for
    _ <- teamRepo.coll.delete.one(bid(team.id))
    _ <- memberRepo.removeByTeam(team.id)
  yield logger.info(s"delete team ${team.id} by @${by.id}: $explain")

  def syncBelongsTo(teamId: TeamId, userId: UserId): Boolean =
    cached.syncTeamIds(userId).contains(teamId)

  def clasMemberCheck(teamId: TeamId)(using me: Option[MyId]): Fu[Boolean] =
    for
      isClas <- teamRepo.isClas(teamId)
      ok <- if isClas then me.soUse(isMember(teamId)) else fuTrue
    yield ok

  def memberOf(teamId: TeamId)(using myId: MyId): Fu[Option[TeamMember]] =
    isMember(teamId).flatMapz:
      memberRepo.get(teamId, myId)

  def isCreatorGranted(team: Team, perm: TeamSecurity.Permission.Selector): Fu[Boolean] =
    memberRepo.hasPerm(team.id, team.createdBy, perm)

  def isLeader(team: TeamId)(using myId: MyId) =
    isMember(team).flatMapz:
      memberRepo.hasAnyPerm(team, myId)

  def isGranted(team: TeamId, perm: TeamSecurity.Permission.Selector)(using Me) =
    fuccess(Granter(_.ManageTeam)) >>|
      hasPerm(team, perm)

  def hasPerm(team: TeamId, perm: TeamSecurity.Permission.Selector)(using myId: MyId): Fu[Boolean] =
    isMember(team).flatMapz:
      memberRepo.hasPerm(team, myId, perm)

  def hasCommPerm(team: TeamId)(using MyId): Fu[Boolean] = hasPerm(team, _.Comm)

  def isLeaderOf[U: UserIdOf](leader: UserId, member: U) =
    cached
      .teamIdsList(member)
      .flatMap:
        memberRepo.leadsOneOf(leader, _)

  def withLeaders(team: Team): Fu[Team.WithLeaders] =
    memberRepo.leaders(team.id).map(Team.WithLeaders(team, _))

  def filterExistingIdsNoClas(ids: Set[TeamId]): Fu[Set[TeamId]] =
    teamRepo.coll.distinctEasy[TeamId, Set]("_id", inIds(ids) ++ teamRepo.noClasSelect, _.sec)

  def autocomplete(term: String, max: Int): Fu[List[Team]] =
    teamRepo.coll
      .find:
        bdoc(
          "name".regexStart(java.util.regex.Pattern.quote(term), "i"),
          "enabled" -> true
        )
      .sort(sort.desc("nbMembers"))
      .cursor[Team](ReadPref.sec)
      .list(max)

  export cached.nbRequests.get as nbRequests
