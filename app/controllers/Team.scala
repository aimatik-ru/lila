package controllers
package team

import play.api.data.Form
import play.api.libs.json.*
import play.api.mvc.*

import lila.app.{ *, given }
import lila.common.Json.given
import lila.core.LightUser
import lila.core.msg.SystemMsg
import lila.team.{ Requesting, Team as TeamModel, TeamMember, TeamSecurity }

final class Team(env: Env) extends LilaController(env):

  import env.team.{ forms, api, paginator }

  def all(page: Int) = Open:
    Reasonable(page):
      Ok.async:
        paginator
          .popularTeams(page)
          .map:
            views.team.list.all(_)

  def home(page: Int) = Open:
    ctx.me
      .so(api.hasTeams(_))
      .map:
        if _ then Redirect(routes.Team.mine)
        else Redirect(routes.Team.all(page))

  def show(id: TeamId, page: Int, mod: Boolean) = Open:
    if id.value == "kubok" then // «Логик»: доска кубка + регламент из CMS-страницы kubok-rules
      for
        teams <- api.cupBoard
        rules <- env.cms.renderKey("kubok-rules")
        canEdit = lila.core.perm.Granter.opt(_.Pages)
        shown = rules.filter(r => r.live || canEdit)
        editUrl = canEdit.option:
          rules.fold(routes.Cms.createForm(lila.core.id.CmsPageKey("kubok-rules").some).url)(r =>
            routes.Cms.edit(r.id).url
          )
        bracket <- cupBracketState
        dates <- api.cupBracketDates
        page <- renderPage(
          views.team.list.cup(
            teams,
            shown.map(_.html),
            editUrl,
            bracket.map((rounds, tours) => (rounds, tours.view.mapValues(cupTourView).toMap)),
            dates.view.mapValues(d => cupMsk(d)).collect { case (k, Some(v)) => k -> v }.toMap,
            dates
          )
        )
      yield Ok(page)
    else
      Reasonable(page):
        WithTeamOrClas(id): team =>
          if !team.notable && req.client.isCrawler
          then notFound
          else renderTeam(team, page, mod && canEnterModView)

  private def CanSeeMembers(team: TeamModel)(f: => Fu[lila.ui.Page])(using ctx: Context): Fu[Result] =
    val canSee = fuccess(team.publicMembers || isGrantedOpt(_.ManageTeam)) >>|
      ctx.useMe(api.isMember(team.id))
    canSee.flatMap:
      if _ then Ok.async(f) else authorizationFailed

  def members(id: TeamId, page: Int) = Open:
    Reasonable(page, Max(50)):
      WithEnabledTeamOrClas(id): team =>
        CanSeeMembers(team):
          paginator
            .teamMembersWithDate(team, page)
            .map(views.team.membersPage(team, _))

  def search(text: String, page: Int) = OpenBody:
    Reasonable(page):
      Ok.async:
        if text.trim.isEmpty
        then paginator.popularTeams(page).map { views.team.list.all(_) }
        else
          for
            ids <- env.teamSearch(text, page)
            teams <- ids.mapFutureList(env.team.teamRepo.byOrderedIds)
            forMe <- teams.mapFutureList(env.team.memberRepo.addMyLeadership)
          yield views.team.list.search(text, forMe)

  private def renderTeam(team: TeamModel, page: Int, asMod: Boolean)(using ctx: Context) = for
    show <- api.show(team)
    info <- env.teamInfo(show, withForum = canHaveForum(show.team, asMod))
    members <- paginator.teamMembers(show.team, page)
    log <- (asMod && isGrantedOpt(_.ManageTeam)).so(env.mod.logApi.teamLog(team.id))
    hasChat = canHaveChat(info, asMod)
    chat <- hasChat.optionFu(env.chat.api.userChat.cached.findMine(team.id.into(ChatId)))
    version <- hasChat.optionFu(env.team.version(team.id))
    page <- renderPage(views.team.show(info, members, chat, version, asMod, log))
  yield Ok(page).withCanonical(routes.Team.show(team.id))

  private def canEnterModView(using Context) =
    isGrantedOpt(_.Shusher) || isGrantedOpt(_.ManageTeam)

  private def canHaveChat(info: lila.app.mashup.TeamInfo, requestModView: Boolean)(using
      ctx: Context
  ): Boolean =
    import info.show.team.isChatFor
    info.show.team.enabled && !isChatFor(_.None) && ctx.kid.no && ctx.req.client.isHuman && {
      (isChatFor(_.Leaders) && info.show.ledByMe) ||
      (isChatFor(_.Members) && info.show.mine) ||
      (canEnterModView && requestModView)
    }

  private def canHaveForum(team: TeamModel, asMod: Boolean)(member: Option[TeamMember])(using
      ctx: Context
  ): Boolean =
    team.enabled && !team.isForumFor(_.None) && ctx.kid.no && {
      team.isForumFor(_.Everyone) ||
      (team.isForumFor(_.Leaders) && member.exists(_.perms.nonEmpty)) ||
      (team.isForumFor(_.Members) && member.isDefined) ||
      (isGrantedOpt(_.ModerateForum) && asMod)
    }

  def tournaments(teamId: TeamId) = Open:
    WithEnabledTeamOrClas(teamId): team =>
      CanSeeMembers(team):
        env.teamInfo
          .tournamentsOf(team, 30, 30)
          .map(views.team.tournaments.page(team, _))

  private def renderEdit(team: TeamModel, form: Form[?])(using me: Me, ctx: Context) = for
    member <- env.team.memberRepo.get(team.id, me)
    _ <- env.msg.systemMsg.twoFactorReminder(me)
  yield views.team.form.edit(team, form, member)

  def edit(id: TeamId) = Auth { ctx ?=> me ?=>
    if id.value == "kubok" then
      cupDraftPage(Nil, get("perf") | "rapid", getInt("size") | 5, getInt("teams") | -1, getBool("online"))
    else
      WithOwnedTeamEnabled(id, _.Settings): team =>
        Ok.async(renderEdit(team, forms.edit(team)))
  }

  def update(id: TeamId) = AuthBody { ctx ?=> me ?=>
    if id.value == "kubok" then cupDraftPost
    else
      WithOwnedTeamEnabled(id, _.Settings): team =>
        // «Логик»: капитан кубковой команды переименовывает её коротким полем на странице команды
        val rename = cupRenameForm.bindFromRequest().value.flatten.map(_.trim)
        if team.isCup && rename.isDefined then
          rename.filter(n => n.length >= 3 && n.length <= 40) match
            case Some(name) =>
              api.update(team.copy(name = name)).inject(Redirect(routes.Team.show(team.id)).flashSuccess)
            case None =>
              Redirect(routes.Team.show(team.id)).flashFailure("Название: от 3 до 40 знаков").toFuccess
        else
          bindForm(forms.edit(team))(
            err => BadRequest.async(renderEdit(team, err)),
            data =>
              for
                automodText <- api.update(team, data)
                url = routes.Team.show(team.id).url
                _ <- env.memo.picfitApi.addRef(Markdown(automodText), ref(team.id), url.some)
              yield
                discard { env.report.api.automodComms(team.automodText, url) }
                Redirect(url).flashSuccess
          )
  }

  private val cupRenameForm =
    import play.api.data.Forms.*
    Form(single("cupRename" -> optional(text)))

  // «Логик»: автосбор равных команд кубка из учеников школы, которые сейчас на площадке.
  // GET /team/kubok/edit — список и составы, POST — показать составы (go=0) или создать (go=1).
  private val cupDraftForm =
    import play.api.data.Forms.*
    Form(
      tuple(
        "perf" -> default(text, "rapid"),
        "size" -> default(number(min = 0, max = TeamModel.cupMaxMembers), 3), // 0 = по группам A–E
        "go" -> default(boolean, false),
        "u" -> list(text),
        "t" -> list(text),
        "teams" -> default(number(min = -1, max = 64), -1), // -1 = сколько команд уже есть, 0 = по размеру
        "online" -> default(boolean, false)
      )
    )

  private def cupPerf(key: String) =
    lila.rating
      .PerfKey(key)
      .filter(List("blitz", "rapid", "classical").contains)
      .getOrElse(lila.rating.PerfKey.rapid)

  private def cupPlayers(ids: List[UserId], perf: lila.rating.PerfKey) =
    env.user.api
      .listWithPerfs(ids, includeClosed = false)
      .map(_.map: u =>
        (
          u.user.id,
          u.perfs(perf).intRating.value,
          u.user.roles.nonEmpty,
          TeamModel.cupGroupOf(u.user.flair.map(_.value))
        ))

  private def cupDraftPage(
      picked: List[UserId],
      perfKey: String,
      sizeArg: Int,
      teamsArg: Int,
      onlineOnly: Boolean,
      preview: Option[List[List[(UserId, Int)]]] = none
  )(using ctx: Context, me: Me): Fu[Result] =
    if !isGranted(_.ManageTeam) then authorizationFailed
    else
      val perf = cupPerf(perfKey)
      val size = if sizeArg == 0 then 0 else sizeArg.atLeast(2).atMost(TeamModel.cupMaxMembers)
      for
        school <- env.team.memberRepo.userIdsByTeam(TeamId("logic-all"))
        current <- api.cupCurrentTeam
        board <- api.cupBoard
        teams = if teamsArg < 0 then board.size else teamsArg
        isOn = (u: UserId) => env.socket.isOnline.exec(u)
        shown = if onlineOnly then school.filter(u => isOn(u) || picked.contains(u)) else school
        players <- cupPlayers(shown, perf)
        page <- Ok.page(
          views.team.list.cupDraft(
            perf.value,
            size,
            teams,
            board.size,
            onlineOnly,
            players.sortBy(p => (!isOn(p._1), -p._2)),
            current,
            isOn,
            picked.toSet,
            preview
          )
        )
      yield page

  // «Логик»: сетка плей-офф. Пара = свой «Командный бой» из двух команд; победитель боя
  // сам проходит дальше. Ничья решается дополнительными показателями боя (порядок RankedTeam).
  private def cupBracketState
      : Fu[Option[(List[List[TeamModel.CupMatch]], Map[String, lila.tournament.Tournament])]] =
    api.cupBracketGet.flatMap:
      case None => fuccess(none)
      case Some((slots, tourIds)) =>
        for
          found <- tourIds.toList.sequentially: (k, id) =>
            env.tournament.tournamentRepo.byId(TourId(id)).map(k -> _)
          tours = found.collect { case (k, Some(t)) => k -> t }.toMap
          winners <- tours.toList.sequentially: (k, t) =>
            if !t.isFinished then fuccess(k -> none)
            else
              env.tournament.cached.battle.teamStanding
                .get(t.id)
                .map(rs => k -> rs.headOption.filter(_.score > 0).map(_.teamId))
          won = winners.toMap
        yield (TeamModel.cupBracket(slots, k => won.get(k).flatten), tours).some

  private def cupTourView(t: lila.tournament.Tournament) =
    TeamModel.CupTour(t.id.value, t.startsAt, t.isStarted || t.isFinished, t.isFinished)

  private val cupBracketForm =
    import play.api.data.Forms.*
    Form(
      tuple(
        "bracket" -> optional(text),
        "perf" -> default(text, "rapid"),
        "date" -> optional(text), // datetime-local, московское время
        "minutes" -> default(number(min = 10, max = 360), 40),
        "clock" -> default(text, "5"),
        "inc" -> default(number(min = 0, max = 60), 3),
        "leaders" -> default(number(min = 1, max = 5), 2),
        "rated" -> boolean
      )
    )

  private def cupMsk(d: String): Option[java.time.Instant] =
    scala.util
      .Try(java.time.LocalDateTime.parse(d).atZone(java.time.ZoneId.of("Europe/Moscow")).toInstant)
      .toOption

  private def cupName(raw: String) =
    raw.filter(c => c.isLetterOrDigit || c.isWhitespace || ":.,;'+-".contains(c)).trim.replaceAll("\\s+", " ")

  private def cupLeadersText(n: Int) = n match
    case 1 => "В зачёт идёт лучший результат команды."
    case 5 => "В зачёт идут результаты всех игроков команды."
    case _ =>
      val w = Map(2 -> "двух", 3 -> "трёх", 4 -> "четырёх").getOrElse(n, n.toString)
      s"В зачёт идут результаты $w лучших игроков команды."

  private def cupBracketPost(using ctx: BodyContext[?], me: Me): Fu[Result] =
    val back = Redirect(routes.Team.show(TeamId("kubok")))
    cupBracketForm.bindFromRequest().value match
      case Some((Some("reset"), _, _, _, _, _, _, _)) =>
        api.cupBracketReset.inject(back.flashSuccess("Сетка сброшена"))
      case Some((Some("draw"), perfKey, _, _, _, _, _, _)) =>
        for
          teams <- api.cupBoard
          players <- cupPlayers(teams.flatMap(_._2), cupPerf(perfKey))
          rating = players.map(p => p._1 -> p._2).toMap
          strength = (ids: List[UserId]) =>
            val rs = ids.flatMap(rating.get)
            if rs.isEmpty then 0 else rs.sum / rs.size
          // если хоть в одной команде есть игроки, пустые в сетку не идут (лишние после пересборки);
          // если пусты все — в сетке все, сила 0
          inPlay = if teams.exists(_._2.nonEmpty) then teams.filter(_._2.nonEmpty) else teams
          seeded = inPlay.sortBy(t => -strength(t._2)).map(_._1.id)
          result <-
            if seeded.sizeIs < 2 then fuccess(back.flashFailure("Для сетки нужны хотя бы две команды"))
            else
              api
                .cupBracketDraw(TeamModel.cupSlots(seeded))
                .inject(back.flashSuccess(s"Жеребьёвка: команд ${seeded.size}"))
        yield result
      case Some((Some("dates"), _, _, _, _, _, _, _)) =>
        val data = cupBracketForm.bindFromRequest().data
        val dates =
          (0 until 8).flatMap(r => data.get(s"d$r").map(_.trim).filter(cupMsk(_).isDefined).map(r -> _)).toMap
        api.cupBracketSetDates(dates).inject(back.flashSuccess(s"Даты кругов сохранены: ${dates.size}"))
      case Some((Some("round"), _, date, minutes, clock, inc, leaders, rated)) =>
        val now = java.time.Instant.now
        val future = (i: java.time.Instant) => Option.when(i.isAfter(now.plusSeconds(60)))(i)
        val start = date.flatMap(cupMsk).flatMap(future)
        api.cupBracketDates
          .zip(cupBracketState)
          .flatMap: (planned, state) =>
            state match
              case None => fuccess(back.flashFailure("Сначала проведите жеребьёвку"))
              case Some((rounds, tours)) =>
                // пары, которые можно играть сейчас: обе команды известны, победителя нет,
                // а боя нет вовсе или он закончился без победителя
                val todo = rounds.flatten
                  .filter(m => m.pair.isDefined && m.winner.isEmpty && tours.get(m.key).forall(_.isFinished))
                for
                  teams <- api.cupBoard
                  names = teams.map((t, _, _) => t.id -> t.name).toMap
                  made <- todo.sequentially: m =>
                    val (x, y) = m.pair.get
                    val roundName = TeamModel.cupRoundName(m.round, rounds.size)
                    val (nx, ny) = (names.getOrElse(x, x.value), names.getOrElse(y, y.value))
                    val full = cupName(s"$nx - $ny")
                    val leaderTeams = List(lila.core.team.LightTeam(x, nx, none))
                    val data = Map(
                      "name" -> (if full.sizeIs <= 35 then full else "Кубок Логика"),
                      "clockTime" -> clock,
                      "clockIncrement" -> inc.toString,
                      "minutes" -> minutes.toString,
                      "rated" -> rated.toString,
                      "teamBattleByTeam" -> x.value,
                      "berserkable" -> "true",
                      "streakable" -> "true",
                      "hasChat" -> "true",
                      "description" -> s"Кубок Логика, $roundName: $nx против $ny. ${cupLeadersText(leaders)}"
                    ) ++ start
                      .orElse(planned.get(m.round).flatMap(cupMsk).flatMap(future))
                      .fold(Map("waitMinutes" -> "5"))(s => Map("startDate" -> s.toEpochMilli.toString))
                    env.tournament.forms
                      .create(leaderTeams)
                      .bind(data)
                      .fold(
                        err => fuccess(Left(s"$nx - $ny: ${err.errors.flatMap(_.messages).mkString(", ")}")),
                        setup =>
                          for
                            tour <- env.tournament.api.createTournament(setup, leaderTeams, andJoin = false)
                            _ <- env.tournament.api.teamBattleUpdate(
                              tour,
                              lila.tournament.TeamBattle.DataForm.Setup(s"${x.value}\n${y.value}\n", leaders),
                              env.team.api.filterExistingIdsNoClas
                            )
                            _ <- api.cupBracketSetTour(m.key, tour.id.value)
                          yield Right(tour.id)
                      )
                  errs = made.collect { case Left(e) => e }
                  ok = made.count(_.isRight)
                yield
                  if errs.nonEmpty then
                    back.flashFailure(s"Создано боёв: $ok. Не получилось: ${errs.mkString("; ")}")
                  else if ok == 0 then back.flashFailure("Нет пар, готовых к игре")
                  else back.flashSuccess(s"Создано боёв: $ok")
      // общая арена: все команды кубка с игроками в одном «Командном бою», сетку не трогает
      case Some((Some("all"), _, date, minutes, clock, inc, leaders, rated)) =>
        val start = date.flatMap(cupMsk).filter(_.isAfter(java.time.Instant.now.plusSeconds(60)))
        api.cupBoard.flatMap: teams =>
          val inPlay = teams.filter(_._2.nonEmpty).map((t, _, _) => t)
          inPlay.headOption.filter(_ => inPlay.sizeIs >= 2) match
            case None => fuccess(back.flashFailure("Для общего боя нужны хотя бы две команды с игроками"))
            case Some(first) =>
              val leaderTeams = List(lila.core.team.LightTeam(first.id, first.name, none))
              val data = Map(
                "name" -> "Кубок Логика - общий бой",
                "clockTime" -> clock,
                "clockIncrement" -> inc.toString,
                "minutes" -> minutes.toString,
                "rated" -> rated.toString,
                "teamBattleByTeam" -> first.id.value,
                "berserkable" -> "true",
                "streakable" -> "true",
                "hasChat" -> "true",
                "description" -> s"Кубок Логика, общий бой: играют все команды (${inPlay.size}), соперник — из любой другой команды. ${cupLeadersText(leaders)}"
              ) ++ start.fold(Map("waitMinutes" -> "5"))(s => Map("startDate" -> s.toEpochMilli.toString))
              env.tournament.forms
                .create(leaderTeams)
                .bind(data)
                .fold(
                  err =>
                    fuccess(
                      back.flashFailure(s"Не получилось: ${err.errors.flatMap(_.messages).mkString(", ")}")
                    ),
                  setup =>
                    for
                      tour <- env.tournament.api.createTournament(setup, leaderTeams, andJoin = false)
                      _ <- env.tournament.api.teamBattleUpdate(
                        tour,
                        lila.tournament.TeamBattle.DataForm
                          .Setup(inPlay.map(_.id.value).mkString("", "\n", "\n"), leaders),
                        env.team.api.filterExistingIdsNoClas
                      )
                    yield Redirect(s"/tournament/${tour.id}")
                      .flashSuccess(s"Общий бой создан: команд ${inPlay.size}")
                )
      case _ => fuccess(back)

  private def cupDraftPost(using ctx: BodyContext[?], me: Me): Fu[Result] =
    if !isGranted(_.ManageTeam) then authorizationFailed
    else if cupBracketForm.bindFromRequest().value.exists(_._1.isDefined) then cupBracketPost
    else
      bindForm(cupDraftForm)(
        _ => Redirect(routes.Team.edit(TeamId("kubok"))),
        (perfKey, size, go, us, ts, teams, onlineOnly) =>
          env.team.memberRepo.userIdsByTeam(TeamId("logic-all")).flatMap { schoolList =>
            val school = schoolList.toSet
            def ids(xs: List[String]) = xs.flatMap(UserStr.read).map(_.id).filter(school).distinct
            if go then
              val rosters = ts.map(t => ids(t.split(',').toList)).filter(_.nonEmpty)
              api
                .refillCupTeams(rosters)
                .map: (filled, created) =>
                  Redirect(routes.Team.show(TeamId("kubok"))).flashSuccess(
                    s"Составы обновлены: команд ${filled + created}" +
                      (if created > 0 then s", из них новых $created" else "") +
                      ". Проведите жеребьёвку заново, чтобы пары встали по силе."
                  )
            else
              val picked = ids(us)
              for
                players <- cupPlayers(picked, cupPerf(perfKey))
                draft =
                  if size == 0 then
                    TeamModel.cupDraftByGroups(players.map(p => (p._1, p._2, p._4)), teams.atLeast(0))
                  else if teams > 0 then TeamModel.cupDraftInto(players.map(p => (p._1, p._2)), teams)
                  else TeamModel.cupDraft(players.map(p => (p._1, p._2)), size)
                page <- cupDraftPage(picked, perfKey, size, teams, onlineOnly, draft.some)
              yield page
          }
      )

  def kickForm(id: TeamId) = Auth { ctx ?=> _ ?=>
    WithOwnedTeamEnabled(id, _.Kick): team =>
      api.blocklist
        .get(team)
        .flatMap: blocklist =>
          Ok.page(views.team.admin.kick(team, forms.members, forms.blocklist.fill(blocklist)))
  }

  def kick(id: TeamId) = AuthBody { ctx ?=> me ?=>
    WithOwnedTeamEnabled(id, _.Kick): team =>
      forms.members
        .bindFromRequest()
        .value
        .so { api.kickMembers(team, _) }
        .inject(Redirect(routes.Team.show(team.id)).flashSuccess)
  }

  def blocklist(id: TeamId) = AuthBody { ctx ?=> me ?=>
    WithOwnedTeamEnabled(id, _.Kick): team =>
      forms.blocklist
        .bindFromRequest()
        .value
        .so { api.blocklist.set(team, _) }
        .inject(Redirect(routes.Team.show(team.id)).flashSuccess)
  }

  def leaders(id: TeamId) = Auth { ctx ?=> _ ?=>
    WithOwnedTeamEnabled(id, _.Admin): team =>
      api
        .withLeaders(team)
        .flatMap: team =>
          Ok.page(leadersPage(team, None, None))
  }

  def permissions(id: TeamId) = AuthBody { ctx ?=> me ?=>
    WithOwnedTeamEnabledWithModInfo(id, _.Admin): (team, asMod) =>
      api
        .withLeaders(team)
        .flatMap: team =>
          bindForm(env.team.security.form.permissions(team))(
            err => BadRequest.page(leadersPage(team, None, err.some)),
            data =>
              env.team.security
                .setPermissions(team, data)
                .flatMap:
                  _.filter(_.user.isnt(me))
                    .sequentially: change =>
                      val text = lila.msg.MsgPreset.newPermissions(
                        if asMod then LightUser.fallback(UserName.lichess) else me.light,
                        team.team.light,
                        change.perms.map(_.name),
                        routeUrl(routes.Team.show(team.id))
                      )
                      env.msg.api.systemPost(SystemMsg.mustRead(change.user, text))
                    .inject:
                      Redirect(routes.Team.leaders(team.id)).flashSuccess
          )
  }

  def addLeader(id: TeamId) = AuthBody { ctx ?=> me ?=>
    WithOwnedTeamEnabled(id, _.Admin): team =>
      api
        .withLeaders(team)
        .flatMap: team =>
          bindForm(env.team.security.form.addLeader(team))(
            err => BadRequest.page(leadersPage(team, err.some, None)),
            name =>
              env.team.security
                .addLeader(team, name)
                .inject(Redirect(routes.Team.leaders(team.id)).flashSuccess)
          )
  }

  private def leadersPage(
      team: TeamModel.WithLeaders,
      addLeader: Option[Form[UserStr]],
      permissions: Option[Form[List[TeamSecurity.LeaderData]]]
  )(using Context, Me) = views.team.admin.leaders(
    team,
    addLeader | env.team.security.form.addLeader(team),
    permissions | env.team.security.form.permissions(team)
  )

  def close(id: TeamId) = SecureBody(_.ManageTeam) { ctx ?=> me ?=>
    WithTeamOrClas(id): team =>
      bindForm(forms.explain)(
        _ => funit,
        explain =>
          api.delete(team, me.value, explain) >>
            env.mod.logApi.deleteTeam(team.id, explain)
      )
        .inject(Redirect(routes.Team.all(1)).flashSuccess)
  }

  def disable(id: TeamId) = AuthBody { ctx ?=> me ?=>
    WithOwnedTeamEnabled(id, _.Admin): team =>
      val redirect = Redirect(routes.Team.show(id))
      bindForm(forms.explain)(
        _ => redirect.flashFailure,
        explain =>
          for
            _ <- api.toggleEnabled(team, explain)
            _ <- env.mod.logApi.toggleTeam(team.id, team.enabled, explain)
          yield redirect.flashSuccess
      )
  }

  private def LimitPerWeek[A <: Result](a: => Fu[A])(using ctx: Context, me: Me): Fu[Result] =
    api.countCreatedRecently(me).flatMap { count =>
      val allow =
        isGrantedOpt(_.ManageTeam) ||
          (isGrantedOpt(_.Verified) && count < 100) ||
          count < 3
      if allow then a
      else Forbidden.page(views.site.message.teamCreateLimit)
    }

  def form = Auth { ctx ?=> me ?=>
    LogicAdminOnly:
      LimitPerWeek:
        Ok.page(views.team.form.create(forms.create, anyCaptcha))
  }

  private val OneAtATime = lila.web.FutureConcurrencyLimit[UserId](
    key = "team.concurrency.user",
    ttl = 10.minutes,
    maxConcurrency = 1
  )

  private def ref(id: TeamId) = s"team:$id"

  def create = AuthBody { ctx ?=> me ?=>
    LogicAdminOnly:
      OneAtATime(me, rateLimited):
        LimitPerWeek:
          JoinLimit(tooManyTeamsHtml):
            bindForm(forms.create)(
              err => BadRequest.page(views.team.form.create(err, anyCaptcha)),
              data =>
                for
                  team <- api.create(data)
                  url = routes.Team.show(team.id).url
                  _ <- env.memo.picfitApi.addRef(Markdown(team.automodText), ref(team.id), url.some)
                yield
                  discard { env.report.api.automodComms(team.automodText, url) }
                  Redirect(url)
            )
  }

  def mine = Auth { ctx ?=> me ?=>
    Ok.async:
      api.mine.map:
        views.team.list.mine(_)
  }

  private def tooManyTeamsHtml(using Context, Me): Fu[Result] =
    BadRequest.async:
      api.mine.map(views.team.list.mine)
  private val tooManyTeamsJson = BadRequest(jsonError("You have joined too many teams"))

  def leader = Auth { ctx ?=> me ?=>
    Ok.async:
      api.teamsLedBy(me).map(views.team.list.ledByMe)
  }

  private def JoinLimit(tooMany: => Fu[Result])(f: => Fu[Result])(using Me) =
    api.hasJoinedTooManyTeams
      .flatMap:
        if _ then tooMany else f

  def join(id: TeamId) = AuthOrScopedBody(_.Team.Write) { ctx ?=> me ?=>
    WithEnabledTeamOrClas(id): team =>
      OneAtATime(me, rateLimited):
        JoinLimit(negotiate(tooManyTeamsHtml, tooManyTeamsJson)):
          negotiate(
            html = webJoin(team, request = none, password = none),
            json = bindForm(forms.apiRequest(team))(
              jsonFormError,
              setup =>
                api
                  .join(team, setup.message, setup.password)
                  .flatMap:
                    case Requesting.Joined => jsonOkResult
                    case Requesting.NeedRequest => JsonBadRequest("This team requires confirmation.")
                    case Requesting.NeedPassword => JsonBadRequest("This team requires a password.")
                    case _ => JsonBadRequest("You cannot join this team.")
            )
          )
  }

  def subscribe(teamId: TeamId) =
    AuthOrScopedBody(_.Team.Write, _.Web.Mobile) { _ ?=> me ?=>
      bindForm(env.team.forms.subscribe)(_ => funit, v => api.subscribe(teamId, me, ~v))
        .inject(jsonOkResult)
    }

  def requests = Auth { _ ?=> me ?=>
    import lila.memo.CacheApi.*
    env.team.cached.nbRequests.invalidate(me)
    Ok.async:
      api.requestsWithUsers(me).map(views.team.request.all)
  }

  def requestForm(id: TeamId) = Auth { _ ?=> me ?=>
    FoundPage(api.requestable(id)): team =>
      views.team.request.requestForm(team, forms.request(team))
  }

  def requestCreate(id: TeamId) = AuthBody { ctx ?=> me ?=>
    Found(api.requestable(id)): team =>
      OneAtATime(me, rateLimited):
        JoinLimit(tooManyTeamsHtml):
          bindForm(forms.request(team))(
            err => BadRequest.page(views.team.request.requestForm(team, err)),
            setup =>
              if team.open then webJoin(team, request = none, password = setup.password)
              else
                setup.message.so: msg =>
                  api.createRequest(team, msg).inject(Redirect(routes.Team.show(team.id)).flashSuccess)
          )
  }

  private def webJoin(team: TeamModel, request: Option[String], password: Option[String])(using
      Me,
      RequestHeader
  ) =
    api
      .join(team, request = request, password = password)
      .flatMap:
        case Requesting.Joined =>
          Redirect(env.web.referrerRedirect.fromReq.fold(routes.Team.show(team.id).url)(_.value)).flashSuccess
        case Requesting.NeedRequest | Requesting.NeedPassword =>
          Redirect(routes.Team.requestForm(team.id)).flashSuccess
        case Requesting.Closed if team.isCup =>
          Redirect(routes.Team.show(team.id))
            .flashFailure(
              s"Команда уже набрана: ${lila.team.Team.cupMaxMembers} из ${lila.team.Team.cupMaxMembers}"
            )
        case _ => Redirect(routes.Team.show(team.id)).flashFailure("You cannot join this team.")

  def requestProcess(requestId: String) = AuthBody { ctx ?=> me ?=>
    Found(for
      requestOption <- api.request(requestId)
      teamOption <- requestOption.so(req => env.team.teamRepo.byId(req.team))
      isGranted <- teamOption.so: team =>
        api.isGranted(team.id, _.Request)
    yield (teamOption.ifTrue(isGranted), requestOption).tupled): (team, request) =>
      bindForm(forms.processRequest)(
        _ => Redirect(routes.Team.show(team.id)),
        (decision, url) => api.processRequest(team, request, decision).inject(Redirect(url))
      )
  }

  def declinedRequests(id: TeamId, page: Int) = AuthBody { ctx ?=> _ ?=>
    WithOwnedTeamEnabled(id, _.Request): team =>
      bindForm(forms.searchDeclinedForm)(
        _ => BadRequest(""),
        userQuery =>
          Ok.async:
            paginator.declinedRequests(team, page, userQuery).map {
              views.team.request.declined(team, _, userQuery)
            }
      )
  }

  def updates(page: Int) = AuthOrScoped(_.Team.Read, _.Web.Mobile) { _ ?=> _ ?=>
    for
      byTeam <- env.team.update.byTeams
      recent <- env.team.update.allRecent(page)
      res <- negotiate(
        html = Ok.page(views.team.update.allRecent(recent, byTeam)),
        json = JsonOk(env.team.update.json.allRecent(recent, byTeam))
      )
    yield res
  }

  def updatesOf(id: TeamId, page: Int) = AuthOrScoped(_.Team.Read, _.Web.Mobile) { ctx ?=> me ?=>
    def orElse = Redirect(routes.Team.updates())
    def showUpdates(team: TeamModel) =
      api
        .isMember(id)
        .flatMap:
          if _ then
            for
              byTeam <- env.team.update.byTeams
              recent <- env.team.update.teamRecentAndMarkRead(team, page)
              subscribed <- api.isSubscribed(team, me)
              res <- negotiate(
                html = Ok.page(views.team.update.teamRecent(recent, byTeam, team, subscribed)),
                json = JsonOk(env.team.update.json.teamRecent(recent, byTeam, team.light, subscribed))
              )
            yield res
          else orElse
    WithEnabledTeamOrClas(id)(showUpdates, orElse)
  }

  def quit(id: TeamId) = AuthOrScoped(_.Team.Write) { ctx ?=> me ?=>
    WithEnabledTeamOrClas(id): team =>
      team.isClas.not.so:
        api
          .withLeaders(team)
          .flatMap: t =>
            val admins = t.leaders.filter(_.hasPerm(_.Admin))
            if admins.nonEmpty && admins.forall(_.is(me))
            then
              val msg = lila.core.i18n.I18nKey.team.onlyLeaderLeavesTeam.txt()
              negotiate(
                html = Redirect(routes.Team.edit(team.id)).flashFailure(msg),
                json = JsonBadRequest(msg)
              )
            else
              api.cancelRequestOrQuit(team) >>
                negotiate(
                  html = Redirect(routes.Team.mine).flashSuccess,
                  json = jsonOkResult
                )
  }

  def autocomplete = Anon:
    get("term").filter(_.nonEmpty) match
      case None => BadRequest("No search term provided")
      case Some(term) =>
        for
          teams <- api.autocomplete(term, 10)
          _ <- env.user.lightUserApi.preloadMany(teams.map(_.createdBy))
        yield JsonOk:
          JsArray(teams.map: team =>
            Json.obj(
              "id" -> team.id,
              "name" -> team.name,
              "owner" -> env.user.lightUserApi.syncFallback(team.createdBy).name,
              "members" -> team.nbMembers
            ))

  def updateNew(id: TeamId) = Auth { ctx ?=> _ ?=>
    WithOwnedTeamEnabled(id, _.PmAll): team =>
      renderUpdateForm(team, forms.update)
  }

  private def renderUpdateForm(team: TeamModel, form: Form[?])(using Context) = for
    tours <- env.tournament.api.visibleByTeam(team.id, 0, 20).dmap(_.next)
    swiss <- env.swiss.api.visibleByTeam(team.id, 0, 20).dmap(_.next)
    unsubs <- env.team.cached.unsubs.get(team.id)
    limiter <- env.team.update.limiter.status(team.id)
    links = views.team.updateEventLinks(tours, swiss)
    page <- renderPage(views.team.admin.updateForm(team, form, links, unsubs, limiter))
  yield Ok(page)

  def updateSend(id: TeamId) = AuthOrScopedBody(_.Team.Lead) { ctx ?=> me ?=>
    WithOwnedTeamEnabled(id, _.PmAll): team =>
      import lila.memo.RateLimit.LimitResult
      bindForm(forms.update)(
        Left(_),
        text => env.team.update.send(team, text).left.map(forms.update.withError("duplicate", _))
      )
        .fold(
          err => negotiate(renderUpdateForm(team, err), BadRequest(errorsAsJson(err))),
          _.flatMap: res =>
            negotiate(
              Redirect(routes.Team.show(team.id)).flashing:
                res match
                  case LimitResult.Through => "success" -> ""
                  case LimitResult.Limited => "failure" -> rateLimitedMsg
              ,
              res match
                case LimitResult.Through => jsonOkResult
                case LimitResult.Limited => rateLimitedJson
            )
        )
  }

  private def WithTeamOrClas(
      teamId: TeamId
  )(f: TeamModel => Fu[Result], orElse: Context ?=> Fu[Result] = notFound)(using
      ctx: Context
  ): Fu[Result] =
    api
      .team(teamId)
      .flatMap:
        _.fold(orElse): team =>
          env.api.clas
            .teamClas(team)
            .flatMap:
              case None => f(team)
              case Some(_) if isGrantedOpt(_.ManageTeam) => f(team)
              case Some(clas) if ctx.useMe(clas.isTeacher) && team.enabled => f(team)
              case Some(clas) => Redirect(routes.Clas.show(clas.id)).toFuccess

  private def WithEnabledTeamOrClas(
      teamId: TeamId
  )(f: TeamModel => Fu[Result], orElse: Context ?=> Fu[Result] = notFound)(using
      ctx: Context
  ): Fu[Result] =
    WithTeamOrClas(teamId)(
      team =>
        if team.enabled || isGrantedOpt(_.ManageTeam) then f(team)
        else orElse,
      orElse
    )

  private def WithOwnedTeam(teamId: TeamId, perm: TeamSecurity.Permission.Selector)(
      f: (TeamModel, AsMod) => Fu[Result]
  )(using Context): Fu[Result] =
    WithTeamOrClas(teamId): team =>
      ctx
        .useMe(api.hasPerm(team.id, perm))
        .flatMap: isGrantedLeader =>
          val asMod = !isGrantedLeader && isGrantedOpt(_.ManageTeam)
          if isGrantedLeader || asMod then f(team, asMod)
          else Redirect(routes.Team.show(team.id))

  private def WithOwnedTeamEnabledWithModInfo(
      teamId: TeamId,
      perm: TeamSecurity.Permission.Selector
  )(f: (TeamModel, AsMod) => Fu[Result])(using Context): Fu[Result] =
    WithOwnedTeam(teamId, perm): (team, asMod) =>
      if team.enabled || isGrantedOpt(_.ManageTeam) then f(team, asMod)
      else notFound

  private def WithOwnedTeamEnabled(
      teamId: TeamId,
      perm: TeamSecurity.Permission.Selector
  )(f: TeamModel => Fu[Result])(using Context): Fu[Result] =
    WithOwnedTeamEnabledWithModInfo(teamId, perm): (team, _) =>
      if team.enabled || isGrantedOpt(_.ManageTeam) then f(team)
      else notFound

  opaque type AsMod = Boolean
  object AsMod extends TotalWrapper[AsMod, Boolean]
