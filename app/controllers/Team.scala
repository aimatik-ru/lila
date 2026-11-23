package controllers
package team

import play.api.data.Form
import play.api.libs.json.*
import play.api.mvc.*

import lila.app.{ *, given }
import lila.common.Json.given
import lila.core.LightUser
import lila.core.msg.SystemMsg
import lila.db.dsl.distinctEasy
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
    if id.value == "kubok-schedule" then // «Логик»: даты кругов текущего кубка для ленты сайта школы
      for
        s <- api.cupSettings
        st <- cupBracketState
        dates <- api.cupBracketDates
      yield
        // до жеребьёвки: сколько дат админ сохранил, иначе по настройке «Команд»
        val nb = st
          .map(_._1.size)
          .filter(_ > 0)
          .getOrElse:
            dates.keys.maxOption
              .map(_ + 1)
              .getOrElse:
                if s.teams >= 2 then 32 - Integer.numberOfLeadingZeros(s.teams - 1) else 0
        val plan = cupRoundDates(s, nb) ++ dates.filter(_._1 < nb)
        JsonOk:
          Json.obj(
            "title" -> s.title,
            "url" -> routes.Team.show(TeamId("kubok")).url,
            "minutes" -> s.minutes,
            "rounds" -> plan.toList
              .sortBy(_._1)
              .flatMap: (r, d) =>
                cupMsk(d).map: at =>
                  Json.obj(
                    "round" -> (r + 1),
                    "name" -> TeamModel.cupRoundName(r, nb),
                    "startsAt" -> at.toEpochMilli
                  )
          )
    else if id.value == "kubok-history" then // «Логик»: прошедшие кубки
      api.cupArchiveList.flatMap(archives => Ok.page(views.team.list.cupHistory(archives)))
    else if id.value == "kubok" then // «Логик»: доска кубка + регламент из CMS-страницы kubok-rules
      for
        teams <- api.cupBoard
        rules <- env.cms.renderKey("kubok-rules")
        canEdit = lila.core.perm.Granter.opt(_.Pages) || lila.core.perm.Granter.opt(_.ManageTeam)
        shown = rules.filter(r => r.live || canEdit)
        // регламент админ правит прямо на доске обычным текстом: форма CMS на телефоне не заполнялась
        editUrl <- lila.core.perm.Granter
          .opt(_.ManageTeam)
          .so(cupRulesPages.map(_.headOption.fold("")(_.markdown.value).some))
        bracket <- cupBracketState
        dates <- api.cupBracketDates
        settings <- api.cupSettings
        pult <- lila.core.perm.Granter.opt(_.ManageTeam).so(cupPult(settings, bracket).map(_.some))
        live <- cupLiveCache.get({})
        page <- renderPage(
          views.team.list.cup(
            settings,
            teams,
            shown.map(_.html),
            editUrl,
            // документ сетки бывает только с датами (их сохранили до жеребьёвки) — это ещё не сетка
            bracket
              .filter(_._1.nonEmpty)
              .map((rounds, tours) => (rounds, tours.view.mapValues(cupTourView).toMap)),
            dates.view.mapValues(d => cupMsk(d)).collect { case (k, Some(v)) => k -> v }.toMap,
            dates,
            pult,
            live
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

  // «Логик»: опоздавшие — без пересборки составов (l = отмеченные ученики).
  private val cupLateForm =
    import play.api.data.Forms.*
    Form(tuple("late" -> default(boolean, false), "perf" -> default(text, "rapid"), "l" -> list(text)))

  private def cupLatePost(perfKey: String, ls: List[String])(using ctx: BodyContext[?], me: Me): Fu[Result] =
    val perf = cupPerf(perfKey)
    for
      school <- env.team.memberRepo.userIdsByTeam(TeamId("logic-all")).map(_.toSet)
      inCup <- api.cupMemberIds
      wanted = ls.flatMap(UserStr.read).map(_.id).filter(u => school(u) && !inCup(u)).distinct
      players <- cupPlayers(wanted, perf).map(_.filterNot(_._3))
      board <- api.cupBoard
      ratings <- cupPlayers(board.flatMap(_._2), perf).map(_.map(p => p._1 -> p._2).toMap)
      // команды без игроков (лишние после пересборки) опоздавших не принимают
      teams = board.collect:
        case (t, members, _) if members.nonEmpty => t.id -> members.map(u => ratings.getOrElse(u, 1000))
      // 22.11 остаток уходил в новые команды из одного человека: теперь новых команд нет
      (into, rest) = lila.team.Cup.placeLate(teams, players.map(p => (p._1, p._2)), TeamModel.cupMaxMembers)
      res <- api.addLateCupPlayers(into, Nil)
    yield
      val added = res._1
      val back = Redirect(routes.Team.edit(TeamId("kubok")))
      val noRoom =
        if rest.isEmpty then ""
        else s" Не хватило мест: ${rest.size}. Создайте для них команду кнопкой «Новая команда»."
      if added == 0 && rest.isEmpty then
        back.flashFailure("Никого не добавлено: отметьте учеников, которых нет в командах.")
      else if added == 0 then back.flashFailure("Никого не добавлено." + noRoom)
      else back.flashSuccess(s"Добавлено в команды: $added." + noRoom)

  // «Логик»: ручная перестановка ученика (to: id команды, "new" — новая команда, "none" — убрать).
  private val cupMoveForm =
    import play.api.data.Forms.*
    Form(tuple("move" -> default(boolean, false), "u" -> default(text, ""), "to" -> default(text, "")))

  private def cupMovePost(uStr: String, toStr: String)(using ctx: BodyContext[?], me: Me): Fu[Result] =
    val back = Redirect(routes.Team.edit(TeamId("kubok")))
    val to: Option[Option[TeamId]] = toStr match
      case "none" => none
      case "new" => some(none)
      case id => some(TeamId(id).some)
    UserStr.read(uStr).map(_.id) match
      case None => fuccess(back.flashFailure("Выберите ученика"))
      case Some(u) =>
        for
          school <- env.team.memberRepo.userIdsByTeam(TeamId("logic-all"))
          res <-
            if !school.contains(u) then fuccess(Left("Ученика нет в клубе «ШК Логик»"))
            else if toStr.isEmpty then fuccess(Left("Выберите, куда поставить ученика"))
            else api.cupMove(u, to)
          battles <- res.toOption.flatten.filter(_._2).so((t, _) => cupAddToBattles(List(t.id)))
          name <- env.user.lightUserApi.async(u).map(_.fold(u.value)(_.name.value))
        yield res match
          case Left(err) => back.flashFailure(err)
          case Right(None) => back.flashSuccess(s"$name убран(а) из команды")
          case Right(Some((t, false))) => back.flashSuccess(s"$name теперь в команде «${t.name}»")
          case Right(Some((t, true))) =>
            back.flashSuccess(s"$name теперь в новой команде «${t.name}». ${cupBattlesNote(battles)}")

  // «Логик»: новые команды сразу встают в ещё не закончившиеся общие бои кубка.
  // В сетку плей-офф и в бои пар их не ставим: там пары уже определены.
  private def cupAddToBattles(newTeams: List[TeamId]): Fu[Int] =
    if newTeams.isEmpty then fuccess(0)
    else
      import reactivemongo.api.bson.BSONDocument
      for
        ids <- env.tournament.tournamentRepo.coll.distinctEasy[String, List](
          "_id",
          BSONDocument(
            "name" -> BSONDocument("$regex" -> "^Кубок Логика - общий бой"),
            "status" -> BSONDocument("$lt" -> 30),
            "teamBattle" -> BSONDocument("$exists" -> true)
          )
        )
        tours <- ids.sequentially(id => env.tournament.tournamentRepo.byId(TourId(id)))
        done <- tours.flatten.sequentially: tour =>
          tour.teamBattle.so: battle =>
            val all = (battle.teams.toList.map(_.value) ++ newTeams.map(_.value)).distinct
            for
              _ <- env.tournament.api.teamBattleUpdate(
                tour,
                lila.tournament.TeamBattle.DataForm.Setup(all.mkString("", "\n", "\n"), battle.nbLeaders),
                env.team.api.filterExistingIdsNoClas
              )
              _ <- cupShowInTeams(tour.id, all)
            yield 1
      yield done.sum

  private def cupBattlesNote(battles: Int) =
    (if battles > 0 then s"Новые команды добавлены в общий бой кубка ($battles). "
     else "Общего боя, который ещё не закончился, нет. ") +
      "В сетку плей-офф новые команды не попадают: для этого проведите жеребьёвку заново."

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
        rsvp <- api.cupRsvp
        run <- api.cupRun
        yes = (u: UserId) => rsvp.get(u).contains(true)
        page <- Ok.page(
          views.team.list.cupDraft(
            perf.value,
            size,
            teams,
            board.size,
            board.map((t, ms, _) => (t.id, t.name, ms.size)),
            onlineOnly,
            players.sortBy(p => (!yes(p._1), !isOn(p._1), -p._2)),
            current,
            isOn,
            picked.toSet,
            preview,
            rsvp,
            run.locked
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
          wins <- api.cupBracketWins
          voids <- api.cupBracketVoids
          found <- tourIds.toList
            .filterNot((_, id) => id == "-" || voids(id))
            .sequentially: (k, id) =>
              env.tournament.tournamentRepo.byId(TourId(id)).map(k -> _)
          tours = found.collect { case (k, Some(t)) => k -> t }.toMap
          winners <- tours.toList.sequentially: (k, t) =>
            if !t.isFinished then fuccess(k -> none)
            else
              env.tournament.cached.battle.teamStanding
                .get(t.id)
                .map(rs => k -> rs.headOption.filter(_.score > 0).map(_.teamId))
          // присуждённая администратором победа важнее результата боя
          won = winners.toMap ++ wins.view.mapValues(_.some)
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
        "leaders" -> default(number(min = 1, max = 5), 5),
        "rated" -> boolean
      )
    )

  private def cupMsk(d: String): Option[java.time.Instant] =
    scala.util
      .Try(java.time.LocalDateTime.parse(d).atZone(java.time.ZoneId.of("Europe/Moscow")).toInstant)
      .toOption

  // NFKC: «𝕊𝕙𝕒𝕕𝕠𝕨» → «Shadow». Такие буквы — пары суррогатов, посимвольный фильтр их выкидывал,
  // и 22.11 бой назывался «- победители».
  private def cupName(raw: String) =
    java.text.Normalizer
      .normalize(raw, java.text.Normalizer.Form.NFKC)
      .filter(c => c.isLetterOrDigit || c.isWhitespace || ":.,;'+-".contains(c))
      .trim
      .replaceAll("\\s+", " ")

  // имя команды для названия боя: из одних значков получается пусто — тогда «Команда N»
  private def cupTeamTitle(id: TeamId, name: String) =
    cupName(name) match
      case "" => s"Команда ${id.value.stripPrefix(TeamModel.cupIdPrefix)}"
      case n => n

  // «Логик»: бой кубка виден на странице каждой своей команды и клуба «ШК Логик».
  // createTournament ставит forTeams только первой команде, остальные туда не попадали.
  private def cupShowInTeams(tourId: TourId, teamIds: Iterable[String]): Funit =
    import reactivemongo.api.bson.{ BSONArray, BSONDocument, BSONString }
    // BSONArray(List[String]) кладёт список одним вложенным элементом: нужны BSONString
    env.tournament.tournamentRepo.coll.update
      .one(
        BSONDocument("_id" -> tourId.value),
        BSONDocument(
          "$addToSet" -> BSONDocument(
            "forTeams" -> BSONDocument(
              "$each" -> BSONArray((teamIds.toList :+ "logic-all").distinct.map(BSONString(_)))
            )
          )
        )
      )
      .void

  private def cupLeadersText(n: Int) = n match
    case 1 => "В зачёт идёт лучший результат команды."
    case 5 => "В зачёт идут результаты всех игроков команды."
    case _ =>
      val w = Map(2 -> "двух", 3 -> "трёх", 4 -> "четырёх").getOrElse(n, n.toString)
      s"В зачёт идут результаты $w лучших игроков команды."

  // «Логик»: посев по среднему рейтингу команды и жеребьёвка. Возвращает число команд в сетке.
  private def cupDrawSeeded(perfKey: String): Fu[Int] =
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
      _ <- (seeded.sizeIs >= 2).so(api.cupBracketDraw(TeamModel.cupSlots(seeded)))
    yield seeded.size

  // «Логик»: бои пар сетки — по «Командному бою» на пару; старт из startOf или через 5 минут
  private def cupCreateBattles(
      todo: List[TeamModel.CupMatch],
      nbRounds: Int,
      startOf: TeamModel.CupMatch => Option[java.time.Instant],
      title: String,
      clock: String,
      inc: Int,
      minutes: Int,
      leaders: Int,
      rated: Boolean,
      ignore: Set[String] = Set.empty // бой, который сейчас пересоздаётся, команду не занимает
  )(using Me): Fu[List[Either[String, TourId]]] =
    for
      teams <- api.cupBoard
      names = teams.map((t, _, _) => t.id -> t.name).toMap
      made <- todo.sequentially: m =>
        val (x, y) = m.pair.get
        val roundName = TeamModel.cupRoundName(m.round, nbRounds)
        val (nx, ny) = (names.getOrElse(x, x.value), names.getOrElse(y, y.value))
        val full = cupName(s"${cupTeamTitle(x, nx)} - ${cupTeamTitle(y, ny)}")
        val leaderTeams = List(lila.core.team.LightTeam(x, nx, none))
        val data = Map(
          // длинные имена команд обрезаются, чтобы в названии остались обе команды
          "name" -> (if full.sizeIs <= 30 then full
                     else
                       cupName(
                         s"${cupTeamTitle(x, nx).take(13).trim} - ${cupTeamTitle(y, ny).take(13).trim}"
                       )),
          "clockTime" -> clock,
          "clockIncrement" -> inc.toString,
          "minutes" -> minutes.toString,
          "rated" -> rated.toString,
          "teamBattleByTeam" -> x.value,
          "berserkable" -> "false", // Кубок без берсерка (Антон, 16.11)
          "streakable" -> "true",
          "hasChat" -> "true",
          "description" -> s"$title, $roundName: $nx против $ny. ${cupLeadersText(leaders)}"
        ) ++ startOf(m).fold(Map("waitMinutes" -> "5"))(s => Map("startDate" -> s.toEpochMilli.toString))
        val start = startOf(m).getOrElse(java.time.Instant.now.plusSeconds(300))
        cupBusyTeam(List(x, y), start, minutes, ignore).flatMap:
          case Some(busy) =>
            val nb = names.getOrElse(busy, busy.value)
            fuccess(Left(s"$nx - $ny: команда «$nb» в это время уже играет в другом бою"))
          case None =>
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
                    _ <- cupShowInTeams(tour.id, List(x.value, y.value))
                    _ <- api.cupBracketSetTour(m.key, tour.id.value)
                  yield Right(tour.id)
              )
    yield made

  // «Логик»: команда уже стоит в другом бою (не начался или идёт), который пересекается
  // по времени с [start, start + minutes]. 22.11 две команды сыграли по два боя сразу.
  private def cupBusyTeam(
      teams: List[TeamId],
      start: java.time.Instant,
      minutes: Int,
      ignore: Set[String] = Set.empty
  ): Fu[Option[TeamId]] =
    import reactivemongo.api.bson.{ BSONArray, BSONDateTime, BSONDocument, BSONString }
    import lila.db.dsl.list
    import lila.core.tournament.Status as TourStatus
    val end = start.plusSeconds(minutes * 60L)
    env.tournament.tournamentRepo.coll
      .list[BSONDocument](
        BSONDocument(
          "status" -> BSONDocument("$in" -> BSONArray(TourStatus.created.id, TourStatus.started.id)),
          "teamBattle.teams" -> BSONDocument("$in" -> BSONArray(teams.map(t => BSONString(t.value)))),
          "startsAt" -> BSONDocument("$lt" -> BSONDateTime(end.toEpochMilli))
        ),
        50
      )
      .map: docs =>
        val overlapping = docs.filter: d =>
          !d.getAsOpt[String]("_id").exists(ignore) && {
            val from = d.getAsOpt[BSONDateTime]("startsAt").fold(0L)(_.value)
            from + d.getAsOpt[Int]("minutes").getOrElse(0) * 60_000L > start.toEpochMilli
          }
        val playing = overlapping
          .flatMap(_.getAsOpt[BSONDocument]("teamBattle"))
          .flatMap(_.getAsOpt[List[String]]("teams"))
          .flatten
          .toSet
        teams.find(t => playing(t.value))

  // «Логик»: защита сетки перед жеребьёвкой и сбросом. Пока идёт бой сетки, оба действия
  // запрещены: бой оторвётся от сетки (22.11 пересборка в 18:04 так и вышла). Жеребьёвка
  // поверх сыгранных пар тоже запрещена: сначала «Сбросить сетку». Не начавшиеся бои
  // сетки снимаются, иначе они стартуют сами со старыми парами. Right — сколько боёв снято.
  private def cupBracketGuard(draw: Boolean): Fu[Either[String, Int]] =
    cupBracketState.flatMap:
      case None => fuccess(Right(0))
      case Some((rounds, tours)) =>
        val live = tours.values.count(t => t.isStarted && !t.isFinished)
        val played = rounds.flatten.exists(m => m.pair.isDefined && m.winner.isDefined)
        val what = if draw then "Новую жеребьёвку можно провести" else "Сбросить сетку можно"
        if live > 0 then fuccess(Left(s"Сейчас идут бои сетки: $live. $what, когда они закончатся."))
        else if draw && played then
          fuccess(
            Left(
              "В сетке уже есть сыгранные пары, и новая жеребьёвка их сотрёт. " +
                "Чтобы начать заново, сначала нажмите «Сбросить сетку»."
            )
          )
        else
          val pending = tours.values.filter(_.isCreated).toList
          pending.sequentiallyVoid(env.tournament.api.kill).inject(Right(pending.size))

  private def cupKilledText(n: Int, again: Boolean = true) =
    if n == 0 then ""
    else
      s". Отменены бои, которые ещё не начались: $n" + (if again then ". Бои создаст старт круга." else ".")

  // бои сетки, которые ещё не начались, снимаются (перед пересборкой и новым кубком)
  private def cupKillPending: Funit =
    api.cupBracketGet.flatMap:
      _.so: (_, tours) =>
        tours.values.toList.sequentiallyVoid: id =>
          env.tournament.tournamentRepo
            .byId(TourId(id))
            .flatMap(_.filter(_.isCreated).so(env.tournament.api.kill))

  private val cupZone = java.time.ZoneId.of("Europe/Moscow")

  // даты кругов: старт первого + круг × everyDays, московское время как в datetime-local
  private def cupRoundDates(s: TeamModel.CupSettings, nbRounds: Int): Map[Int, String] =
    s.start
      .flatMap(d => scala.util.Try(java.time.LocalDateTime.parse(d)).toOption)
      .fold(Map.empty[Int, String]): first =>
        (0 until nbRounds).map(r => r -> first.plusDays(r.toLong * s.everyDays).toString).toMap

  private def cupInt(data: Map[String, String], k: String, v: Int, lo: Int, hi: Int) =
    data.get(k).flatMap(_.trim.toIntOption).getOrElse(v).atLeast(lo).atMost(hi)

  private val cupClocks = List("1", "2", "3", "4", "5", "6", "7", "8", "10", "15", "20", "25", "30")

  private def cupSettingsPost(using ctx: BodyContext[?], me: Me): Fu[String] =
    val data = cupBracketForm.bindFromRequest().data
    for
      old <- api.cupSettings
      size = cupInt(data, "size", old.size, 0, TeamModel.cupMaxMembers)
      // дата и время первого круга — отдельными полями (datetime-local без времени Chrome шлёт пустым);
      // пустая или кривая дата не стирает прежний старт
      start = data.get("startDate").map(_.trim).filter(_.nonEmpty) match
        case Some(d) =>
          val t = data.get("startTime").map(_.trim).filter(_.nonEmpty).getOrElse("18:00")
          Some(s"${d}T${t.take(5)}").filter(cupMsk(_).isDefined).orElse(old.start)
        case None => data.get("start").map(_.trim).filter(cupMsk(_).isDefined).orElse(old.start)
      hasBracket <- api.cupBracketGet.map(_.exists(_._1.exists(_.isDefined)))
      s = old.copy(
        name = data.get("name").map(cupName).filter(_.nonEmpty).fold(old.name)(_.take(40)),
        start = start,
        everyDays = cupInt(data, "everyDays", old.everyDays, 1, 60),
        connectMin = cupInt(data, "connectMin", old.connectMin, 0, 240),
        formMin = cupInt(data, "formMin", old.formMin, 1, 180),
        auto = data.get("auto").contains("true"),
        size = if size == 1 then 2 else size,
        teams = cupInt(data, "teams", old.teams, 0, 64),
        perf = cupPerf(data.getOrElse("perf", old.perf)).value,
        clock = data.get("clock").filter(cupClocks.contains).getOrElse(old.clock),
        inc = cupInt(data, "inc", old.inc, 0, 60),
        minutes = cupInt(data, "minutes", old.minutes, 10, 360),
        leaders = cupInt(data, "leaders", old.leaders, 1, 5),
        rated = data.get("rated").contains("true"),
        formed = old.formed || hasBracket,
        by = me.userId.value.some
      )
      _ <- api.cupSettingsSet(s)
      saved <- api.cupBracketDates
      // старт перенесли — плановые даты кругов сдвигаются вслед; до жеребьёвки пересчитываем
      // только уже сохранённые даты, иначе первый круг разошёлся бы со стартом автосбора
      changed = s.start.isDefined && (s.start != old.start || s.everyDays != old.everyDays)
      moved = changed && (s.formed || saved.nonEmpty)
      _ <- moved.so:
        cupBracketState.flatMap: st =>
          val nb = st.map(_._1.size).filter(_ > 0).getOrElse(saved.keys.maxOption.fold(0)(_ + 1))
          api.cupBracketSetDates(cupRoundDates(s, nb))
    yield "Настройки кубка сохранены" + (if moved then ", даты кругов пересчитаны" else "")

  // все бои кубка, которые ещё не начались: сетка и отдельные «общие бои»
  private def cupKillAllPending: Funit =
    import reactivemongo.api.bson.BSONDocument
    env.tournament.tournamentRepo.coll
      .distinctEasy[String, List](
        "_id",
        BSONDocument(
          "teamBattle.teams" -> BSONDocument("$regex" -> s"^${TeamModel.cupIdPrefix}"),
          "status" -> lila.core.tournament.Status.created.id
        )
      )
      .flatMap:
        _.sequentiallyVoid: id =>
          env.tournament.tournamentRepo
            .byId(TourId(id))
            .flatMap(_.filter(_.isCreated).so(env.tournament.api.kill))

  private def cupRulesPages = env.cms.api.getAlternatives(lila.core.id.CmsPageKey("kubok-rules"))
  // «Логик»: регламент кубка — одна CMS-страница kubok-rules на языке по умолчанию (видна всем),
  // сразу опубликована; пустой текст убирает регламент с доски
  private def cupRulesPost(using ctx: BodyContext[?], me: Me): Fu[String] =
    val text = cupBracketForm
      .bindFromRequest()
      .data
      .get("rules")
      .fold("")(_.replace("\r\n", "\n").replace('\r', '\n').trim)
      .take(20000)
    for
      old <- cupRulesPages
      _ <- old.sequentiallyVoid(p => env.cms.api.delete(p.id))
      _ <- text.nonEmpty.so:
        env.cms.api.create(
          lila.cms.CmsPage(
            id = lila.core.id.CmsPageId(scalalib.ThreadLocalRandom.nextString(6)),
            key = lila.core.id.CmsPageKey("kubok-rules"),
            title = "Регламент турнира",
            markdown = Markdown(text),
            language = lila.core.i18n.defaultLanguage,
            live = true,
            canonicalPath = None,
            by = me.userId,
            at = nowInstant
          )
        )
    yield if text.isEmpty then "Регламент убран с доски" else "Регламент сохранён, его видят все"
  // «Логик»: закрыть текущий кубок в историю и начать следующий с теми же настройками.
  // Подпись в истории и номер нового кубка задаёт администратор; команды можно оставить.
  private def cupNewCup(using ctx: BodyContext[?], me: Me): Fu[String] =
    import reactivemongo.api.bson.BSONDocument
    val data = cupBracketForm.bindFromRequest().data
    val keep = data.get("keep").contains("true")
    for
      s <- api.cupSettings
      label = data.get("label").map(_.trim.take(80)).filter(l => l.nonEmpty && l != s.title)
      next = cupInt(data, "next", s.n + 1, 1, 999)
      rules <- env.cms.renderKey("kubok-rules")
      board <- api.cupBoard
      bracket <- api.cupBracketGet
      state <- cupBracketState
      dates <- api.cupBracketDates
      voids <- api.cupBracketVoids
      _ <- cupKillAllPending
      ids <- env.tournament.tournamentRepo.coll.distinctEasy[String, List](
        "_id",
        BSONDocument(
          "teamBattle.teams" -> BSONDocument("$regex" -> s"^${TeamModel.cupIdPrefix}"),
          "status" -> lila.core.tournament.Status.finished.id
        ) ++ s.since.fold(BSONDocument())(i => BSONDocument("createdAt" -> BSONDocument("$gte" -> i)))
      )
      // отменённые бои (остановленный круг, пересозданная пара) в историю не идут
      battles <- ids
        .filterNot(voids)
        .sequentially: id =>
          env.tournament.tournamentRepo
            .byId(TourId(id))
            .flatMap:
              _.so: t =>
                env.tournament.cached.battle.teamStanding
                  .get(t.id)
                  .map: rs =>
                    TeamModel
                      .CupArchiveBattle(
                        t.id.value,
                        t.name,
                        t.startsAt,
                        t.isFinished,
                        rs.map(r => r.teamId.value -> r.score)
                      )
                      .some
      teams = board.collect:
        case (t, ms, leaders) if ms.nonEmpty =>
          TeamModel.CupArchiveTeam(t.id.value, t.name, ms.map(_.value), ms.find(leaders).map(_.value))
      winners = state.fold(Map.empty[String, TeamId]): (rounds, _) =>
        rounds.flatten.flatMap(m => m.winner.filter(_ => m.pair.isDefined).map(m.key -> _)).toMap
      archive = TeamModel.CupArchive(
        n = s.n,
        name = s.name,
        closedAt = java.time.Instant.now,
        dates = dates,
        teams = teams,
        slots = bracket.fold(Nil)(_._1),
        tours = bracket.fold(Map.empty)(_._2),
        winners = winners,
        battles = battles.flatten.sortBy(_.startsAt),
        label = label,
        rules = rules.map(_.html.value)
      )
      // бои этого кубка, уже лежащие в истории отдельной записью (тестовый тур),
      // склеиваются с ним в одну запись: без повторов боёв и с сохранённым регламентом
      past <- api.cupArchiveList
      ownIds = archive.battles.map(_.id).toSet
      overlap = past.filter(_.battles.exists(b => ownIds(b.id)))
      merged = overlap.foldLeft(archive): (a, o) =>
        a.copy(
          teams = if a.teams.nonEmpty then a.teams else o.teams,
          battles =
            (a.battles ++ o.battles.filterNot(b => a.battles.exists(_.id == b.id))).sortBy(_.startsAt),
          label = a.label.orElse(o.label),
          rules = a.rules.orElse(o.rules)
        )
      saved = merged.teams.nonEmpty || merged.slots.nonEmpty || merged.battles.nonEmpty
      _ <- saved.so(api.cupArchiveAdd(merged))
      _ <- (saved && overlap.nonEmpty).so(api.cupArchiveRemoveOthers(ownIds.toList, merged))
      // регламент уходит в историю вместе с кубком, новый кубок начинается с пустой формы
      _ <- rules.so(r => env.cms.api.delete(r.page.id))
      _ <- api.cupBracketReset
      _ <- api.cupRunReset
      _ <- (!keep).so(api.refillCupTeams(Nil).void)
      _ <- (!keep).so(api.cupRenameDefault)
      _ <- api.cupSettingsSet(
        s.copy(
          n = next,
          start = none,
          formed = false,
          since = java.time.Instant.now.some,
          by = me.userId.value.some
        )
      )
    yield (if saved then s"${merged.title} теперь в истории кубков. " else "") +
      s"Начат ${s.name} №$next. Укажите дату первого круга и напишите регламент" +
      (if keep then ", команды остались прежними" else "")

  // ───────────── «Логик»: тур кубка ─────────────
  // Тур идёт так: «Ждём» → «Составы открыты» (за formMin до старта; в первом круге площадка один раз
  // собирает команды) → старт круга за 2 минуты до боёв (составы закрываются, жеребьёвка, бои пар,
  // товарищеский бой команд без пары, составы записываются в бои) → «Бои идут» → «Круг сыгран».
  // Бои создаёт только старт круга. Тик площадки и все кнопки кубка идут через одну очередь:
  // двойное нажатие или нажатие в момент тика не создаёт второй бой (22.11 так появились дубли).
  private val cupQueue = scalalib.actor.AsyncActorSequencers[String](
    maxSize = Max(32),
    expiration = 1.hour,
    timeout = 3.minutes,
    name = "cupRunner",
    lila.mon.asyncActorMonitor.highCardinality
  )
  private def cupSeq[A <: Matchable](f: => Fu[A]): Fu[A] = cupQueue("kubok")(f)

  private val cupTickBusy = java.util.concurrent.atomic.AtomicBoolean(false)

  env.scheduler.scheduleWithFixedDelay(2.minutes, 20.seconds): () =>
    if cupTickBusy.compareAndSet(false, true) then
      cupSeq(cupTick)
        .recover { case e: Exception => lila.log("cup").warn(s"cup tick: ${e.getMessage}") }
        .onComplete(_ => cupTickBusy.set(false))

  private val cupBot = "площадка"

  private def cupBotMe(s: TeamModel.CupSettings): Fu[Option[Me]] =
    s.by.so(by => env.user.repo.me(UserId(by)))

  private def cupTick: Funit =
    for
      s <- api.cupSettings
      run <- api.cupRun
      me <- cupBotMe(s)
      _ <- me.so: m =>
        given Me = m
        run.round match
          case Some(r) if run.started => cupFinishCheck(r)
          // старт круга оборвался (рестарт площадки): доделываем, повтор ничего не задвоит
          case Some(r) => cupStartRound(r, none, cupBot).void
          case None => (s.auto && s.start.isDefined).so(cupAutoNext(s, run))
    yield ()

  // число кругов по сетке (слотов 16 → 4 круга), 0 — сетки нет
  private def cupNbRounds(slots: List[Option[TeamId]]): Int =
    if slots.isEmpty then 0 else 31 - Integer.numberOfLeadingZeros(slots.size)

  // плановые даты кругов: сохранённые в сетке, иначе от старта кубка; до жеребьёвки — по сохранённым
  // датам или один первый круг
  private def cupPlan(s: TeamModel.CupSettings): Fu[Map[Int, java.time.Instant]] =
    for
      saved <- api.cupBracketDates
      slots <- api.cupBracketGet.map(_.fold(Nil)(_._1))
    yield
      val nb = cupNbRounds(slots)
      val raw =
        if nb > 0 then (cupRoundDates(s, nb) ++ saved).filter(_._1 < nb)
        else if saved.nonEmpty then saved
        else cupRoundDates(s, 1)
      raw.flatMap((r, d) => cupMsk(d).map(r -> _))

  private type CupState = Option[(List[List[TeamModel.CupMatch]], Map[String, lila.tournament.Tournament])]

  private def cupUndecided(st: CupState, r: Int): List[TeamModel.CupMatch] =
    st.fold(Nil)(_._1.lift(r).getOrElse(Nil).filter(m => m.pair.isDefined && m.winner.isEmpty))

  // запись в журнал без повтора: тик раз в 20 секунд не должен засорять журнал одним и тем же
  private def cupLogOnce(by: String, text: String): Funit =
    api.cupLogList.flatMap: log =>
      (!log.headOption.exists(_._3 == text)).so(api.cupLog(by, text))

  private def cupRoundTitle(r: Int, nb: Int) =
    if nb > 0 then TeamModel.cupRoundName(r, nb) else "Первый круг"

  private def cupAutoNext(s: TeamModel.CupSettings, run: lila.team.Cup.Run)(using Me): Funit =
    val now = java.time.Instant.now
    for
      dates <- cupPlan(s)
      st <- cupBracketState
      _ <- lila.team.Cup
        .nextRound(dates, run)
        .so: r =>
          val at = run.startOf(r, dates(r))
          val nb = st.fold(0)(_._1.size)
          if r > 0 && cupUndecided(st, r - 1).nonEmpty then
            // нет победителя в паре прошлого круга: ждём решения администратора, пульт это показывает
            funit
          else if lila.team.Cup.missed(now, at) then
            cupLogOnce(
              cupBot,
              s"${cupRoundTitle(r, nb)}: время старта прошло, сама не запускаю. Нажмите «Запустить сейчас» или «Отложить»."
            )
          else
            val formDue = r == 0 && !s.formed && !now.isBefore(at.minusSeconds(s.formMin * 60L))
            for
              _ <- formDue.so(cupFormTeams(s))
              _ <- lila.team.Cup.dueToStart(now, at).so(cupStartRound(r, at.some, cupBot).void)
            yield ()
    yield ()

  // сбор команд перед первым кругом: один раз, только если готовых команд меньше двух.
  // Жеребьёвки и боёв здесь нет — их делает старт круга, после правки составов администратором.
  private def cupFormTeams(s: TeamModel.CupSettings)(using Me): Funit =
    api.cupBoard.flatMap: board =>
      val ready = board.count(_._2.nonEmpty)
      if ready >= 2 then
        for
          _ <- api.cupSetFormed(true)
          _ <- api.cupLog(
            cupBot,
            s"Составы открыты: готовых команд $ready, новых не собираю. До старта их можно править."
          )
        yield ()
      else
        for
          school <- env.team.memberRepo.userIdsByTeam(TeamId("logic-all"))
          players <- cupPlayers(school.filter(env.socket.isOnline.exec), cupPerf(s.perf))
          kids = players.filterNot(_._3)
          draft =
            if s.size == 0 then TeamModel.cupDraftByGroups(kids.map(p => (p._1, p._2, p._4)), s.teams)
            else if s.teams > 0 then TeamModel.cupDraftInto(kids.map(p => (p._1, p._2)), s.teams)
            else TeamModel.cupDraft(kids.map(p => (p._1, p._2)), s.size)
          rosters = draft.map(_.map(_._1)).filter(_.nonEmpty)
          _ <-
            if rosters.sizeIs < 2 then
              cupLogOnce(
                cupBot,
                s"Не из кого собрать команды: учеников на площадке ${kids.size}. Пробую снова каждые 20 секунд."
              )
            else
              for
                _ <- api.refillCupTeams(rosters)
                _ <- api.cupSetFormed(true)
                _ <- api.cupLog(
                  cupBot,
                  s"Составы собраны: команд ${rosters.size}, игроков ${rosters.map(_.size).sum}. До старта их можно править."
                )
              yield ()
        yield ()

  // Старт круга r. Идемпотентен: повторный вызов доделывает недостающее и ничего не задваивает.
  // Right — текст для журнала и администратора, Left — почему старт не состоялся.
  private def cupStartRound(r: Int, at: Option[java.time.Instant], by: String)(using
      Me
  ): Fu[Either[String, String]] =
    val now = java.time.Instant.now
    api.cupBoard
      .zip(cupBracketState)
      .flatMap: (board0, st0) =>
        val ready = board0.count(_._2.nonEmpty)
        if r == 0 && st0.isEmpty && ready < 2 then
          fuccess(Left(s"Для старта нужны хотя бы две команды с игроками, сейчас: $ready."))
        else
          api
            .cupRunLock(r)
            .flatMap: locked =>
              if !locked then fuccess(Left("Сейчас идёт другой круг."))
              else
                for
                  s <- api.cupSettings
                  board <- api.cupBoard
                  nonEmpty = board.filter(_._2.nonEmpty).map(_._1.id)
                  dups = lila.team.Cup.duplicates(board.map((t, ms, _) => t.id -> ms))
                  // жеребьёвка — только в первом круге и только если команды с игроками поменялись
                  drew <- (r == 0).so:
                    for
                      st <- cupBracketState
                      slots <- api.cupBracketGet.map(_.fold(Nil)(_._1))
                      hasResults = st.exists(_._1.flatten.exists(m => m.pair.isDefined && m.winner.isDefined))
                      need = lila.team.Cup.needsDraw(slots, nonEmpty.toSet, hasResults)
                      _ <- need.so(cupDrawSeeded(s.perf).void)
                      _ <- need.so:
                        for
                          st2 <- cupBracketState
                          saved <- api.cupBracketDates
                          nb = st2.fold(0)(_._1.size)
                          _ <- api.cupBracketSetDates(cupRoundDates(s, nb) ++ saved.filter(_._1 < nb))
                        yield ()
                    yield need
                  st <- cupBracketState
                  keys <- api.cupBracketGet.map(_.fold(Map.empty[String, String])(_._2))
                  rounds = st.fold(Nil)(_._1)
                  matches = rounds.lift(r).getOrElse(Nil)
                  pairs = matches
                    .flatMap(m => m.pair.filter(_ => m.winner.isEmpty).map((x, y) => (m.key, x, y)))
                  plan = lila.team.Cup.startPlan(pairs, nonEmpty)
                  start = lila.team.Cup.battleStart(now, at.getOrElse(now))
                  _ <- plan.walkovers.sequentiallyVoid((k, t) => api.cupBracketSetWin(k, t.some))
                  todo = matches.filter(m => plan.pairs.exists(_._1 == m.key) && !keys.contains(m.key))
                  made <- cupCreateBattles(
                    todo,
                    rounds.size,
                    _ => start.some,
                    s.name,
                    s.clock,
                    s.inc,
                    s.minutes,
                    s.leaders,
                    s.rated
                  )
                  friendlyTeams = board.collect { case (t, _, _) if plan.friendly.contains(t.id) => t }
                  friendly <-
                    if keys.contains(s"out$r") then fuccess(none)
                    else if friendlyTeams.sizeIs < 2 then api.cupBracketSetTour(s"out$r", "-").inject(none)
                    else
                      cupCreateMixedBattle(
                        friendlyTeams,
                        if r == 0 then "friendly" else "out",
                        start.some,
                        s.clock,
                        s.inc,
                        s.minutes,
                        s.leaders,
                        s.rated
                      )
                        .flatMap:
                          case Right(id) => api.cupBracketSetTour(s"out$r", id.value).inject(Right(id).some)
                          case Left(e) => fuccess(Left(e).some)
                  ids <- api.cupBracketGet.map(
                    _.fold(Nil)(_._2.toList.collect:
                      case (k, v) if lila.team.Cup.keyRound(k).contains(r) && v != "-" => v)
                  )
                  enrolled <- ids.sequentially(id => cupEnroll(TourId(id)))
                  _ <- api.cupRunStarted(r)
                  errs = made.collect { case Left(e) => e } ++ friendly
                    .flatMap(_.left.toOption)
                    .map(e => s"товарищеский бой: $e")
                  nb = rounds.size
                  text =
                    s"${cupRoundTitle(r, nb)}: составы закрыты" +
                      (if drew then ", проведена жеребьёвка" else "") +
                      s". Боёв пар: ${ids.size - friendly.count(
                          _.isRight
                        ) - (if keys.contains(s"out$r") && keys(s"out$r") != "-" then 1 else 0)}" +
                      (if friendly.exists(_.isRight) || keys.get(s"out$r").exists(_ != "-") then
                         ", есть товарищеский бой"
                       else "") +
                      (if plan.walkovers.nonEmpty then s", прошли без боя: ${plan.walkovers.size}" else "") +
                      s". В бои записано игроков: ${enrolled.sum}. Начало ${cupClock(start)}." +
                      (if dups.nonEmpty then s" Внимание: учеников в двух командах: ${dups.size}." else "") +
                      (if errs.nonEmpty then s" Не получилось: ${errs.mkString("; ")}" else "")
                  _ <- api.cupLog(by, text)
                yield Right(text)

  private val cupClockFmt =
    java.time.format.DateTimeFormatter.ofPattern("HH:mm").withZone(java.time.ZoneId.of("Europe/Moscow"))
  private def cupClock(i: java.time.Instant) = cupClockFmt.format(i)

  // Записать составы команд в бой: ребёнку остаётся открыть страницу боя. Уже записанных
  // пропускаем (повторная запись упала бы на уникальном индексе), ученик идёт только за одну команду.
  private def cupEnroll(id: TourId): Fu[Int] =
    env.tournament.tournamentRepo
      .byId(id)
      .flatMap:
        _.filterNot(_.isFinished)
          .flatMap(t => t.teamBattle.map(t -> _))
          .so: (tour, battle) =>
            for
              board <- api.cupBoard
              members = board.collect { case (t, ms, _) if battle.teams.contains(t.id) => t.id -> ms }
              already <- env.tournament.playerRepo
                .byTourAndUserIds(tour.id, members.flatMap(_._2))
                .map(_.map(_.userId).toSet)
              seen = scala.collection.mutable.Set.from(already)
              todo = members.map((t, ms) => t -> ms.filter(seen.add)).filter(_._2.nonEmpty)
              _ <- todo.sequentiallyVoid((t, ms) => env.tournament.api.joinManyNoChecks(tour.id, ms, t))
            yield todo.map(_._2.size).sum

  // все бои круга закончились (или удалены пустыми): составы снова открыты, круг сыгран
  private def cupFinishCheck(r: Int): Funit =
    for
      keys <- api.cupBracketGet.map(_.fold(Map.empty[String, String])(_._2))
      ids = keys.collect { case (k, v) if lila.team.Cup.keyRound(k).contains(r) && v != "-" => v }.toList
      found <- ids.sequentially(id => env.tournament.tournamentRepo.byId(TourId(id)))
      _ <- found
        .forall(_.forall(_.isFinished))
        .so:
          for
            _ <- api.cupRunUnlock(r, played = true)
            _ <- api.cupRunHold(r, none)
            st <- cupBracketState
            open = cupUndecided(st, r)
            names <- api.cupBoard.map(_.map((t, _, _) => t.id -> t.name).toMap)
            nb = st.fold(0)(_._1.size)
            _ <- api.cupLog(
              cupBot,
              s"${cupRoundTitle(r, nb)} сыгран, составы снова открыты." +
                (if open.isEmpty then ""
                 else
                   s" Нет победителя в парах: ${open.flatMap(_.pair).map((x, y) => s"${names.getOrElse(x, x.value)} — ${names.getOrElse(y, y.value)}").mkString(", ")}. " +
                     "Выберите в пульте «Пересоздать бой» или «Присудить победу».")
            )
          yield ()
    yield ()

  // снять бой с пары: ключ убирается из сетки до отмены, бой помечается аннулированным,
  // иначе отменённый идущий бой досчитался бы и дал паре победителя
  private def cupDropTour(key: String, id: String): Funit =
    for
      _ <- api.cupBracketUnsetTour(key)
      _ <- (id != "-").so(api.cupBracketVoid(id))
      t <- (id != "-").so(env.tournament.tournamentRepo.byId(TourId(id)))
      _ <- t.so(env.tournament.api.kill)
    yield ()

  private def cupLocked(run: lila.team.Cup.Run): Option[String] =
    run.round.map: _ =>
      "Круг идёт, составы закрыты. Чтобы изменить составы или сетку, нажмите на пульте «Остановить круг»."

  private def cupPostpone(using me: Me): Fu[Either[String, String]] =
    val now = java.time.Instant.now
    for
      s <- api.cupSettings
      run <- api.cupRun
      dates <- cupPlan(s)
      res <-
        if run.locked then
          fuccess(Left("Круг уже начался. Чтобы вернуться к составам, нажмите «Остановить круг»."))
        else
          lila.team.Cup.nextRound(dates, run) match
            case None => fuccess(Left("Нет даты следующего круга. Укажите её в настройках."))
            case Some(r) =>
              val base = run.startOf(r, dates(r))
              val at = (if base.isBefore(now) then now else base).plusSeconds(600)
              val text = s"Старт перенесён на ${cupClock(at)}"
              for
                _ <- api.cupRunHold(r, at.some)
                _ <- api.cupLog(me.username.value, text)
              yield Right(text)
    yield res

  private def cupStartNow(using me: Me): Fu[Either[String, String]] =
    val now = java.time.Instant.now
    for
      s <- api.cupSettings
      run <- api.cupRun
      dates <- cupPlan(s)
      st <- cupBracketState
      res <-
        if run.locked then fuccess(Left("Круг уже идёт."))
        else
          // дата первого круга не задана: стартуем «сейчас» как первый круг
          lila.team.Cup.nextRound(dates, run).orElse(Option.when(run.played.isEmpty)(0)) match
            case None => fuccess(Left("Все круги кубка сыграны."))
            case Some(r) if r > 0 && cupUndecided(st, r - 1).nonEmpty =>
              fuccess(
                Left(
                  "Сначала решите пары прошлого круга без победителя: «Пересоздать бой» или «Присудить победу»."
                )
              )
            case Some(r) =>
              for
                _ <- api.cupRunHold(r, now.some)
                res <- cupStartRound(r, now.some, me.username.value)
                _ <- res.isLeft.so(api.cupRunHold(r, none))
              yield res
    yield res

  private def cupStop(using me: Me): Fu[Either[String, String]] =
    api.cupRun.flatMap: run =>
      run.round match
        case None => fuccess(Left("Сейчас круг не идёт."))
        case Some(r) =>
          for
            keys <- api.cupBracketGet.map(_.fold(Map.empty[String, String])(_._2))
            mine = keys.filter((k, _) => lila.team.Cup.keyRound(k).contains(r)).toList
            _ <- mine.sequentiallyVoid(cupDropTour)
            wins <- api.cupBracketWins
            _ <- wins.keys
              .filter(k => lila.team.Cup.keyRound(k).contains(r))
              .toList
              .sequentiallyVoid(k => api.cupBracketSetWin(k, none))
            _ <- api.cupRunUnlock(r, played = false)
            _ <- api.cupRunHold(r, none)
            text =
              s"Круг остановлен, бои отменены (${mine.count(_._2 != "-")}) и не пойдут в зачёт. Составы открыты. " +
                "Когда всё готово, нажмите «Запустить сейчас»."
            _ <- api.cupLog(me.username.value, text)
          yield Right(text)

  // пересоздать бой одной пары (или товарищеский бой) текущего или только что сыгранного круга
  private def cupRecreate(key: String)(using me: Me): Fu[Either[String, String]] =
    val now = java.time.Instant.now
    lila.team.Cup.keyRound(key) match
      case None => fuccess(Left("Пара не найдена."))
      case Some(r) =>
        for
          s <- api.cupSettings
          run <- api.cupRun
          keys <- api.cupBracketGet.map(_.fold(Map.empty[String, String])(_._2))
          old = keys.get(key).filter(_ != "-")
          oldTour <- old.so(id => env.tournament.tournamentRepo.byId(TourId(id)))
          st <- cupBracketState
          res <-
            val ok = run.round.contains(r) && run.started || run.round.isEmpty && run.played(r)
            if !ok then fuccess(Left("Пересоздать можно бой круга, который идёт или только что закончился."))
            else
              val start = now.plusSeconds(lila.team.Cup.minLeadSec)
              val ignore = old.toSet
              for
                reopened <- run.round.isEmpty.so(api.cupRunReopen(r))
                _ <- api.cupBracketSetWin(key, none)
                _ <- old.so(cupDropTour(key, _))
                made <-
                  if key.startsWith("out") then
                    oldTour.flatMap(_.teamBattle).map(_.teams) match
                      case None => fuccess(Left("Товарищеский бой пересоздаётся только из прежнего боя."))
                      case Some(ids) =>
                        api.cupBoard.flatMap: board =>
                          val ts = board.collect { case (t, ms, _) if ids.contains(t.id) && ms.nonEmpty => t }
                          cupCreateMixedBattle(
                            ts,
                            if r == 0 then "friendly" else "out",
                            start.some,
                            s.clock,
                            s.inc,
                            s.minutes,
                            s.leaders,
                            s.rated
                          )
                            .flatMap:
                              case Right(id) => api.cupBracketSetTour(key, id.value).inject(Right(id))
                              case Left(e) => fuccess(Left(e))
                  else
                    st.fold(Nil)(_._1).flatten.find(_.key == key).filter(_.pair.isDefined) match
                      case None => fuccess(Left("В паре ещё нет обеих команд."))
                      case Some(m) =>
                        val nb = st.fold(0)(_._1.size)
                        cupCreateBattles(
                          List(m.copy(winner = none)),
                          nb,
                          _ => start.some,
                          s.name,
                          s.clock,
                          s.inc,
                          s.minutes,
                          s.leaders,
                          s.rated,
                          ignore
                        )
                          .map(_.headOption.getOrElse(Left("бой не создан")))
                enrolled <- made.toOption.so(cupEnroll)
                _ <- (made.isLeft && reopened).so(api.cupRunUnlock(r, played = true))
                text = made.fold(
                  e => s"Бой не пересоздан: $e",
                  id =>
                    s"Бой пересоздан, начало ${cupClock(start)}, записано игроков: $enrolled. Прежний бой отменён и не идёт в зачёт."
                )
                _ <- api.cupLog(me.username.value, text)
              yield made.left.map(_ => text).map(_ => text)
        yield res

  private def cupAward(key: String, team: TeamId)(using me: Me): Fu[Either[String, String]] =
    for
      keys <- api.cupBracketGet.map(_.fold(Map.empty[String, String])(_._2))
      st <- cupBracketState
      m = st.fold(Nil)(_._1).flatten.find(_.key == key)
      tour <- keys.get(key).filter(_ != "-").so(id => env.tournament.tournamentRepo.byId(TourId(id)))
      names <- api.cupBoard.map(_.map((t, _, _) => t.id -> t.name).toMap)
      res <-
        if !m.exists(_.pair.exists((x, y) => x == team || y == team)) then
          fuccess(Left("Команды нет в этой паре."))
        else if tour.exists(t => !t.isFinished) then
          fuccess(Left("Бой этой пары ещё идёт. Дождитесь конца или пересоздайте бой."))
        else
          val text = s"Победа в паре присуждена команде «${names.getOrElse(team, team.value)}»"
          for
            _ <- api.cupBracketSetWin(key, team.some)
            _ <- api.cupLog(me.username.value, text)
          yield Right(text)
    yield res

  // пульт тура для администратора
  private def cupPult(s: TeamModel.CupSettings, st: CupState): Fu[TeamModel.CupPult] =
    val now = java.time.Instant.now
    for
      run <- api.cupRun
      dates <- cupPlan(s)
      log <- api.cupLogList
      board <- api.cupBoard
      keys <- api.cupBracketGet.map(_.fold(Map.empty[String, String])(_._2))
    yield
      val phase = lila.team.Cup.phase(now, dates, run, s.formMin, cupUndecided(st, _).nonEmpty)
      val names = board.map((t, _, _) => t.id -> t.name).toMap
      val startAt = phase match
        case lila.team.Cup.Phase.Waiting(_, at) => at.some
        case lila.team.Cup.Phase.Rosters(_, at) => at.some
        case _ => none
      val dups = lila.team.Cup.duplicates(board.map((t, ms, _) => t.id -> ms))
      val current = run.round.orElse(run.played.maxOption)
      val warnings = List(
        (!s.auto).option("Автоматика выключена: круг начнётся только кнопкой «Запустить сейчас»."),
        s.by.isEmpty.option("Сохраните настройки кубка: без этого площадка не создаёт бои."),
        dups.nonEmpty.option(
          s"Учеников в двух командах: ${dups.size} (${dups.keys.map(_.value).mkString(", ")}). Поставьте каждого в одну команду на странице «Составы»."
        ),
        cupUndecided(st, run.played.maxOption.getOrElse(-1)).nonEmpty.option(
          "В сыгранном круге есть пары без победителя. Следующий круг не начнётся, пока вы не решите их."
        ),
        board
          .exists(_._2.sizeIs == 1)
          .option(
            s"Команды из одного игрока: ${board.filter(_._2.sizeIs == 1).map(_._1.name).mkString(", ")}."
          )
      ).flatten
      TeamModel
        .CupPult(
          phase = phase,
          nbRounds = st.fold(0)(_._1.size),
          auto = s.auto,
          closeAt = startAt.map(_.minusSeconds(lila.team.Cup.closeSec)),
          missed = startAt.exists(lila.team.Cup.missed(now, _)),
          warnings = warnings,
          undecided = run.played.maxOption.filter(_ => run.round.isEmpty).so(cupUndecided(st, _)),
          tours =
            keys.filter((k, v) => v != "-" && current.exists(c => lila.team.Cup.keyRound(k).contains(c))),
          log = log.take(50)
        )
        .pipe(p => p.copy(warnings = p.warnings ++ names.isEmpty.option("Команд кубка ещё нет.").toList))

  // бои текущего круга со счётом — для всех (кэш 5 секунд: доску смотрят много детей сразу)
  private val cupLiveCache = env.memo.cacheApi.unit[List[TeamModel.CupLive]]("cup.live"):
    _.expireAfterWrite(5.seconds).buildAsyncFuture: _ =>
      for
        run <- api.cupRun
        keys <- api.cupBracketGet.map(_.fold(Map.empty[String, String])(_._2))
        current = run.round.orElse(run.played.maxOption)
        mine = keys.toList.filter((k, v) =>
          v != "-" && current.exists(c => lila.team.Cup.keyRound(k).contains(c))
        )
        live <- mine.sequentially: (k, id) =>
          env.tournament.tournamentRepo
            .byId(TourId(id))
            .flatMap:
              _.so: t =>
                for
                  standing <- env.tournament.cached.battle.teamStanding.get(t.id)
                  playing <- t.isStarted.so(env.tournament.pairingRepo.playingUserIds(t.id).map(_.size / 2))
                yield TeamModel
                  .CupLive(
                    k,
                    t.id.value,
                    t.startsAt,
                    t.finishesAt,
                    t.isStarted || t.isFinished,
                    t.isFinished,
                    standing.map(rt => rt.teamId -> rt.score),
                    playing
                  )
                  .some
      yield live.flatten.sortBy(l => (l.key.startsWith("out"), l.key))

  private val cupOutName = "Кубок Логика - бой выбывших"

  // команды, проигравшие пару в сетке
  private def cupKnockedOut(rounds: List[List[TeamModel.CupMatch]]): Set[TeamId] =
    rounds.flatten
      .flatMap(m => m.pair.zip(m.winner))
      .flatMap { case ((x, y), w) => List(x, y).filter(_ != w) }
      .toSet

  // «Логик»: общий бой или бой выбывших — один «Командный бой» на несколько команд, сетку не трогает
  private def cupCreateMixedBattle(
      inPlay: List[TeamModel],
      kind: String, // "all" — общий бой, "out" — бой выбывших, "friendly" — товарищеский бой круга
      start: Option[java.time.Instant],
      clock: String,
      inc: Int,
      minutes: Int,
      leaders: Int,
      rated: Boolean
  )(using Me): Fu[Either[String, TourId]] =
    inPlay.headOption.filter(_ => inPlay.sizeIs >= 2) match
      case None => fuccess(Left("нужны хотя бы две команды с игроками"))
      case Some(first) =>
        val leaderTeams = List(lila.core.team.LightTeam(first.id, first.name, none))
        val data = Map(
          "name" -> (kind match
            case "out" => cupOutName
            // не длиннее 30 знаков: лимит 35 есть только у менеджера турниров
            case "friendly" => "Кубок Логика - товарищеский"
            case _ => "Кубок Логика - общий бой"),
          "clockTime" -> clock,
          "clockIncrement" -> inc.toString,
          "minutes" -> minutes.toString,
          "rated" -> rated.toString,
          "teamBattleByTeam" -> first.id.value,
          "berserkable" -> "false", // Кубок без берсерка (Антон, 16.11)
          "streakable" -> "true",
          "hasChat" -> "true",
          "description" -> (kind match
            case "out" =>
              s"Кубок Логика, бой выбывших: играют команды, которые выбыли из плей-офф (${inPlay.size}), соперник — из любой другой команды. На сетку кубка бой не влияет. ${cupLeadersText(leaders)}"
            case "friendly" =>
              s"Кубок Логика, товарищеский бой: играют команды, которым в этом круге не досталось пары (${inPlay.size}), соперник — из любой другой команды. На сетку кубка бой не влияет. ${cupLeadersText(leaders)}"
            case _ =>
              s"Кубок Логика, общий бой: играют все команды (${inPlay.size}), соперник — из любой другой команды. ${cupLeadersText(leaders)}")
        ) ++ start.fold(Map("waitMinutes" -> "5"))(s => Map("startDate" -> s.toEpochMilli.toString))
        env.tournament.forms
          .create(leaderTeams)
          .bind(data)
          .fold(
            err => fuccess(Left(err.errors.flatMap(_.messages).mkString(", "))),
            setup =>
              for
                tour <- env.tournament.api.createTournament(setup, leaderTeams, andJoin = false)
                _ <- env.tournament.api.teamBattleUpdate(
                  tour,
                  lila.tournament.TeamBattle.DataForm
                    .Setup(inPlay.map(_.id.value).mkString("", "\n", "\n"), leaders),
                  env.team.api.filterExistingIdsNoClas
                )
                _ <- cupShowInTeams(tour.id, inPlay.map(_.id.value))
              yield Right(tour.id)
          )

  // кнопки пульта тура; жеребьёвка, сброс сетки и новый кубок — только при открытых составах
  private def cupBracketPost(using ctx: BodyContext[?], me: Me): Fu[Result] =
    val back = Redirect(routes.Team.show(TeamId("kubok")))
    val data = cupBracketForm.bindFromRequest().data
    val reply = (r: Either[String, String]) => r.fold(back.flashFailure(_), back.flashSuccess(_))
    data.get("bracket") match
      case Some("postpone") => cupPostpone.map(reply)
      case Some("startnow") => cupStartNow.map(reply)
      case Some("stop") => cupStop.map(reply)
      case Some("recreate") => cupRecreate(data.getOrElse("key", "")).map(reply)
      case Some("award") =>
        cupAward(data.getOrElse("key", ""), TeamId(data.getOrElse("team", ""))).map(reply)
      case Some("round") =>
        fuccess(
          back.flashFailure("Бои создаёт старт круга. Чтобы начать круг раньше, нажмите «Запустить сейчас».")
        )
      case Some("draw" | "reset" | "newcup") =>
        api.cupRun.flatMap: run =>
          cupLocked(run).fold(cupBracketPostOther)(e => fuccess(back.flashFailure(e)))
      case _ => cupBracketPostOther

  private def cupBracketPostOther(using ctx: BodyContext[?], me: Me): Fu[Result] =
    val back = Redirect(routes.Team.show(TeamId("kubok")))
    cupBracketForm.bindFromRequest().value match
      case Some((Some("reset"), _, _, _, _, _, _, _)) =>
        cupBracketGuard(draw = false).flatMap:
          case Left(e) => fuccess(back.flashFailure(e))
          case Right(killed) =>
            api.cupBracketReset.inject(
              back.flashSuccess("Сетка сброшена" + cupKilledText(killed, again = false))
            )
      case Some((Some("draw"), perfKey, _, _, _, _, _, _)) =>
        cupBracketGuard(draw = true).flatMap:
          case Left(e) => fuccess(back.flashFailure(e))
          case Right(killed) =>
            cupDrawSeeded(perfKey).map: n =>
              if n < 2 then back.flashFailure("Для сетки нужны хотя бы две команды" + cupKilledText(killed))
              else back.flashSuccess(s"Жеребьёвка: команд $n" + cupKilledText(killed))
      case Some((Some("settings"), _, _, _, _, _, _, _)) =>
        cupSettingsPost.map(msg => back.flashSuccess(msg))
      case Some((Some("newcup"), _, _, _, _, _, _, _)) =>
        cupNewCup.map(msg => back.flashSuccess(msg))
      case Some((Some("rules"), _, _, _, _, _, _, _)) =>
        cupRulesPost.map(msg => back.flashSuccess(msg))
      case Some((Some("dates"), _, _, _, _, _, _, _)) =>
        val data = cupBracketForm.bindFromRequest().data
        val dates =
          (0 until 8).flatMap(r => data.get(s"d$r").map(_.trim).filter(cupMsk(_).isDefined).map(r -> _)).toMap
        for
          _ <- api.cupBracketSetDates(dates)
          s <- api.cupSettings
          // до сбора команд дата первого круга и есть старт кубка: автосбор идёт к ней
          d0 = dates.get(0).filter(d => !s.formed && !s.start.contains(d))
          _ <- d0.so(d => api.cupSettingsSet(s.copy(start = d.some)))
        yield back.flashSuccess(
          s"Даты кругов сохранены: ${dates.size}" + d0.fold("")(_ => ", старт кубка перенесён на первый круг")
        )
      // общая арена: все команды кубка с игроками в одном «Командном бою», сетку не трогает
      // who=out — утешительный бой: только команды, проигравшие пару в сетке; идёт параллельно кругу
      case Some((Some("all"), _, date, minutes, clock, inc, leaders, rated)) =>
        val start = date.flatMap(cupMsk).filter(_.isAfter(java.time.Instant.now.plusSeconds(60)))
        val outOnly = cupBracketForm.bindFromRequest().data.get("who").contains("out")
        val knockedOut: Fu[Set[TeamId]] =
          if !outOnly then fuccess(Set.empty)
          else cupBracketState.map(_.fold(Set.empty[TeamId])((rounds, _) => cupKnockedOut(rounds)))
        knockedOut
          .zip(api.cupBoard)
          .flatMap: (out, teams) =>
            val inPlay = teams.filter(_._2.nonEmpty).map((t, _, _) => t).filter(t => !outOnly || out(t.id))
            inPlay.headOption.filter(_ => inPlay.sizeIs >= 2) match
              case None =>
                fuccess(
                  back.flashFailure(
                    if outOnly then
                      s"Для боя выбывших нужны хотя бы две выбывшие команды с игроками, сейчас: ${inPlay.size}. Команда становится выбывшей, когда закончится бой её пары."
                    else "Для общего боя нужны хотя бы две команды с игроками"
                  )
                )
              case Some(_) =>
                cupCreateMixedBattle(
                  inPlay,
                  if outOnly then "out" else "all",
                  start,
                  clock,
                  inc,
                  minutes,
                  leaders,
                  rated
                ).map:
                  case Left(e) => back.flashFailure(s"Не получилось: $e")
                  case Right(id) =>
                    Redirect(s"/tournament/$id")
                      .flashSuccess(
                        s"${if outOnly then "Бой выбывших" else "Общий бой"} создан: команд ${inPlay.size}"
                      )
      case _ => fuccess(back)

  // все действия администратора кубка идут через ту же очередь, что и тик площадки
  private def cupDraftPost(using ctx: BodyContext[?], me: Me): Fu[Result] =
    if !isGranted(_.ManageTeam) then authorizationFailed
    else cupSeq(cupDraftPostSeq)

  // правка составов, пока идёт круг, запрещена: игрок остался бы в бою за прежнюю команду
  private def cupUnlocked(f: => Fu[Result]): Fu[Result] =
    api.cupRun.flatMap: run =>
      cupLocked(run).fold(f)(e => fuccess(Redirect(routes.Team.edit(TeamId("kubok"))).flashFailure(e)))

  private def cupDraftPostSeq(using ctx: BodyContext[?], me: Me): Fu[Result] =
    if cupBracketForm.bindFromRequest().value.exists(_._1.isDefined) then cupBracketPost
    else if cupMoveForm.bindFromRequest().value.exists(_._1) then
      cupMoveForm
        .bindFromRequest()
        .value
        .fold(fuccess(Redirect(routes.Team.edit(TeamId("kubok"))))): (_, u, to) =>
          cupUnlocked(cupMovePost(u, to))
    else if cupLateForm.bindFromRequest().value.exists(_._1) then
      cupLateForm
        .bindFromRequest()
        .value
        .fold(fuccess(Redirect(routes.Team.edit(TeamId("kubok"))))): (_, perfKey, ls) =>
          cupUnlocked(cupLatePost(perfKey, ls))
    else
      bindForm(cupDraftForm)(
        _ => Redirect(routes.Team.edit(TeamId("kubok"))),
        (perfKey, size, go, us, ts, teams, onlineOnly) =>
          env.team.memberRepo.userIdsByTeam(TeamId("logic-all")).flatMap { schoolList =>
            val school = schoolList.toSet
            def ids(xs: List[String]) = xs.flatMap(UserStr.read).map(_.id).filter(school).distinct
            if go then
              val rosters = ts.map(t => ids(t.split(',').toList)).filter(_.nonEmpty)
              cupUnlocked:
                api
                  .refillCupTeams(rosters)
                  .map: (filled, created) =>
                    Redirect(routes.Team.show(TeamId("kubok"))).flashSuccess(
                      s"Составы обновлены: команд ${filled + created}" +
                        (if created > 0 then s", из них новых $created" else "") +
                        ". Жеребьёвка пройдёт сама при старте первого круга."
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
                    case Requesting.OtherCup(name) => JsonBadRequest(cupOtherTeamText(name))
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
          Redirect(routes.Team.show(team.id)).flashFailure(cupClosedText)
        case Requesting.OtherCup(name) =>
          Redirect(routes.Team.show(team.id)).flashFailure(cupOtherTeamText(name))
        case _ => Redirect(routes.Team.show(team.id)).flashFailure("You cannot join this team.")

  private val cupClosedText =
    "Составы команд кубка набирает тренер. Чтобы попасть в команду или перейти в другую, напиши тренеру."

  private def cupOtherTeamText(name: String) =
    s"Ты уже в команде «$name». В кубке можно играть только за одну команду. Чтобы перейти в другую, напиши тренеру."

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
      if team.isCup && !isGranted(_.ManageTeam) then
        negotiate(
          html = Redirect(routes.Team.show(team.id)).flashFailure(cupClosedText),
          json = JsonBadRequest(cupClosedText)
        )
      else
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
