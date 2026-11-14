package controllers

import lila.app.{ *, given }

final class Dev(env: Env) extends LilaController(env):

  def settings = Secure(_.Settings) { _ ?=> _ ?=>
    Ok.page:
      views.dev.settings(settingsListForMe)
  }

  def settingsPost(id: String) = SecureBody(_.Settings) { _ ?=> me ?=>
    settingsListForMe.flatMap(_._2).find(_.id == id).so { setting =>
      bindForm(setting.form)(
        _ => BadRequest.page(views.dev.settings(settingsListForMe)),
        v =>
          lila.log.system.info(s"setting ${me.username} changes $id from ${setting.get()} to ${v.toString}")
          setting.setString(v.toString).inject(Redirect(routes.Dev.settings))
      )
    }
  }

  def logicColors = Secure(_.Settings) { ctx ?=> _ ?=>
    val s = env.web.settings
    if getBool("traps") && getBool("json") then JsonOk(logicTrapsJson(s.logicTraps.get()))
    else if getBool("marks") then logicMarks.flatMap(rows => Ok.page(views.dev.logicMarks(rows)))
    else if getBool("traps") then
      val text = s.logicTraps.get()
      val parsed = lila.web.LogicTraps.parse(text)
      logicTrapsCaught(parsed.traps).flatMap: caught =>
        Ok.page(views.dev.logicTraps(text, parsed, caught))
    else
      Ok.page:
        views.dev.logicColors(s.logicBg.get(), s.logicPrimary.get())
  }

  // Сбой «Базы дебютов» не должен ронять страницу: тогда списки попавшихся просто пустые.
  private def logicTrapsCaught(
      traps: List[lila.web.LogicTraps.Trap]
  ): Fu[List[List[lila.web.LogicTraps.Caught]]] =
    import java.net.http.{ HttpClient, HttpRequest, HttpResponse }
    import scala.jdk.FutureConverters.*
    if traps.isEmpty then fuccess(Nil)
    else
      val req = HttpRequest
        .newBuilder(java.net.URI.create(lila.web.LogicTraps.caughtUrl(traps)))
        .timeout(java.time.Duration.ofSeconds(10))
        .build()
      HttpClient
        .newHttpClient()
        .sendAsync(req, HttpResponse.BodyHandlers.ofString())
        .asScala
        .map(res => lila.web.LogicTraps.caughtFromJson(res.body))
        .recover { case e: Exception =>
          lila.log("logicTraps").warn(s"explorer traps: ${e.getMessage}")
          Nil
        }

  private val logicTrapsForm =
    import play.api.data.Forms.*
    play.api.data.Form(single("traps" -> text))

  // Список для редактора на доске анализа: номер строки нужен для удаления.
  private def logicTrapsJson(text: String) =
    play.api.libs.json.JsArray(
      logicTrapLines(text).zipWithIndex.flatMap: (raw, i) =>
        lila.web.LogicTraps
          .parseLine(raw)
          .toOption
          .map: t =>
            play.api.libs.json.Json
              .obj("index" -> i, "line" -> t.line, "hint" -> t.hint, "block" -> t.block, "uci" -> t.uci)
    )

  private def logicTrapLines(text: String) = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList

  private val logicTrapAddForm =
    import play.api.data.Forms.*
    play.api.data.Form(tuple("moves" -> nonEmptyText, "hint" -> text, "block" -> boolean))

  // Правка с доски анализа: op=add дописывает строку, op=del&i=N убирает строку N.
  private def logicTrapsApi(op: String)(using ctx: BodyContext[?], me: Me): Fu[play.api.mvc.Result] =
    val setting = env.web.settings.logicTraps
    val lines = logicTrapLines(setting.get())
    def save(next: List[String], what: String) =
      val text = next.mkString("\n")
      lila.log.system.info(s"setting ${me.username} $what logic trap, now ${next.size} lines")
      setting.set(text).inject(JsonOk(logicTrapsJson(text)))
    op match
      case "del" =>
        getInt("i").filter(i => i >= 0 && i < lines.size) match
          case Some(i) => save(lines.patch(i, Nil, 1), "removes")
          case None => JsonBadRequest("Нет такой ловушки").toFuccess
      case _ =>
        bindForm(logicTrapAddForm)(
          _ => JsonBadRequest("Нужны ходы").toFuccess,
          (moves, hint, block) =>
            val line = lila.web.LogicTraps.lineOf(moves.split("\\s+").toList.filter(_.nonEmpty), hint, block)
            lila.web.LogicTraps.parseLine(line) match
              case Left(err) => JsonBadRequest(err).toFuccess
              case Right(trap) =>
                val same = lines.filter(l =>
                  lila.web.LogicTraps
                    .parseLine(l)
                    .toOption
                    .exists(t => t.board == trap.board && t.uci == trap.uci)
                )
                save(lines.filterNot(same.contains) :+ line, "adds")
        )

  private def logicTrapsPost(using ctx: BodyContext[?], me: Me): Fu[play.api.mvc.Result] =
    bindForm(logicTrapsForm)(
      _ => Redirect(s"${routes.Dev.logicColors}?traps=1"),
      text =>
        val clean = text.linesIterator.map(_.trim).filter(_.nonEmpty).mkString("\n").take(20000)
        val parsed = lila.web.LogicTraps.parse(clean)
        if parsed.errors.nonEmpty then BadRequest.page(views.dev.logicTraps(clean, parsed))
        else
          lila.log.system.info(s"setting ${me.username} changes logic traps to ${parsed.traps.size} lines")
          env.web.settings.logicTraps
            .set(clean)
            .inject(Redirect(s"${routes.Dev.logicColors}?traps=1").flashSuccess)
    )

  // Метка «читер» (engine): кто помечен, когда и кем — по последней записи модлога.
  private def logicMarks: Fu[List[(String, Option[Instant], String)]] =
    import lila.db.dsl.{ list, Bdoc }
    val repo = env.user.repo
    for
      docs <- repo.coll
        .find(repo.engineSelect(true))
        .cursor[Bdoc]()
        .list(500)
      rows <- scala.concurrent.Future.sequence:
        docs
          .flatMap(d =>
            d.getAsOpt[String]("_id").map(id => UserId(id) -> (d.getAsOpt[String]("username") | ""))
          )
          .map: (id, name) =>
            env.mod.logApi
              .userHistory(id)
              .map(_.find(_.action == lila.mod.Modlog.engine))
              .map: last =>
                (
                  if name.nonEmpty then name else id.toString,
                  last.map(_.date),
                  last.fold("")(m => if m.isLichess then "автоматически" else m.mod.toString)
                )
    yield rows.sortBy(r => (r._2.fold(0L)(-_.toEpochMilli), r._1.toLowerCase))

  private val logicMarksForm =
    import play.api.data.Forms.*
    play.api.data.Form(single("user" -> text))

  // Снятие без записи «unengine» в модлог: иначе автопроверка больше никогда не пометит этого игрока.
  private def logicMarksPost(using ctx: BodyContext[?], me: Me): Fu[play.api.mvc.Result] =
    val back = Redirect(s"${routes.Dev.logicColors}?marks=1")
    val on = get("op").contains("set")
    bindForm(logicMarksForm)(
      _ => back.toFuccess,
      name =>
        UserStr.read(name.trim.stripPrefix("@")) match
          case None => back.flashFailure("Нет такого игрока").toFuccess
          case Some(u) =>
            env.report.api
              .getSuspect(u)
              .flatMap:
                case None => back.flashFailure(s"Нет игрока ${name.trim}").toFuccess
                case Some(sus) if sus.user.marks.engine == on =>
                  back
                    .flashFailure(if on then s"${sus.user.username} уже помечен"
                    else s"У ${sus.user.username} нет метки")
                    .toFuccess
                case Some(sus) =>
                  val id = sus.user.id
                  lila.log.system
                    .info(s"setting ${me.username} ${if on then "sets" else "removes"} engine mark of $id")
                  for
                    _ <-
                      if on then env.mod.api.setEngine(sus, true)
                      else
                        for
                          _ <- env.user.repo.setEngine(id, false)
                          _ = lila.common.Bus.pub(lila.core.mod.MarkCheater(id, false))
                          // Прощённые партии не должны снова дать метку: автопроверка
                          // берёт сводку по сохранённым оценкам, оставляем только новые.
                          _ <- env.mongo
                            .mainDb(lila.core.config.CollName("player_assessment"))
                            .delete
                            .one(lila.db.dsl.bdoc("userId" -> id.value))
                        yield ()
                    _ <- env.user.noteApi.write(
                      id,
                      if on then "Метка «читер» поставлена вручную в «Настройках площадки»."
                      else
                        "Метка «читер» снята в «Настройках площадки», прежние оценки партий удалены. Автопроверка продолжает работать по новым партиям."
                      ,
                      modOnly = true,
                      dox = false
                    )
                  yield back.flashSuccess(
                    if on then s"Метка поставлена: ${sus.user.username}"
                    else s"Метка снята: ${sus.user.username}"
                  )
    )

  private val logicColorsForm =
    import play.api.data.Forms.*
    play.api.data.Form(tuple("bg" -> text, "primary" -> text, "reset" -> optional(text)))

  def logicColorsPost = SecureBody(_.Settings) { ctx ?=> me ?=>
    if getBool("marks") then logicMarksPost
    else if getBool("traps") then get("op").fold(logicTrapsPost)(logicTrapsApi)
    else
      bindForm(logicColorsForm)(
        _ => Redirect(routes.Dev.logicColors),
        (bg, primary, reset) =>
          val s = env.web.settings
          val clean = (v: String) => if reset.isDefined then "" else lila.web.LogicColors.clean(v) | ""
          lila.log.system
            .info(s"setting ${me.username} changes logic colors to ${clean(bg)} ${clean(primary)}")
          for
            _ <- s.logicBg.set(clean(bg))
            _ <- s.logicPrimary.set(clean(primary))
          yield Redirect(routes.Dev.logicColors).flashSuccess
      )
  }

  def cli = Secure(_.Cli) { _ ?=> _ ?=>
    Ok.page:
      views.dev.cli(env.api.cli.form, none)
  }

  def cliPost = SecureBody(_.Cli) { _ ?=> me ?=>
    bindForm(env.api.cli.form)(
      err => BadRequest.page(views.dev.cli(err, "Invalid command".some)),
      command =>
        Ok.async:
          runCommand(command).map: res =>
            views.dev.cli(env.api.cli.form.fill(command), s"$command\n\n$res".some)
    )
  }

  def command = ScopedBody(parse.tolerantText)(Seq(_.Preference.Write)) { ctx ?=> _ ?=>
    isGranted(_.Cli).so:
      runCommand(ctx.body.body).map { Ok(_) }
  }

  def ipTiers = Secure(_.IpTiers) { ctx ?=> _ ?=>
    env.security.ipTiers.form.flatMap: form =>
      Ok.page(views.dev.ipTiers(form))
  }

  def ipTiersPost = SecureBody(_.IpTiers) { ctx ?=> _ ?=>
    Found(env.security.ipTiers.form.map(_.toOption)): form =>
      bindForm(form)(
        err => BadRequest.page(views.dev.ipTiers(Right(err))),
        v => env.security.ipTiers.writeToFile(v).inject(Redirect(routes.Dev.ipTiers).flashSuccess)
      )
  }

  def emailErrorPost = SecuredScopedBody(_.SetEmail)():
    if env.web.emailError.setFromReq().isDefined then NoContent else BadRequest

  def emailErrorGet = Open: ctx ?=>
    ctx.isAnon
      .so(lila.security.EmailConfirm.cookie.get(ctx.req))
      .flatMap(u => env.web.emailError.get(u.email))
      .fold(NoContent)(Ok(_))

  private def runCommand(command: String)(using Me): Fu[String] =
    for
      _ <- env.mod.logApi.cli(command)
      res <- env.api.cli.run(command.split(" ").toList)
    yield res

  private def settingsListForMe(using me: Me) =
    settingsList.flatMap: (categ, settings) =>
      val sets = settings.filter(s => isGranted(s.perm))
      sets.nonEmpty.option(categ -> sets)

  private lazy val settingsList = List[(String, List[lila.memo.SettingStore[?]])](
    "Moderation" -> List(
      env.security.ugcArmedSetting,
      env.security.spamKeywordsSetting,
      env.irwin.irwinApi.thresholds,
      env.irwin.kaladinApi.thresholds,
      env.report.scoreThresholdsSetting,
      env.report.discordScoreThresholdSetting
    ),
    "Cheat" -> List(
      env.round.selfReport.endGameSetting,
      env.round.selfReport.markUserSetting,
      env.bot.boardReport.domainSetting
    ),
    "Security" -> List(
      env.oAuth.originBlocklistSetting,
      env.security.proxy2faSetting,
      env.security.lichobileLogin
    ),
    "Mailing" -> List(
      env.mailer.mailerSecondaryPermilleSetting,
      env.mailer.canSendEmailsSetting
    ),
    "Streamer" -> List(
      env.streamer.homepageMaxSetting,
      env.streamer.alwaysFeaturedSetting
    ),
    "Permissions" -> List(
      env.web.settings.noDelaySecret,
      env.web.settings.prizeTournamentMakers
    ),
    "Limits" -> List(
      env.web.settings.apiTimeline,
      env.web.settings.apiExplorerGamesPerSecond,
      env.recap.parallelismSetting,
      env.fishnet.openingBookDepth
    ),
    "Broadcast" -> List(
      env.relay.proxy.domainRegex,
      env.memo.proxy.hostPort,
      env.memo.proxy.credentials
    ),
    "Tutor" -> List(
      env.tutor.nbAnalysisSetting,
      env.tutor.parallelismSetting
    ),
    "Automod" -> List(
      env.report.automod.imageModelSetting,
      env.report.automod.imagePromptSetting,
      env.report.api.commsModelSetting,
      env.report.api.commsPromptSetting,
      env.ublog.ublogAutomod.modelSetting,
      env.ublog.ublogAutomod.promptSetting
    ),
    "Mobile" -> List(
      env.web.lichobileAnnounceApi.lichobileUpgrade,
      env.web.lichobileDisables.setting
    ),
    "Config" -> List(
      env.plan.donationGoalSetting,
      env.tournament.reloadEndpointSetting
    )
  )
