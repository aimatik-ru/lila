package lila.team
package ui

import scalalib.paginator.Paginator

import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class TeamUi(helpers: Helpers, markdownCache: lila.memo.MarkdownCache):
  import helpers.{ *, given }
  import trans.team as trt

  def TeamPage(title: String) = Page(title).css("team").js(infiniteScrollEsmInit)

  object markdown:
    private val options = lila.memo.MarkdownOptions(
      autoLink = true,
      header = true,
      headerAnchorLink = true,
      list = true,
      table = true,
      blockQuote = false,
      maxPgns = Max(0)
    )

    def apply(team: Team, text: Markdown): Frag =
      markdownCache.toHtmlSyncWithoutPgnEmbeds(s"team:${team.id}", text, options)

  def menu(currentTab: Option[String])(using ctx: Context) =
    val tab = ~currentTab
    st.nav(cls := "page-menu__menu subnav")(
      Granter
        .opt(_.Admin)
        .option(
          a(cls := tab.active("form"), href := routes.Team.form)(trt.newTeam())
        ),
      a(cls := tab.active("kubok"), href := routes.Team.show(TeamId("kubok")))("Кубок Логика"),
      tab
        .contains("requests")
        .option:
          a(cls := tab.active("requests"), href := routes.Team.requests)("Заявки на вступление")
      ,
      ctx.isAuth.option(
        a(cls := tab.active("mine"), href := routes.Team.mine)(trt.myTeams())
      ),
      ctx.isAuth.option(
        a(cls := tab.active("updates"), href := routes.Team.updates())(trt.teamUpdates())
      ),
      ctx.isAuth.option(
        a(cls := tab.active("leader"), href := routes.Team.leader)(trt.leaderTeams())
      ),
      a(cls := tab.active("all"), href := routes.Team.all())(trt.allTeams())
    )

  def teamTr(t: Team.WithMyLeadership)(using Context) =
    val isMine = isMyTeamSync(t.id)
    tr(cls := "paginated")(
      td(cls := "subject")(
        a(
          dataIcon := Icon.Group,
          cls := List(
            "team-name text" -> true,
            "mine" -> isMine
          ),
          href := routes.Team.show(t.id)
        )(
          t.name,
          t.flair.map(teamFlair),
          t.amLeader.option(em("leader"))
        ),
        t.intro.map(span(_))
      ),
      td(cls := "actions")(
        isMine.option:
          st.form(action := routes.Team.quit(t.id), method := "post")(
            submitButton(cls := "button button-empty button-red button-thin yes-no-confirm team__quit")(
              trt.quitTeam.txt()
            )
          )
      ),
      td(cls := "info")(
        p(trt.nbMembers.plural(t.nbMembers, t.nbMembers.localize))
      )
    )

  def membersPage(t: Team, pager: Paginator[TeamMember.UserAndDate])(using Context) =
    TeamPage(t.name).graph(
      title = s"${t.name} • ${trt.teamRecentMembers.txt()}",
      url = routeUrl(routes.Team.show(t.id)),
      description = t.intro.so(shorten(_, 152))
    ):
      main(cls := "page-small box")(
        boxTop(
          h1(
            teamLink(t.light, true),
            " • ",
            trt.nbMembers.plural(t.nbMembers, t.nbMembers.localize)
          )
        ),
        table(cls := "team-members slist slist-pad slist-invert"):
          tbody(cls := "infinite-scroll")(
            pager.currentPageResults.map { case TeamMember.UserAndDate(u, date) =>
              tr(cls := "paginated")(
                td(lightUserLink(u)),
                td(momentFromNowOnce(date))
              )
            },
            pagerNextTable(pager, np => routes.Team.members(t.slug, np).url)
          )
      )

  object list:

    def search(text: String, teams: Paginator[Team.WithMyLeadership])(using Context) =
      list(
        name = s"""${trans.search.search.txt()} "$text"""",
        teams = teams,
        nextPageUrl = n => routes.Team.search(text, n).url,
        search = text
      )

    def all(teams: Paginator[Team.WithMyLeadership])(using Context) =
      list(
        name = trt.teams.txt(),
        teams = teams,
        nextPageUrl = n => routes.Team.all(n).url
      )

    def mine(teams: List[Team.WithMyLeadership])(using ctx: Context) =
      TeamPage(trt.myTeams.txt()):
        main(cls := "team-list page-menu")(
          menu("mine".some),
          div(cls := "page-menu__content box")(
            h1(cls := "box__top")(trt.myTeams()),
            standardFlash.map(div(cls := "box__pad")(_)),
            ctx.me.filter(me => Team.maxJoin(me) < teams.size).map { me =>
              flashMessage("failure"):
                s"Вы состоите в ${teams.size} командах при лимите ${Team.maxJoin(me)}. " +
                  "Чтобы вступить в другие, выйдите из некоторых команд."
            },
            table(cls := "slist slist-pad slist-invert")(
              if teams.nonEmpty then tbody(teams.map(teamTr(_)))
              else noTeam()
            )
          )
        )

    def ledByMe(teams: List[Team])(using Context) =
      TeamPage(trt.myTeams.txt()):
        main(cls := "team-list page-menu")(
          menu("leader".some),
          div(cls := "page-menu__content box")(
            h1(cls := "box__top")(trt.teamsIlead()),
            standardFlash,
            table(cls := "slist slist-pad slist-invert")(
              if teams.nonEmpty then tbody(teams.map(Team.WithMyLeadership(_, true)).map(teamTr(_)))
              else noTeam()
            )
          )
        )

    private def list(
        name: String,
        teams: Paginator[Team.WithMyLeadership],
        nextPageUrl: Int => String,
        search: String = ""
    )(using Context) =
      TeamPage(s"$name — страница ${teams.currentPage}"):
        main(cls := "team-list page-menu")(
          menu("all".some),
          div(cls := "page-menu__content box")(
            boxTop(
              h1(name),
              div(cls := "box__top__actions")(
                st.form(cls := "search", action := routes.Team.search())(
                  input(st.name := "text", value := search, placeholder := trans.search.search.txt())
                )
              )
            ),
            standardFlash,
            table(cls := "slist slist-pad slist-invert")(
              if teams.nbResults > 0 then
                tbody(cls := "infinite-scroll")(
                  teams.currentPageResults.map(teamTr),
                  pagerNextTable(teams, nextPageUrl)
                )
              else noTeam()
            )
          )
        )

    def cup(
        settings: Team.CupSettings,
        teams: List[(Team, List[UserId], Set[UserId])],
        rules: Option[Html],
        editRules: Option[String],
        bracket: Option[(List[List[Team.CupMatch]], Map[String, Team.CupTour])],
        planned: Map[Int, Instant],
        plannedRaw: Map[Int, String],
        pult: Option[Team.CupPult] = none,
        live: List[Team.CupLive] = Nil
    )(using ctx: Context) =
      // у админа на доске формы плей-офф: meta refresh стирал введённые даты до нажатия кнопки,
      // поэтому админу — перезагрузка скриптом, которая ждёт, пока поля панели не тронуты
      val admin = Granter.opt(_.ManageTeam)
      val reloadJs =
        """var t=0,p='.kl-cup__bracket-admin',m=function(e){if(e.target.closest&&e.target.closest(p))t=1};""" +
          """document.addEventListener('input',m,true);document.addEventListener('change',m,true);""" +
          """setInterval(function(){var a=document.activeElement;""" +
          """if(!t&&!(a&&a.closest&&a.closest(p)))location.reload()},30000)"""
      val page = TeamPage(s"${settings.title}: команды")
      (if admin then page.js(Esm("bits", embedJsUnsafeLoadThen(reloadJs))) else page).apply:
        main(cls := "team-list page-menu")(
          menu("kubok".some),
          div(cls := "page-menu__content box box-pad")(
            // во время боёв счёт обновляется чаще
            (!admin).option(
              meta(httpEquiv := "refresh", content := (if live.exists(!_.finished) then "15" else "30"))
            ),
            h1(cls := "box__top")(
              s"${settings.title}: команды",
              a(cls := "button button-empty", href := routes.Team.show(TeamId("kubok-history")))(
                "История кубков"
              )
            ),
            standardFlash,
            cupSchedule(
              settings,
              bracket.fold(0)(_._1.size),
              planned ++ bracket.fold(Map.empty[Int, Instant])(b => cupTourStarts(b._2))
            ),
            p(
              s"Команд: ${teams.size}, игроков: ${teams.map(_._2.size).sum}. ",
              if !admin && live.exists(!_.finished) then
                "Пока идут бои, страница обновляется каждые 15 секунд."
              else "Страница обновляется сама каждые 30 секунд.",
              admin.option(" Пока вы заполняете поля плей-офф, обновление ждёт.")
            ),
            pult.filter(_ => admin).map(cupPultView(_, teams.map(t => t._1.id -> t._1.name).toMap, live)),
            cupLiveView(
              live,
              teams.map(t => t._1.id -> t._1.name).toMap,
              ctx.me.flatMap(me => teams.find(_._2.contains(me.userId)).map(_._1.id)),
              ctx.me.exists(me => teams.exists(_._2.contains(me.userId)))
            ),
            rules.map(r =>
              st.section(cls := "kl-cup__rules", style := "margin:1em 0 2em")(h2("Регламент"), r)
            ),
            cupOutRules(settings),
            editRules.map(text =>
              // класс панели: пока поле в фокусе или тронуто, автообновление страницы ждёт
              // свёрнуто под стрелочку: поле правки не занимает экран (Антон, 21.11)
              details(cls := "kl-cup__bracket-admin box", style := "margin:1em 0 2em;padding:1em")(
                summary(style := "cursor:pointer;font-weight:bold")(
                  if rules.isDefined then "Изменить регламент" else "Добавить регламент"
                ),
                p(
                  small(
                    "Пишите обычным текстом: каждая строка станет отдельным абзацем. ",
                    "После сохранения регламент сразу видят все. Чтобы убрать регламент, очистите поле и сохраните."
                  )
                ),
                postForm(action := routes.Team.edit(TeamId("kubok")))(
                  input(tpe := "hidden", name := "bracket", value := "rules"),
                  textarea(
                    name := "rules",
                    rows := "14",
                    style := "width:100%;box-sizing:border-box;font:inherit;line-height:1.4"
                  )(text),
                  p(button(cls := "button", tpe := "submit")("Сохранить регламент"))
                )
              )
            ),
            Granter
              .opt(_.ManageTeam)
              .option(
                p(
                  a(cls := "button", href := routes.Team.edit(TeamId("kubok")))(
                    "Составы команд: наполнить и переиграть"
                  )
                )
              ),
            bracket.map((rounds, tours) =>
              cupBracket(
                rounds,
                tours,
                teams.map(t => t._1.id -> t._1.name).toMap,
                teams.map(t => t._1.id -> t._2).toMap,
                planned
              )
            ),
            // редкие действия свёрнуты: на виду только пульт тура
            admin.option(
              details(cls := "kl-cup__bracket-admin", style := "margin:1em 0 2em")(
                summary(style := "cursor:pointer;font-weight:bold")("Настройки и редкие действия"),
                cupSettingsAdmin(settings, bracket.exists(_._1.nonEmpty)),
                cupBracketAdmin(settings, bracket, plannedRaw, teams.count(_._2.nonEmpty))
              )
            ),
            if teams.isEmpty then p("Команды ещё не созданы.")
            else
              div(cls := "kl-cup")(
                teams.map: (t, members, leaders) =>
                  val captain = members.filter(leaders.contains)
                  val others = members.filterNot(leaders.contains)
                  st.section(cls := "kl-cup__team", style := "margin:1.5em 0")(
                    h2(
                      a(href := routes.Team.show(t.id))(t.name),
                      " ",
                      small(s"${members.size} из ${Team.cupMaxMembers}", t.isCupFull.option(" — набрана"))
                    ),
                    if members.isEmpty then p("Пока никого — ждём первого игрока, он станет капитаном.")
                    else
                      ol(
                        captain.map(u => li(userIdLink(u.some), " — капитан")),
                        others.map(u => li(userIdLink(u.some)))
                      )
                  )
              )
          )
        )

    private def cupHm(i: Instant) = cupTimeFmt.format(i)

    // «Логик»: бои текущего круга со счётом — видят все
    private def cupLiveView(
        live: List[Team.CupLive],
        names: Map[TeamId, String],
        myTeam: Option[TeamId],
        inCup: Boolean
    )(using Context) =
      val now = nowInstant
      val tn = (id: TeamId) => names.getOrElse(id, id.value)
      val going = live.filterNot(_.finished)
      val mine = myTeam.flatMap(t => going.find(_.scores.exists(_._1 == t)))
      live.nonEmpty.option(
        st.section(cls := "kl-cup__live", style := "margin:1em 0 2em")(
          h2(if going.nonEmpty then "Сейчас играют" else "Бои круга"),
          mine.map(l =>
            p(style := "font-size:1.15em")(
              strong("Твой бой: "),
              if l.key.startsWith("out") then "товарищеский бой"
              else l.scores.map((t, _) => tn(t)).mkString(" — "),
              " ",
              a(cls := "button", href := s"/tournament/${l.id}")("Играть")
            )
          ),
          (mine.isEmpty && !inCup && going.nonEmpty).option(
            p("Составы закрыты, бои можно смотреть.")
          ),
          div(style := "display:flex;flex-wrap:wrap;gap:.8em")(
            live.map: l =>
              val status =
                if l.finished then "завершён"
                else if l.started then
                  val left = java.time.Duration.between(now, l.finishesAt).toMinutes.max(0)
                  s"идёт, осталось $left мин" + (if l.playing > 0 then s", партий сейчас: ${l.playing}"
                                                 else "")
                else s"начнётся в ${cupHm(l.startsAt)}"
              div(
                style := s"min-width:15em;padding:.6em .8em;border:1px solid rgba(128,128,128,.4);border-radius:6px${
                    if l.started && !l.finished then ";background:rgba(98,153,36,.12)" else ""
                  }"
              )(
                if l.key.startsWith("out") then
                  frag(
                    strong("Товарищеский бой"),
                    ol(style := "margin:.3em 0 .3em 1.2em")(
                      l.scores.map((t, s) => li(s"${tn(t)}: $s"))
                    )
                  )
                else
                  l.scores match
                    case List((x, sx), (y, sy)) =>
                      div(style := "font-size:1.1em")(tn(x), " ", strong(s"$sx : $sy"), " ", tn(y))
                    case other => div(other.map((t, s) => s"${tn(t)} $s").mkString(", "))
                ,
                div(small(status, " · ", a(href := s"/tournament/${l.id}")("открыть бой")))
              )
          )
        )
      )

    // «Логик»: пульт тура — только администраторам
    private def cupPultView(pu: Team.CupPult, names: Map[TeamId, String], live: List[Team.CupLive])(using
        Context
    ) =
      import Cup.Phase
      val url = routes.Team.edit(TeamId("kubok"))
      val tn = (id: TeamId) => names.getOrElse(id, id.value)
      val round = (r: Int) => if pu.nbRounds > 0 then Team.cupRoundName(r, pu.nbRounds) else "Первый круг"
      val step = pu.phase match
        case Phase.Idle | Phase.Waiting(_, _) => 0
        case Phase.Rosters(_, _) => 1
        case Phase.Starting(_) | Phase.Playing(_) => 2
        case Phase.Decide(_) | Phase.Done => 3
      val steps = List("Ждём", "Составы открыты", "Бои идут", "Круг сыгран")
      val auto =
        if pu.auto then " Нажимать ничего не нужно."
        else " Автоматика выключена: круг начнётся только по кнопке «Запустить сейчас»."
      val close = pu.closeAt.fold("")(cupHm)
      val next = pu.phase match
        case Phase.Idle => "Даты следующего круга нет. Укажите её в настройках ниже."
        case Phase.Waiting(r, at) =>
          s"${round(r)}: ${cupDateFmt.format(at)}. В $close составы закроются, в ${cupHm(at)} начнутся бои." + auto
        case Phase.Rosters(r, at) =>
          if pu.missed then
            s"${round(r)}: время старта ${cupHm(at)} прошло больше 30 минут назад. Сама площадка круг не начнёт: нажмите «Запустить сейчас» или «Отложить на 10 минут»."
          else
            s"${round(r)}: составы открыты, их можно править. В $close они закроются, в ${cupHm(at)} начнутся бои." + auto
        case Phase.Starting(r) =>
          s"${round(r)} стартует: создаются бои и записываются составы. Обновите страницу через минуту."
        case Phase.Playing(r) =>
          s"${round(r)}: бои идут, составы закрыты. Когда все бои закончатся, составы откроются сами."
        case Phase.Decide(r) =>
          s"${round(r)} сыгран, но у части пар нет победителя. Решите их ниже, иначе следующий круг не начнётся."
        case Phase.Done => "Все круги кубка сыграны."
      def act(what: String, label: String, extra: Frag*)(
          confirmMsg: Option[String] = none,
          red: Boolean = false
      ) =
        postForm(action := url, style := "display:inline-block;margin:.3em .5em .3em 0")(
          input(tpe := "hidden", name := "bracket", value := what),
          extra,
          button(
            cls := s"button${if red then " button-red" else ""}",
            tpe := "submit",
            confirmMsg.map(m => attr("onclick") := s"return confirm('$m')")
          )(label)
        )
      def hidden(n: String, v: String) = input(tpe := "hidden", name := n, value := v)
      val beforeStart = pu.phase match
        case Phase.Waiting(_, _) | Phase.Rosters(_, _) => true
        case _ => false
      val playing = pu.phase match
        case Phase.Starting(_) | Phase.Playing(_) => true
        case _ => false
      st.section(cls := "kl-cup__pult box", style := "margin:1em 0 2em;padding:1em")(
        h2("Пульт тура (видно только администраторам)"),
        div(style := "display:flex;flex-wrap:wrap;gap:.4em;margin:.5em 0")(
          steps.zipWithIndex.map: (s, i) =>
            span(
              style := s"padding:.25em .7em;border-radius:1em;${
                  if i == step then "background:#629924;color:#fff;font-weight:bold"
                  else "background:rgba(128,128,128,.15);opacity:.7"
                }"
            )(s)
        ),
        p(style := "font-size:1.1em")(next),
        div(
          beforeStart.option(act("postpone", "Отложить на 10 минут")()),
          (beforeStart || pu.phase == Phase.Idle).option(
            act("startnow", "Запустить сейчас")(
              "Запустить круг сейчас? Составы закроются, через полторы минуты начнутся бои.".some
            )
          ),
          (!playing).option(
            a(cls := "button button-empty", href := url, style := "margin:.3em 0")("Править составы")
          ),
          playing.option(
            act("stop", "Остановить круг и вернуться к составам")(
              "Остановить круг? Все бои круга отменятся и не пойдут в зачёт, составы откроются.".some,
              red = true
            )
          )
        ),
        pu.warnings.nonEmpty.option(
          ul(style := "margin:.8em 0;color:#c33")(pu.warnings.map(w => li(w)))
        ),
        (playing && pu.tours.nonEmpty).option(
          div(style := "margin:.8em 0")(
            h3("Бои круга"),
            ul(
              pu.tours.toList
                .sortBy(_._1)
                .map: (k, id) =>
                  li(style := "margin:.3em 0")(
                    a(href := s"/tournament/$id")(
                      if k.startsWith("out") then "Товарищеский бой"
                      else s"Пара ${k.dropWhile(_ != '.').drop(1).toIntOption.fold(k)(i => (i + 1).toString)}"
                    ),
                    // кто играет: без названий команд непонятно, какой бой пересоздаётся
                    live
                      .find(_.id == id)
                      .filter(_ => !k.startsWith("out"))
                      .map(l => s": ${l.scores.map((t, _) => tn(t)).mkString(" — ")}"),
                    " ",
                    act("recreate", "Пересоздать бой", hidden("key", k))(
                      "Пересоздать этот бой? Прежний бой отменится и не пойдёт в зачёт, новый начнётся через полторы минуты.".some
                    )
                  )
            )
          )
        ),
        pu.undecided.nonEmpty.option(
          div(style := "margin:.8em 0")(
            h3("Пары без победителя"),
            pu.undecided.map: m =>
              m.pair.map: (x, y) =>
                div(
                  style := "margin:.4em 0;padding:.5em;border:1px solid rgba(128,128,128,.4);border-radius:6px"
                )(
                  strong(s"${tn(x)} — ${tn(y)}"),
                  ": бой не состоялся или закончился вничью. ",
                  br,
                  act("recreate", "Пересоздать бой", hidden("key", m.key))(
                    "Пересоздать бой этой пары? Он начнётся через полторы минуты.".some
                  ),
                  act("award", s"Победа: ${tn(x)}", hidden("key", m.key), hidden("team", x.value))(),
                  act("award", s"Победа: ${tn(y)}", hidden("key", m.key), hidden("team", y.value))()
                )
          )
        ),
        pu.log.nonEmpty.option(
          details(style := "margin-top:.8em")(
            summary(style := "cursor:pointer")("Журнал тура"),
            ul(style := "font-size:.9em")(
              pu.log.map: (at, by, text) =>
                li(style := "margin:.2em 0")(strong(cupDateFmt.format(at)), s" · $by: $text")
            )
          )
        )
      )

    private val cupPerfs = List("rapid" -> "рапид", "blitz" -> "блиц", "classical" -> "классика")

    // регламент боя выбывших: из настроек кубка, видят все (Антон, 21.11)
    private def cupOutRules(s: Team.CupSettings) =
      val perf = cupPerfs.find(_._1 == s.perf).fold(s.perf)(_._2)
      val count =
        if s.leaders == 1 then "лучший результат команды"
        else if s.leaders >= s.size then "результаты всех игроков команды"
        else s"результаты ${s.leaders} лучших игроков команды"
      st.section(cls := "kl-cup__out-rules", style := "margin:1em 0 2em")(
        h2("Бой выбывших (параллельный турнир)"),
        p(
          "Для команд, которые проиграли свою пару в плей-офф. Команда попадает сюда, когда закончится бой её пары, ",
          "и играет бой выбывших вместе со следующим кругом кубка. На сетку кубка этот бой не влияет."
        ),
        p(
          "Каждый играет сам за себя и за команду. Соперника площадка подбирает из другой выбывшей команды, ",
          "после партии сразу даёт новую. Играть можно сколько угодно партий, пока идёт бой."
        ),
        p(
          s"Контроль: $perf, ${s.clock} мин + ${s.inc} сек на ход. Бой длится ${s.minutes} мин. ",
          if s.rated then "Партии рейтинговые. " else "Партии без рейтинга. ",
          "Берсерка нет."
        ),
        p(
          "Очки за партию: победа — 2, ничья — 1, поражение — 0. После двух побед подряд у игрока «огонь»: ",
          "победа даёт 4, ничья — 2, пока он снова не сыграет без победы. Ничья раньше 10-го хода очков не даёт."
        ),
        p(s"Очки команды: в зачёт идут $count. Побеждает команда, набравшая больше очков.")
      )
    private val cupClocks = List("1", "2", "3", "4", "5", "6", "7", "8", "10", "15", "20", "25", "30")
    private val cupZone = java.time.ZoneId.of("Europe/Moscow")
    private val cupTimeFmt = java.time.format.DateTimeFormatter.ofPattern("HH:mm").withZone(cupZone)
    private val cupDayFmt =
      java.time.format.DateTimeFormatter
        .ofPattern("d MMMM yyyy", java.util.Locale.forLanguageTag("ru"))
        .withZone(cupZone)
    private def cupParse(d: String): Option[Instant] =
      scala.util.Try(java.time.LocalDateTime.parse(d).atZone(cupZone).toInstant).toOption

    // «Логик»: время уже созданных боёв круга важнее плановой даты
    private def cupTourStarts(tours: Map[String, Team.CupTour]): Map[Int, Instant] =
      tours.toList
        .flatMap((k, t) => k.takeWhile(_ != '.').toIntOption.map(_ -> t.startsAt))
        .groupMapReduce(_._1)(_._2)((a, b) => if a.isBefore(b) then a else b)

    // «Логик»: расписание ближайшего круга для детей и родителей
    private def cupSchedule(s: Team.CupSettings, nbRounds: Int, planned: Map[Int, Instant]) =
      val now = nowInstant
      val next = planned
        .filter(_._2.isAfter(now.minusSeconds(3 * 3600)))
        .minByOption(_._2)
        .orElse(s.start.flatMap(cupParse).filter(_.isAfter(now)).map(0 -> _))
      next.map: (r, at) =>
        val round = if nbRounds > 0 then Team.cupRoundName(r, nbRounds) else "Первый круг"
        def at_(min: Int) = cupTimeFmt.format(at.minusSeconds(min * 60L))
        st.section(cls := "kl-cup__schedule", style := "margin:1em 0 1.5em")(
          h2(s"$round: ${cupDateFmt.format(at).takeWhile(_ != ',')}"),
          ul(
            (s.connectMin > 0).option(li(strong(at_(s.connectMin)), " — заходите на площадку")),
            (!s.formed && nbRounds == 0).option(
              li(
                strong(at_(s.formMin)),
                " — площадка собирает команды из тех, кто на ней. После сбора капитан может сменить название команды"
              )
            ),
            li(strong(cupTimeFmt.format(at)), " — начало боёв")
          )
        )

    // «Логик»: шаблон кубка и переход к следующему кубку (только админам)
    private def cupSettingsAdmin(s: Team.CupSettings, hasBracket: Boolean)(using Context) =
      val url = routes.Team.edit(TeamId("kubok"))
      val start = s.start.flatMap(cupParse)
      val status =
        if !s.auto then "Автоматика выключена: круг начнётся только кнопкой «Запустить сейчас» на пульте."
        else if s.by.isEmpty then "Сохраните настройки, и площадка начнёт собирать команды сама."
        else if start.isEmpty then "Укажите старт первого круга, без него автосбор не начнётся."
        else if s.formed || hasBracket then
          "Команды собраны. Бои каждого круга площадка создаёт сама в момент старта круга, за 2 минуты до начала боёв."
        else
          start.fold(""): at =>
            s"Команды соберутся ${cupDateFmt.format(at.minusSeconds(s.formMin * 60L))} из учеников, которые будут на площадке. До старта их можно править, жеребьёвка пройдёт при старте круга."
      def num(n: String, v: Int, lo: Int, hi: Int, w: String = "4em") =
        input(
          tpe := "number",
          name := n,
          value := v.toString,
          min := lo.toString,
          max := hi.toString,
          style := s"width:$w"
        )
      val safeTitle = s.title.replace("'", "")
      st.section(cls := "kl-cup__bracket-admin box", style := "margin:1em 0 2em;padding:1em")(
        h2(s"Настройки: ${s.title} (видно только администраторам)"),
        p(status),
        p(
          small(
            s"Настройки, команды и сетка на этой странице относятся к кубку ${s.title}. ",
            "Закончить кубок и начать новый можно кнопкой внизу страницы."
          )
        ),
        postForm(action := url, style := "margin:.5em 0")(
          input(tpe := "hidden", name := "bracket", value := "settings"),
          div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center")(
            label("Название: ")(input(tpe := "text", name := "name", value := s.name, maxlength := "40")),
            // дата и время отдельно: datetime-local без выбранного времени браузер отправляет пустым
            label("Первый круг (МСК): ")(
              input(tpe := "date", name := "startDate", value := s.start.so(_.take(10))),
              " в ",
              input(
                tpe := "time",
                name := "startTime",
                value := s.start.map(_.drop(11).take(5)).filter(_.nonEmpty).getOrElse("18:00")
              )
            ),
            label("Между кругами, дней: ")(num("everyDays", s.everyDays, 1, 60)),
            label("Подключение за, мин: ")(num("connectMin", s.connectMin, 0, 240)),
            label("Сбор команд за, мин: ")(num("formMin", s.formMin, 1, 180))
          ),
          p(
            label(
              input(tpe := "checkbox", name := "auto", value := "true", s.auto.option(checked)),
              " Собирать команды и создавать бои автоматически"
            )
          ),
          div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center")(
            label("Игроков в команде: ")(num("size", s.size, 0, Team.cupMaxMembers)),
            label("Команд: ")(num("teams", s.teams, 0, 64)),
            label("Сила по рейтингу: ")(
              st.select(name := "perf")(
                cupPerfs.map((k, n) => st.option(value := k, (k == s.perf).option(selected))(n))
              )
            )
          ),
          div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center;margin-top:.6em")(
            label("Контроль, мин: ")(
              st.select(name := "clock")(
                cupClocks.map(v => st.option(value := v, (v == s.clock).option(selected))(v))
              )
            ),
            label("+ сек: ")(num("inc", s.inc, 0, 60)),
            label("Бой длится, мин: ")(num("minutes", s.minutes, 10, 360, "5em")),
            label("Очки команды по лучшим игрокам: ")(num("leaders", s.leaders, 1, 5)),
            label(
              input(tpe := "checkbox", name := "rated", value := "true", s.rated.option(checked)),
              " рейтинговые"
            )
          ),
          p(
            small(
              "Игроков в команде 0: по одному ученику из каждой группы A–E. ",
              "Команд 0: столько, сколько получится из учеников на площадке. ",
              "Между кругами 7: второй круг через неделю после первого, третий через две и так далее. ",
              "Очки команды по лучшим игрокам 2: в бою команде засчитываются очки двух её лучших игроков."
            )
          ),
          button(cls := "button", tpe := "submit")("Сохранить настройки")
        ),
        postForm(action := url, style := "margin:1.5em 0 .5em")(
          input(tpe := "hidden", name := "bracket", value := "newcup"),
          h3("Отправить кубок в историю и начать новый"),
          p(
            s"${s.title} целиком переедет в «Историю кубков»: регламент, команды, сетка и итоги сыгранных боёв. ",
            "Бои, которые ещё не начались, отменятся. Настройки останутся. ",
            "После этого укажите дату первого круга и напишите регламент нового кубка."
          ),
          div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center")(
            label("Подпись в истории: ")(
              input(
                tpe := "text",
                name := "label",
                value := s.title,
                maxlength := "80",
                style := "width:18em"
              )
            ),
            label("Номер нового кубка: ")(num("next", s.n + 1, 1, 999))
          ),
          p(
            label(
              input(tpe := "checkbox", name := "keep", value := "true"),
              " Оставить команды. Без галочки команды соберутся заново из учеников на площадке."
            )
          ),
          button(
            cls := "button button-empty",
            tpe := "submit",
            attr(
              "onclick"
            ) := s"return confirm('Отправить $safeTitle в историю? Бои, которые ещё не начались, отменятся. Вернуть кубок обратно будет нельзя.')"
          )("Отправить в историю и начать новый")
        )
      )

    // «Логик»: прошедшие кубки — сетка, составы и итоги всех боёв
    def cupHistory(archives: List[Team.CupArchive])(using Context) =
      TeamPage("История кубков"):
        main(cls := "team-list page-menu")(
          menu("kubok".some),
          div(cls := "page-menu__content box box-pad")(
            h1(cls := "box__top")(
              "История кубков",
              a(cls := "button button-empty", href := routes.Team.show(TeamId("kubok")))("Текущий кубок")
            ),
            if archives.isEmpty then p("Здесь появятся кубки, которые уже закончились.")
            else
              archives.zipWithIndex.map: (c, i) =>
                val names = c.teams.map(t => TeamId(t.id) -> t.name).toMap
                val name = (id: TeamId) => names.getOrElse(id, id.value)
                val rounds = Team.cupBracket(c.slots, c.winners.get)
                val champion = rounds.lastOption.flatMap(_.headOption).flatMap(_.winner)
                val byId = c.battles.map(b => b.id -> b).toMap
                val tours = c.tours.flatMap: (k, id) =>
                  byId.get(id).map(b => k -> Team.CupTour(b.id, b.startsAt, true, b.finished))
                // плановая дата первого круга не в счёт, если кубок закрыли раньше неё
                val firstDay = (c.dates.get(0).flatMap(cupParse).filterNot(_.isAfter(c.closedAt)).toList ++
                  c.battles.headOption.map(_.startsAt)).minOption
                details(
                  cls := "kl-cup__archive",
                  style := "margin:1em 0;border-bottom:1px solid rgba(128,128,128,.3)",
                  (i == 0).option(attr("open") := "")
                )(
                  summary(style := "cursor:pointer;padding:.5em 0;font-size:1.2em")(
                    strong(c.title),
                    " · ",
                    firstDay.fold("")(d => s"${cupDayFmt.format(d)} — ") + cupDayFmt.format(c.closedAt),
                    champion.map(w => frag(" · победитель: ", strong(name(w)), " 🏆"))
                  ),
                  c.rules.map(r =>
                    details(cls := "kl-cup__rules", style := "margin:.5em 0 1em")(
                      summary(style := "cursor:pointer")("Регламент"),
                      raw(r)
                    )
                  ),
                  // сетка без единого сыгранного матча (кубок закрыли до старта) не показывается
                  (rounds.nonEmpty && c.winners.nonEmpty).option(
                    cupBracket(
                      rounds,
                      tours,
                      names,
                      c.teams.map(t => TeamId(t.id) -> t.members.map(UserId(_))).toMap,
                      c.dates.flatMap((r, d) => cupParse(d).map(r -> _))
                    )
                  ),
                  c.teams.nonEmpty.option(
                    st.section(style := "margin:1em 0")(
                      h2("Команды"),
                      div(style := "display:flex;flex-wrap:wrap;gap:1em 2em")(
                        c.teams.map: t =>
                          div(style := "min-width:14em")(
                            strong(t.name),
                            ol(
                              t.members.map: m =>
                                li(
                                  userIdLink(UserId(m).some, withOnline = false),
                                  t.captain.contains(m).option(" — капитан")
                                )
                            )
                          )
                      )
                    )
                  ),
                  c.battles.nonEmpty.option(
                    st.section(style := "margin:1em 0")(
                      h2("Бои"),
                      ul(
                        c.battles.map: b =>
                          li(style := "margin:.4em 0")(
                            a(href := s"/tournament/${b.id}")(b.name),
                            s", ${cupDateFmt.format(b.startsAt)}",
                            b.standing.nonEmpty.option(
                              frag(
                                ": ",
                                b.standing.zipWithIndex.map:
                                  case ((t, score), j) =>
                                    frag((j > 0).option(", "), s"${j + 1}. ${name(TeamId(t))} ($score)")
                              )
                            )
                          )
                      )
                    )
                  )
                )
          )
        )

    private val cupDateFmt =
      java.time.format.DateTimeFormatter
        .ofPattern("d MMMM, HH:mm", java.util.Locale.forLanguageTag("ru"))
        .withZone(java.time.ZoneId.of("Europe/Moscow"))

    // «Логик»: сетка плей-офф кубка. Каждая пара — отдельный «Командный бой».
    private def cupBracket(
        rounds: List[List[Team.CupMatch]],
        tours: Map[String, Team.CupTour],
        names: Map[TeamId, String],
        members: Map[TeamId, List[UserId]],
        planned: Map[Int, Instant]
    )(using Context) =
      val nb = rounds.size
      val name = (id: TeamId) => names.getOrElse(id, id.value)
      def side(s: Team.CupSide, winner: Option[TeamId]) = s match
        case Team.CupSide.Of(id) =>
          val won = winner.contains(id)
          val lost = winner.exists(_ != id)
          div(
            style := s"padding:.35em .6em;${
                if won then "font-weight:bold;background:rgba(98,153,36,.18);" else ""
              }${if lost then "opacity:.5;text-decoration:line-through;" else ""}"
          )(
            a(href := routes.Team.show(id))(name(id)),
            won.option(" ✓"),
            div(style := "font-size:.8em;opacity:.8;font-weight:normal")(
              members
                .get(id)
                .filter(_.nonEmpty)
                .fold(frag("пока без игроков")): ms =>
                  frag(
                    s"${ms.size} из ${Team.cupMaxMembers}: ",
                    ms.zipWithIndex
                      .map((u, i) => frag((i > 0).option(", "), userIdLink(u.some, withOnline = false)))
                  )
            )
          )
        case Team.CupSide.Empty => div(style := "padding:.35em .6em;opacity:.6;font-style:italic")("без игры")
        case Team.CupSide.Waiting => div(style := "padding:.35em .6em;opacity:.6")("…")
      st.section(cls := "kl-cup__bracket", style := "margin:1.5em 0 2em")(
        h2("Сетка плей-офф"),
        div(style := "display:flex;gap:1.2em;overflow-x:auto;padding-bottom:.5em;align-items:stretch")(
          rounds.map: ms =>
            val r = ms.headOption.fold(1)(_.round)
            val date = ms.flatMap(m => tours.get(m.key)).map(_.startsAt).minOption.orElse(planned.get(r))
            div(style := "min-width:15em;display:flex;flex-direction:column")(
              div(style := "text-align:center;margin-bottom:.6em")(
                strong(Team.cupRoundName(r, nb), (r == nb - 1).option(" 🏆")),
                br,
                small(date.fold("дата не назначена")(cupDateFmt.format))
              ),
              div(style := "display:flex;flex-direction:column;justify-content:space-around;flex:1;gap:.8em")(
                ms.map: m =>
                  val tour = tours.get(m.key)
                  div(style := "border:1px solid rgba(128,128,128,.4);border-radius:6px;overflow:hidden")(
                    side(m.a, m.winner),
                    div(style := "border-top:1px solid rgba(128,128,128,.25)"),
                    side(m.b, m.winner),
                    tour.map(t =>
                      div(
                        style := "padding:.25em .6em;font-size:.85em;border-top:1px solid rgba(128,128,128,.25)"
                      )(
                        a(href := s"/tournament/${t.id}")(
                          if t.finished then "итоги боя"
                          else if t.started then "идёт бой — смотреть"
                          else s"бой ${cupDateFmt.format(t.startsAt)}"
                        )
                      )
                    )
                  )
              )
            )
        )
      )

    private def cupBracketAdmin(
        settings: Team.CupSettings,
        bracket: Option[(List[List[Team.CupMatch]], Map[String, Team.CupTour])],
        plannedRaw: Map[Int, String],
        readyTeams: Int
    )(using Context) =
      val drawn = bracket.isDefined
      val nbRounds = bracket.fold(0)(_._1.size)
      // до жеребьёвки число кругов неизвестно: считаем по командам с игроками или по настройке «Команд»,
      // иначе 5 (1/16 … финал); пустые поля заполняем от «Первого круга» с шагом «Между кругами»
      val datesRounds =
        if drawn then nbRounds
        else
          val n = if readyTeams >= 2 then readyTeams else settings.teams
          if n >= 2 then 32 - Integer.numberOfLeadingZeros(n - 1) else 5
      val firstDate = settings.start.flatMap(d => scala.util.Try(java.time.LocalDateTime.parse(d)).toOption)
      val dateOf = (r: Int) =>
        plannedRaw
          .get(r)
          .orElse(firstDate.map(_.plusDays(r.toLong * settings.everyDays).toString))
          .getOrElse("")
      val url = routes.Team.edit(TeamId("kubok"))
      val confirmJs = (msg: String) => s"return confirm('$msg')"
      st.section(cls := "kl-cup__bracket-admin box", style := "margin:1em 0 2em;padding:1em")(
        h2("Плей-офф (видно только администраторам)"),
        postForm(action := url, style := "margin:.5em 0")(
          input(tpe := "hidden", name := "bracket", value := "draw"),
          label("Сила команды — средний рейтинг: ")(
            st.select(name := "perf")(
              cupPerfs.map((k, n) => st.option(value := k, (k == settings.perf).option(selected))(n))
            )
          ),
          " ",
          button(
            cls := "button",
            tpe := "submit",
            attr("onclick") := confirmJs(
              if drawn then "Провести жеребьёвку заново? Текущая сетка и бои пропадут."
              else "Провести жеребьёвку?"
            )
          )("Провести жеребьёвку")
        ),
        postForm(action := url, style := "margin:1em 0")(
          input(tpe := "hidden", name := "bracket", value := "all"),
          h3("Общий бой всех команд"),
          p(
            "Одна арена на все команды с игроками. Сокомандники не встречаются, ",
            "соперник — из любой другой команды. Сетку плей-офф бой не меняет. ",
            "Бой выбывших и товарищеский бой команд без пары площадка создаёт сама при старте круга. ",
            "Эта форма нужна только для отдельного боя вне расписания."
          ),
          div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center")(
            label("Кто играет: ")(
              st.select(name := "who")(
                st.option(value := "all")("все команды"),
                st.option(value := "out")("только выбывшие из плей-офф")
              )
            ),
            label("Начало боя (МСК): ")(input(tpe := "datetime-local", name := "date")),
            label("Контроль, мин: ")(
              st.select(name := "clock")(
                cupClocks.map(v => st.option(value := v, (v == settings.clock).option(selected))(v))
              )
            ),
            label("+ сек: ")(
              input(
                tpe := "number",
                name := "inc",
                value := settings.inc.toString,
                min := "0",
                max := "60",
                style := "width:4em"
              )
            ),
            label("Длится, мин: ")(
              input(
                tpe := "number",
                name := "minutes",
                value := settings.minutes.toString,
                min := "10",
                max := "360",
                style := "width:5em"
              )
            ),
            label("В зачёт лучших: ")(
              input(
                tpe := "number",
                name := "leaders",
                value := settings.leaders.toString,
                min := "1",
                max := "5",
                style := "width:4em"
              )
            ),
            label(
              input(tpe := "checkbox", name := "rated", value := "true", settings.rated.option(checked)),
              " рейтинговые"
            )
          ),
          p(
            small(
              "Контроль, длительность и зачёт взяты из настроек кубка, их можно поменять только для этого боя. ",
              "Пустое поле «Начало боя» — бой начнётся через 5 минут. В таблице боя видны 10 лучших команд, остальные — по ссылке."
            )
          ),
          button(
            cls := "button",
            tpe := "submit",
            attr("onclick") := confirmJs("Создать бой? Кто играет — по полю «Кто играет».")
          )("Создать общий бой")
        ),
        (datesRounds > 0).option(
          postForm(action := url, style := "margin:1em 0")(
            input(tpe := "hidden", name := "bracket", value := "dates"),
            h3("Даты кругов"),
            p(
              small(
                "Дата видна всем в расписании и в сетке. В это время площадка сама начнёт круг. ",
                (!drawn).option(
                  frag(
                    "Жеребьёвки ещё не было: число кругов посчитано по командам, точным оно станет после жеребьёвки. ",
                    "Если команд окажется меньше, лишние даты с конца не понадобятся. ",
                    "Дата первого круга — это и старт кубка: команды соберутся к ней."
                  )
                )
              )
            ),
            div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center")(
              (0 until datesRounds).map: r =>
                label(s"${Team.cupRoundName(r, datesRounds)} (МСК): ")(
                  input(tpe := "datetime-local", name := s"d$r", value := dateOf(r))
                )
            ),
            p(button(cls := "button", tpe := "submit")("Сохранить даты"))
          )
        ),
        drawn.option(
          frag(
            postForm(action := url)(
              input(tpe := "hidden", name := "bracket", value := "reset"),
              button(
                cls := "button button-red button-empty",
                tpe := "submit",
                attr("onclick") := confirmJs("Сбросить сетку? Созданные бои останутся, но выпадут из сетки.")
              )("Сбросить сетку")
            )
          )
        )
      )

    // «Логик»: автосбор команд кубка из тех, кто сейчас на площадке (только админам).
    def cupDraft(
        perf: String,
        size: Int,
        teams: Int,
        existing: Int,
        cupTeams: List[(TeamId, String, Int)],
        onlineOnly: Boolean,
        players: List[(UserId, Int, Boolean, Option[String])],
        current: Map[UserId, String],
        isOnline: UserId => Boolean,
        picked: Set[UserId],
        preview: Option[List[List[(UserId, Int)]]],
        rsvp: Map[UserId, Boolean],
        locked: Boolean = false
    )(using Context) =
      val url = routes.Team.edit(TeamId("kubok"))
      val confirmed = (u: UserId) => rsvp.get(u).contains(true)
      val groupOf = players.collect { case (u, _, _, Some(g)) => u -> g }.toMap
      val grp = (u: UserId) => groupOf.get(u).fold("")(g => s" · гр. $g")
      val isChecked = (u: UserId, staff: Boolean) =>
        if picked.isEmpty && preview.isEmpty then (if rsvp.nonEmpty then confirmed(u) else !staff)
        else picked(u)
      val settings = (extra: Frag) =>
        div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center;margin:1em 0")(
          label("Команд: ")(
            input(
              tpe := "number",
              name := "teams",
              value := teams.toString,
              min := "0",
              max := "64",
              style := "width:4em"
            )
          ),
          label("Состав: ")(
            st.select(name := "size")(
              st.option(value := Team.cupMaxMembers.toString, (size != 0).option(selected))(
                s"по рейтингу, до ${Team.cupMaxMembers} в команде"
              ),
              st.option(value := "0", (size == 0).option(selected))("по одному из групп A–E")
            )
          ),
          label("Рейтинг: ")(
            st.select(name := "perf")(
              List("blitz" -> "блиц", "rapid" -> "рапид", "classical" -> "классика").map: (k, n) =>
                st.option(value := k, (k == perf).option(selected))(n)
            )
          ),
          extra
        )
      // «Логик»: опоздавшие — ученики не в командах, без пересборки нынешних составов.
      val lateBlock: Frag =
        players.filter(p => !p._3 && !current.contains(p._1)) match
          case late if late.isEmpty || existing == 0 || preview.isDefined => emptyFrag
          case late =>
            postForm(
              action := url,
              style := "border:1px solid #c9a227;border-radius:6px;padding:1em;margin:1em 0",
              attr(
                "onsubmit"
              ) := "return confirm('Добавить отмеченных в команды? Нынешние составы не изменятся.')"
            )(
              h2(s"Опоздавшие: не в команде ${late.size}"),
              p(
                "Кнопка добавит отмеченных, не трогая нынешние составы. Сначала они займут свободные места ",
                s"(до ${Team.cupMaxMembers} в команде), остальные соберутся в новые команды. ",
                "Новые команды сами встанут в общий бой кубка, если он ещё не закончился, но не в сетку плей-офф. ",
                "Отмечены те, кто сейчас на площадке."
              ),
              input(tpe := "hidden", name := "late", value := "true"),
              input(tpe := "hidden", name := "perf", value := perf),
              table(cls := "slist")(
                thead(tr(th(), th("Игрок"), th("Рейтинг"), th("Группа"), th("На площадке"))),
                tbody(
                  late.map: (u, r, _, g) =>
                    tr(
                      td(
                        input(
                          tpe := "checkbox",
                          name := "l[]",
                          value := u.value,
                          id := s"cup-l-${u.value}",
                          isOnline(u).option(checked)
                        )
                      ),
                      td(label(`for` := s"cup-l-${u.value}")(userIdLink(u.some))),
                      td(r.toString),
                      td(g.fold("—")(x => s"гр. $x")),
                      td(if isOnline(u) then "да" else "—")
                    )
                )
              ),
              p(style := "margin-top:1em")(
                submitButton(cls := "button")("Добавить опоздавших")
              )
            )

      // «Логик»: ручная перестановка одного ученика, остальные составы не меняются.
      val moveBlock: Frag =
        if existing == 0 || preview.isDefined then emptyFrag
        else
          postForm(
            action := url,
            style := "border:1px solid #888;border-radius:6px;padding:1em;margin:1em 0",
            attr("onsubmit") := "return confirm('Переставить ученика? Остальные составы не изменятся.')"
          )(
            h2("Переставить ученика"),
            p(
              "Выберите ученика и команду. Он уйдёт из своей команды и встанет в выбранную ",
              s"(в команде не больше ${Team.cupMaxMembers} игроков). Если уходит капитан, капитаном станет другой игрок. ",
              "Новая команда попадёт в сетку при старте первого круга. ",
              "Пока идёт круг, составы закрыты."
            ),
            input(tpe := "hidden", name := "move", value := "true"),
            div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center")(
              label("Ученик: ")(
                st.select(name := "u", required)(
                  st.option(value := "")("— выберите —"),
                  players
                    .filterNot(_._3)
                    .sortBy(_._1.value)
                    .map: (u, r, _, _) =>
                      st.option(value := u.value)(
                        s"${usernameOrId(u)} ($r) — ${current.getOrElse(u, "без команды")}"
                      )
                )
              ),
              label("Куда: ")(
                st.select(name := "to", required)(
                  st.option(value := "")("— выберите —"),
                  cupTeams.map: (id, name, n) =>
                    st.option(value := id.value, (n >= Team.cupMaxMembers).option(disabled))(
                      s"$name ($n из ${Team.cupMaxMembers})"
                    ),
                  st.option(value := "new")("Новая команда"),
                  st.option(value := "none")("Убрать из команды")
                )
              ),
              submitButton(cls := "button")("Переставить")
            )
          )

      TeamPage("Кубок Логика — составы команд"):
        main(cls := "team-list page-menu")(
          menu("kubok".some),
          div(cls := "page-menu__content box box-pad")(
            h1(cls := "box__top")("Составы команд кубка"),
            p(
              "Здесь все ученики школы на площадке. Отметьте тех, кто играет, и нажмите «Показать составы». ",
              "Сильнейший в каждой команде станет капитаном."
            ),
            p(
              strong("Важно: "),
              s"при сохранении нынешние составы всех команд кубка заменятся новыми. Сейчас команд: $existing. ",
              "Так можно переиграть составы до старта круга, например когда после анонса придут новые игроки. ",
              "Пока идёт круг, составы закрыты."
            ),
            locked.option(
              p(cls := "flash flash-failure", style := "padding:.8em 1em;margin:1em 0")(
                "Круг идёт, составы закрыты: изменения не сохранятся. Чтобы изменить составы, нажмите на пульте кубка «Остановить круг»."
              )
            ),
            moveBlock,
            lateBlock,
            p(
              if onlineOnly then a(href := s"$url?perf=$perf")("Показать всех учеников")
              else a(href := s"$url?perf=$perf&online=1")("Показать только тех, кто сейчас на площадке")
            ),
            postForm(action := url)(
              input(tpe := "hidden", name := "online", value := onlineOnly.toString),
              settings(emptyFrag),
              p(
                small(
                  s"Команд 0 — столько, сколько нужно, чтобы сыграли все отмеченные (по ${Team.cupMaxMembers} в команде). Кто не поместился, станет запасным."
                )
              ),
              if players.isEmpty then p("Учеников не найдено.")
              else
                frag(
                  p(
                    s"Учеников: ${players.size}, сейчас на площадке: ${players.count(p => isOnline(p._1))}. По группам: ",
                    (Team.cupGroups.map(g => s"$g — ${players.count(_._4.contains(g))}") :+
                      s"без группы — ${players.count(_._4.isEmpty)}").mkString(", ")
                  ),
                  rsvp.nonEmpty.option(
                    p(
                      strong(s"Подтвердили в боте: ${players.count(p => confirmed(p._1))}"),
                      s", отказались: ${players.count(p => rsvp.get(p._1).contains(false))}, ",
                      s"не ответили: ${players.count(p => !rsvp.contains(p._1))}. ",
                      "Подтвердившие стоят в начале списка и отмечены сразу."
                    )
                  ),
                  p(
                    button(
                      cls := "button button-empty",
                      tpe := "button",
                      attr("onclick") := "document.querySelectorAll('.kl-cup-u').forEach(c=>c.checked=true)"
                    )("Отметить всех"),
                    " ",
                    button(
                      cls := "button button-empty",
                      tpe := "button",
                      attr("onclick") := "document.querySelectorAll('.kl-cup-u').forEach(c=>c.checked=false)"
                    )("Снять все"),
                    rsvp.nonEmpty.option(
                      frag(
                        " ",
                        button(
                          cls := "button button-empty",
                          tpe := "button",
                          attr("onclick") :=
                            "document.querySelectorAll('.kl-cup-u').forEach(c=>c.checked=c.dataset.rsvp==='y')"
                        )("Только подтвердившие")
                      )
                    )
                  ),
                  table(cls := "slist")(
                    thead(
                      tr(
                        th(),
                        th("Игрок"),
                        th("Рейтинг"),
                        th("Группа"),
                        th("Подтвердил"),
                        th("Сейчас в команде"),
                        th()
                      )
                    ),
                    tbody(
                      players.map: (u, r, staff, g) =>
                        tr(
                          td(
                            input(
                              tpe := "checkbox",
                              cls := "kl-cup-u",
                              name := "u[]",
                              value := u.value,
                              id := s"cup-u-${u.value}",
                              attr("data-rsvp") := rsvp.get(u).fold("")(b => if b then "y" else "n"),
                              isChecked(u, staff).option(checked)
                            )
                          ),
                          td(label(`for` := s"cup-u-${u.value}")(userIdLink(u.some))),
                          td(r.toString),
                          td(g.fold("—")(x => s"гр. $x")),
                          td(rsvp.get(u).fold("—")(b => if b then "да" else "нет")),
                          td(current.getOrElse(u, "—")),
                          td(staff.option("тренер / админ"))
                        )
                    )
                  ),
                  p(style := "margin-top:1em")(
                    button(cls := "button", tpe := "submit", name := "go", value := "false")(
                      "Показать составы"
                    )
                  )
                )
            ),
            preview.map: draft =>
              val inTeams = draft.flatten.map(_._1).toSet
              val reserve = picked.filterNot(inTeams)
              st.section(style := "margin-top:2em")(
                h2("Новые составы"),
                if draft.isEmpty then
                  p(
                    if size == 0 then "Нет отмеченных игроков с группой A–E — команду не собрать."
                    else "Отмечено слишком мало игроков — команду не собрать."
                  )
                else
                  postForm(action := url)(
                    input(tpe := "hidden", name := "go", value := "true"),
                    draft.zipWithIndex.map: (t, i) =>
                      div(style := "margin:1em 0")(
                        h3(
                          s"Команда ${i + 1}: ${t.size} из ${Team.cupMaxMembers}, средний рейтинг ${t.map(_._2).sum / t.size.atLeast(1)}"
                        ),
                        ol(t.zipWithIndex.map { case ((u, r), j) =>
                          li(
                            userIdLink(u.some, withOnline = false),
                            s" — $r${grp(u)}",
                            current.get(u).map(c => s" (был: $c)"),
                            (j == 0).option(" — капитан")
                          )
                        }),
                        input(tpe := "hidden", name := "t[]", value := t.map(_._1.value).mkString(","))
                      ),
                    reserve.nonEmpty.option(
                      p(s"Не попали в команды (запасные): ", reserve.toList.map(_.value).mkString(", "))
                    ),
                    (reserve.nonEmpty && size != 0).option:
                      val need = (picked.size + Team.cupMaxMembers - 1) / Team.cupMaxMembers
                      p(
                        strong(s"Чтобы сыграли все отмеченные (${picked.size}), нужно команд: $need. "),
                        "Поставьте это число в поле «Команд» (или 0) и снова нажмите «Показать составы»."
                      )
                    ,
                    p(
                      s"Составы лягут в команды по порядку номеров. ",
                      if draft.size > existing then
                        s"Не хватает команд: ${draft.size - existing}, они создадутся сами. "
                      else if draft.size < existing then
                        s"Команд больше, чем составов: ${existing - draft.size} останутся пустыми и в сетку не попадут. "
                      else "",
                      "Названия команд сохранятся. После этого проведите жеребьёвку заново."
                    ),
                    button(
                      cls := "button button-green",
                      tpe := "submit",
                      attr("onclick") := "return confirm('Заменить составы всех команд кубка?')"
                    )("Сохранить составы")
                  )
              )
          )
        )

    private def noTeam()(using Context) =
      tbody(tr(td(colspan := "2")(br, trt.noTeamFound())))

  def members(team: Team, members: Paginator[lila.core.LightUser])(using Translate) =
    div(cls := "team-show__members")(
      st.section(cls := "recent-members")(
        h2(a(href := routes.Team.members(team.slug))(trt.teamRecentMembers())),
        div(cls := "userlist infinite-scroll")(
          members.currentPageResults.map: member =>
            div(cls := "paginated")(lightUserLink(member)),
          pagerNext(members, np => routes.Team.show(team.id, np).url)
        )
      )
    )

  def actions(
      team: Team,
      member: Option[TeamMember],
      myRequest: Option[TeamRequest],
      asMod: Boolean
  )(using ctx: Context) =
    def hasPerm(perm: TeamSecurity.Permission.Selector) = member.exists(_.hasPerm(perm))
    val cupAdmin = team.isCup && Granter.opt(_.ManageTeam)
    val canManage = (asMod || cupAdmin) && Granter.opt(_.ManageTeam)
    div(cls := "team-show__actions")(
      // «Логик»: составы кубка правит только администратор на странице «Составы»
      cupAdmin.option(
        a(href := routes.Team.edit(TeamId("kubok")), cls := "button button-empty")("Править составы кубка")
      ),
      (team.enabled && team.isCup && member.isEmpty && !cupAdmin).option(
        strong("Составы команд кубка набирает тренер. Чтобы попасть в команду, напиши тренеру.")
      ),
      (team.enabled && team.acceptsMembers && member.isEmpty && !team.isCup).option(
        frag(
          if myRequest.exists(_.declined) then
            frag(
              strong(trt.requestDeclined()),
              a(cls := "button disabled button-metal")(trt.joinTeam())
            )
          else if myRequest.isDefined then
            frag(
              strong(trt.beingReviewed()),
              postForm(action := routes.Team.quit(team.id)):
                submitButton(cls := "button button-red button-empty yes-no-confirm")(trans.site.cancel())
            )
          else (ctx.isAuth && !asMod && !cupAdmin).option(joinButton(team))
        )
      ),
      (member.isDefined && !team.isClas && !hasPerm(_.Admin) && !team.isCup).option(
        postForm(cls := "quit", action := routes.Team.quit(team.id))(
          submitButton(cls := "button button-empty button-red yes-no-confirm")(trt.quitTeam.txt())
        )
      ),
      (team.enabled && (hasPerm(_.Tour) || team.isCup) && Granter.opt(_.Admin)).option(
        frag(
          team.isClas.not.option:
            a(
              href := routes.Tournament.teamBattleForm(team.id),
              cls := "button button-empty text",
              dataIcon := Icon.Trophy
            ):
              span(
                strong(trt.teamBattle()),
                em(trt.teamBattleOverview())
              )
          ,
          a(
            href := addQueryParams(
              routes.Tournament.form.url,
              Map("team" -> team.id.value) ++ team.isClas.so(Map("clas" -> "1"))
            ),
            cls := "button button-empty text",
            dataIcon := Icon.Trophy
          ):
            span(
              strong(trt.teamTournament()),
              em(trt.teamTournamentOverview())
            )
          ,
          a(
            href := s"${routes.Swiss.form(team.id)}",
            cls := "button button-empty text",
            dataIcon := Icon.Trophy
          ):
            span(
              strong(trans.swiss.swissTournaments()),
              em(trt.swissTournamentOverview())
            )
        )
      ),
      (team.enabled && hasPerm(_.PmAll)).option(
        frag(
          a(
            href := routes.Team.updateNew(team.id),
            cls := "button button-empty text",
            dataIcon := Icon.InkQuill
          )(trt.newTeamUpdate())
        )
      ),
      (team.enabled && team.isCup && (hasPerm(_.Settings) || canManage)).option(
        postForm(action := routes.Team.update(team.id))(
          label(strong("Название команды")),
          input(
            name := "cupRename",
            value := team.name,
            required,
            minlength := 3,
            maxlength := 40,
            st.style := "width:100%;margin:.4em 0"
          ),
          submitButton(cls := "button")("Переименовать")
        )
      ),
      ((team.enabled && hasPerm(_.Settings)) || canManage).option(
        a(
          href := routes.Team.edit(team.id),
          cls := "button button-empty text",
          dataIcon := Icon.Gear
        )(trans.settings.settings())
      ),
      ((team.enabled && hasPerm(_.Admin)) || canManage).option(
        a(
          cls := "button button-empty text",
          href := routes.Team.leaders(team.id),
          dataIcon := Icon.Group
        )(trt.teamLeaders())
      ),
      ((team.enabled && hasPerm(_.Kick)) || canManage).option(
        a(
          cls := "button button-empty text",
          href := routes.Team.kick(team.id),
          dataIcon := Icon.InternalArrow
        )(trt.kickSomeone())
      ),
      ((team.enabled && hasPerm(_.Request)) || canManage).option(
        a(
          cls := "button button-empty text",
          href := routes.Team.declinedRequests(team.id),
          dataIcon := Icon.Cancel
        )(trt.declinedRequests())
      ),
      ((Granter.opt(_.ManageTeam) || Granter.opt(_.Shusher)) && !asMod).option(
        a(
          href := routes.Team.show(team.id, 1, mod = true),
          cls := "button button-red"
        )("View team as Mod")
      )
    )

  // handle special teams here
  private def joinButton(t: Team)(using Context) =
    t.id.value match
      case "english-chess-players" => joinAt("https://ecf.octoknight.com/")
      case "ecf" => joinAt(routes.Team.show(TeamId("english-chess-players")).url)
      case _ =>
        postForm(cls := "inline", action := routes.Team.join(t.id))(
          submitButton(cls := "button button-green")(trt.joinTeam())
        )

  private def joinAt(url: String)(using Context) =
    a(cls := "button button-green", href := url)(trt.joinTeam())
