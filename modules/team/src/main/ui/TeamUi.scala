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
        teams: List[(Team, List[UserId], Set[UserId])],
        rules: Option[Html],
        editRules: Option[String],
        bracket: Option[(List[List[Team.CupMatch]], Map[String, Team.CupTour])],
        planned: Map[Int, Instant],
        plannedRaw: Map[Int, String]
    )(using Context) =
      // у админа на доске формы плей-офф: meta refresh стирал введённые даты до нажатия кнопки,
      // поэтому админу — перезагрузка скриптом, которая ждёт, пока поля панели не тронуты
      val admin = Granter.opt(_.ManageTeam)
      val reloadJs =
        """var t=0,p='.kl-cup__bracket-admin',m=function(e){if(e.target.closest&&e.target.closest(p))t=1};""" +
          """document.addEventListener('input',m,true);document.addEventListener('change',m,true);""" +
          """setInterval(function(){var a=document.activeElement;""" +
          """if(!t&&!(a&&a.closest&&a.closest(p)))location.reload()},30000)"""
      val page = TeamPage("Кубок Логика — команды")
      (if admin then page.js(Esm("bits", embedJsUnsafeLoadThen(reloadJs))) else page).apply:
        main(cls := "team-list page-menu")(
          menu("kubok".some),
          div(cls := "page-menu__content box box-pad")(
            (!admin).option(meta(httpEquiv := "refresh", content := "30")),
            h1(cls := "box__top")("Кубок Логика — команды"),
            p(
              s"Команд: ${teams.size}, игроков: ${teams.map(_._2.size).sum}. ",
              "Страница обновляется сама каждые 30 секунд.",
              admin.option(" Пока вы заполняете поля плей-офф, обновление ждёт.")
            ),
            rules.map(r =>
              st.section(cls := "kl-cup__rules", style := "margin:1em 0 2em")(h2("Регламент"), r)
            ),
            editRules.map(u =>
              p(a(cls := "button button-empty", href := u)(if rules.isDefined then "Изменить регламент"
              else "Добавить регламент"))
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
            Granter.opt(_.ManageTeam).option(cupBracketAdmin(bracket, plannedRaw)),
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
        bracket: Option[(List[List[Team.CupMatch]], Map[String, Team.CupTour])],
        plannedRaw: Map[Int, String]
    )(using Context) =
      val drawn = bracket.isDefined
      val nbRounds = bracket.fold(0)(_._1.size)
      // пары, для которых «Создать бои круга» создаст бой сейчас
      val ready = bracket.so: (rounds, tours) =>
        rounds.flatten.filter(m =>
          m.pair.isDefined && m.winner.isEmpty && tours.get(m.key).forall(_.finished)
        )
      val readyRounds = ready.map(_.round).distinct.sorted.map(Team.cupRoundName(_, nbRounds))
      val url = routes.Team.edit(TeamId("kubok"))
      val confirmJs = (msg: String) => s"return confirm('$msg')"
      st.section(cls := "kl-cup__bracket-admin box", style := "margin:1em 0 2em;padding:1em")(
        h2("Плей-офф (видно только администраторам)"),
        postForm(action := url, style := "margin:.5em 0")(
          input(tpe := "hidden", name := "bracket", value := "draw"),
          label("Сила команды — средний рейтинг: ")(
            st.select(name := "perf")(
              List("rapid" -> "рапид", "blitz" -> "блиц", "classical" -> "классика").map((k, n) =>
                st.option(value := k)(n)
              )
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
            "соперник — из любой другой команды. Сетку плей-офф бой не меняет."
          ),
          div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center")(
            label("Начало (МСК): ")(input(tpe := "datetime-local", name := "date")),
            label("Контроль, мин: ")(
              st.select(name := "clock")(
                List("3", "5", "10", "15", "20", "25", "30").map(v =>
                  st.option(value := v, (v == "5").option(selected))(v)
                )
              )
            ),
            label("+ сек: ")(
              input(
                tpe := "number",
                name := "inc",
                value := "3",
                min := "0",
                max := "60",
                style := "width:4em"
              )
            ),
            label("Длится, мин: ")(
              input(
                tpe := "number",
                name := "minutes",
                value := "60",
                min := "10",
                max := "360",
                style := "width:5em"
              )
            ),
            label("В зачёт лучших: ")(
              input(
                tpe := "number",
                name := "leaders",
                value := "5",
                min := "1",
                max := "5",
                style := "width:4em"
              )
            ),
            label(input(tpe := "checkbox", name := "rated", value := "true", checked), " рейтинговые")
          ),
          p(
            small(
              "Пустое поле «Начало» — бой начнётся через 5 минут. В таблице боя видны 10 лучших команд, остальные — по ссылке."
            )
          ),
          button(
            cls := "button",
            tpe := "submit",
            attr("onclick") := confirmJs("Создать общий бой всех команд?")
          )("Создать общий бой")
        ),
        drawn.option(
          postForm(action := url, style := "margin:1em 0")(
            input(tpe := "hidden", name := "bracket", value := "dates"),
            h3("Даты кругов"),
            p(small("Дата видна всем в сетке. Бои круга создаются кнопкой ниже и начнутся в эту дату.")),
            div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center")(
              (0 until nbRounds).map: r =>
                label(s"${Team.cupRoundName(r, nbRounds)} (МСК): ")(
                  input(tpe := "datetime-local", name := s"d$r", value := plannedRaw.getOrElse(r, ""))
                )
            ),
            p(button(cls := "button", tpe := "submit")("Сохранить даты"))
          )
        ),
        drawn.option(
          frag(
            postForm(action := url, style := "margin:1em 0")(
              input(tpe := "hidden", name := "bracket", value := "round"),
              h3("Бои круга"),
              p(
                if ready.isEmpty then "Сейчас нет пар, готовых к игре: ждём итогов прошлого круга."
                else s"Будут созданы бои: ${readyRounds.mkString(", ")}, пар — ${ready.size}."
              ),
              div(style := "display:flex;gap:1em;flex-wrap:wrap;align-items:center")(
                label("Начало (МСК): ")(input(tpe := "datetime-local", name := "date")),
                label("Контроль, мин: ")(
                  st.select(name := "clock")(
                    List("3", "5", "10", "15", "20", "25", "30").map(v =>
                      st.option(value := v, (v == "5").option(selected))(v)
                    )
                  )
                ),
                label("+ сек: ")(
                  input(
                    tpe := "number",
                    name := "inc",
                    value := "3",
                    min := "0",
                    max := "60",
                    style := "width:4em"
                  )
                ),
                label("Длится, мин: ")(
                  input(
                    tpe := "number",
                    name := "minutes",
                    value := "40",
                    min := "10",
                    max := "360",
                    style := "width:5em"
                  )
                ),
                label("В зачёт лучших: ")(
                  input(
                    tpe := "number",
                    name := "leaders",
                    value := "2",
                    min := "1",
                    max := "5",
                    style := "width:4em"
                  )
                ),
                label(input(tpe := "checkbox", name := "rated", value := "true", checked), " рейтинговые")
              ),
              p(
                small(
                  "Пустое поле «Начало» — бой начнётся в дату круга из блока выше, а если её нет — через 5 минут. ",
                  "Победитель боя сам пройдёт в следующий круг."
                )
              ),
              button(cls := "button", tpe := "submit")("Создать бои круга")
            ),
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
        onlineOnly: Boolean,
        players: List[(UserId, Int, Boolean, Option[String])],
        current: Map[UserId, String],
        isOnline: UserId => Boolean,
        picked: Set[UserId],
        preview: Option[List[List[(UserId, Int)]]]
    )(using Context) =
      val url = routes.Team.edit(TeamId("kubok"))
      val groupOf = players.collect { case (u, _, _, Some(g)) => u -> g }.toMap
      val grp = (u: UserId) => groupOf.get(u).fold("")(g => s" · гр. $g")
      val isChecked = (u: UserId, staff: Boolean) =>
        if picked.isEmpty && preview.isEmpty then !staff else picked(u)
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
              "Так можно переиграть составы в любой момент, например когда после анонса придут новые игроки. ",
              "Во время идущего боя составы не меняйте."
            ),
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
                    )("Снять все")
                  ),
                  table(cls := "slist")(
                    thead(tr(th(), th("Игрок"), th("Рейтинг"), th("Группа"), th("Сейчас в команде"), th())),
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
                              isChecked(u, staff).option(checked)
                            )
                          ),
                          td(label(`for` := s"cup-u-${u.value}")(userIdLink(u.some))),
                          td(r.toString),
                          td(g.fold("—")(x => s"гр. $x")),
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
      (cupAdmin && member.isEmpty).option(
        div(
          p(
            "Ссылка для капитана и игроков команды. Отправьте её, ученик откроет и нажмёт «Вступить в клуб»:"
          ),
          copyMeInput(routeUrl(routes.Team.show(team.id)).value),
          p("Сами в команду не вступайте: первый вступивший становится капитаном.")
        )
      ),
      (team.enabled && team.isCupFull && member.isEmpty).option(
        strong(s"Команда набрана: ${Team.cupMaxMembers} из ${Team.cupMaxMembers}")
      ),
      (team.enabled && team.acceptsMembers && !team.isCupFull && member.isEmpty).option(
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
      (member.isDefined && !team.isClas && !hasPerm(_.Admin)).option(
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
