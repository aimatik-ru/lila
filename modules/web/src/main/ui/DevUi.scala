package lila.web
package ui

import play.api.data.Form

import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }
import lila.common.RawHtml.nl2br

final class DevUi(helpers: Helpers)(modMenu: String => Context ?=> Frag):
  import helpers.*

  def settings(settings: List[(String, List[lila.memo.SettingStore[?]])])(using Context) =
    val title = "Settings"
    Page(title).css("mod.misc"):
      main(cls := "page-menu")(
        modMenu("setting"),
        div(id := "settings", cls := "page-menu__content box box-pad")(
          h1(cls := "box__top")(title),
          settings.map: (group, list) =>
            form3.fieldset(group, false.some):
              div: // necessary for styling
                div: // necessary for styling
                  list.map: s =>
                    postForm(action := routes.Dev.settingsPost(s.id))(
                      label(`for` := "v")(s.text | s.id),
                      s.form.value match
                        case Some(v: Boolean) => form3.nativeCheckbox(s.id, "v", v)
                        case Some(v: lila.core.data.Text) => textarea(name := "v")(v.value)
                        case v => input(name := "v", value := v.map(_.toString))
                      ,
                      submitButton(cls := "button button-empty", dataIcon := Icon.Checkmark)
                    )
        )
      )

  def logicColors(bg: String, primary: String, weekly: String)(using Context) =
    val title = "Настройки площадки"
    def picker(field: String, text: String, current: String, default: String) =
      div(cls := "form-group")(
        label(cls := "form-label", `for` := s"logic-$field")(text),
        input(
          tpe := "color",
          id := s"logic-$field",
          name := field,
          value := (LogicColors.clean(current) | default)
        )
      )
    Page(title)
      .css("mod.misc")
      .css("bits.form3"):
        main(cls := "page-menu")(
          modMenu("logic-colors"),
          div(cls := "page-menu__content box box-pad")(
            h1(cls := "box__top")(title),
            p("После сохранения цвета сменятся на всех страницах. Тёмная тема берёт оттенок от цвета фона."),
            p(a(href := s"${routes.Dev.logicColors}?traps=1")("Ловушки в дебюте →")),
            p(a(href := s"${routes.Dev.logicColors}?marks=1")("Метки «читер» →")),
            p(a(href := "#weekly")("Турниры каждую неделю ↓")),
            postForm(cls := "form3", action := routes.Dev.logicColorsPost)(
              picker("bg", "Фон страницы", bg, LogicColors.defaultBg),
              picker("primary", "Кнопки и ссылки", primary, LogicColors.defaultPrimary),
              div(cls := "form-actions")(
                submitButton(cls := "button")("Сохранить цвета"),
                submitButton(cls := "button button-empty", name := "reset", value := "1")("Вернуть голубые")
              )
            ),
            h2(id := "weekly")("Турниры каждую неделю"),
            p(
              "Блок на главной. Одна строка — один турнир, сначала день и время: ",
              em("Сб. 16:30 швейцарка, 10+2, 7 туров"),
              ". Пустое поле убирает блок."
            ),
            postForm(cls := "form3", action := s"${routes.Dev.logicColorsPost}?weekly=1")(
              div(cls := "form-group")(
                textarea(
                  name := "weekly",
                  rows := 6,
                  st.style := "width:100%;box-sizing:border-box"
                )(weekly)
              ),
              div(cls := "form-actions")(submitButton(cls := "button")("Сохранить памятку"))
            )
          )
        )

  def logicMarks(rows: List[(String, Option[java.time.Instant], String)])(using Context) =
    val fmt = java.time.format.DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm")
    def date(at: java.time.Instant) = at.atZone(java.time.ZoneId.of("Europe/Moscow")).format(fmt)
    val postUrl = (op: String) => s"${routes.Dev.logicColorsPost}?marks=1&op=$op"
    val title = "Метки «читер»"
    Page(title)
      .css("mod.misc")
      .css("bits.form3"):
        main(cls := "page-menu")(
          modMenu("logic-colors"),
          div(cls := "page-menu__content box box-pad", st.style := "min-width:0;overflow-wrap:anywhere")(
            h1(cls := "box__top")(title),
            standardFlash,
            p(
              "Площадка сама ставит метку, если ходы игрока слишком часто совпадают с компьютером. С меткой игрок не попадает в турниры, а его партии не меняют рейтинг соперников."
            ),
            p(
              "Снятие не отключает автопроверку: если игрок снова будет играть с подсказкой, метка вернётся. В профиле игрока остаётся заметка, кто и когда снял или поставил метку."
            ),
            postForm(cls := "form3", action := postUrl("set"))(
              div(cls := "form-group")(
                label(cls := "form-label", `for` := "logic-mark-user")("Поставить метку вручную: ник игрока"),
                input(id := "logic-mark-user", name := "user", required, st.placeholder := "Ivan_Ivanov_2014")
              ),
              div(cls := "form-actions")(
                a(href := routes.Dev.logicColors)("← Настройки площадки"),
                submitButton(
                  cls := "button button-red yes-no-confirm",
                  st.title := "Поставить метку «читер»? Игрок не сможет играть в турнирах."
                )("Поставить метку")
              )
            ),
            h2(s"Сейчас помечены: ${rows.size}"),
            if rows.isEmpty then p(em("Никого нет."))
            else
              table(cls := "slist")(
                thead(tr(th("Игрок"), th("Когда"), th("Кто поставил"), th)),
                tbody(rows.map: (nick, at, by) =>
                  tr(
                    td(a(href := s"/@/$nick")(nick)),
                    td(at.fold("—")(date)),
                    td(if by.nonEmpty then by else "—"),
                    td(
                      postForm(action := postUrl("unset"))(
                        input(tpe := "hidden", name := "user", value := nick),
                        submitButton(
                          cls := "button button-thin yes-no-confirm",
                          st.title := s"Снять метку с $nick?"
                        )("Снять метку")
                      )
                    )
                  ))
              )
          )
        )

  def logicTraps(text: String, parsed: LogicTraps.Parsed, caught: List[List[LogicTraps.Caught]] = Nil)(using
      Context
  ) =
    val day = java.time.format.DateTimeFormatter.ofPattern("dd.MM.yyyy")
    def date(at: Long) =
      java.time.Instant.ofEpochMilli(at).atZone(java.time.ZoneId.of("Europe/Moscow")).format(day)
    def side(color: String) = if color == "white" then "белыми" else "чёрными"
    // Номер опасного хода в партии: ссылка открывает позицию сразу после него.
    val plies = parsed.traps.map(t => LogicTraps.sans(t.line).size)
    def gameUrl(g: LogicTraps.Caught, ply: Int) = s"/${g.gameId}/${g.color}#${g.ply.getOrElse(ply)}"
    val all = caught.zipWithIndex
      .flatMap((gs, i) => gs.map(_ -> plies.lift(i).getOrElse(0)))
      .distinctBy(_._1.gameId)
    val often = all
      .groupBy(_._1.player)
      .toList
      .map((u, gs) => (u, gs.sortBy(-_._1.at)))
      .sortBy((u, gs) => (-gs.size, u.toLowerCase))
    val title = "Ловушки в дебюте"
    // «Поделиться»: на телефоне системное меню (MAX, Telegram…), иначе копируем ссылку.
    val shareJs = """document.addEventListener('click',e=>{const b=e.target.closest('.logic-share');if(!b)return;
const url=location.origin+b.dataset.href,text=b.dataset.text;
const copied=()=>{b.textContent='Ссылка скопирована';setTimeout(()=>b.textContent='Поделиться',2500)};
if(navigator.share)navigator.share({title:text,text:text,url:url}).catch(()=>{});
else if(navigator.clipboard)navigator.clipboard.writeText(text+' '+url).then(copied,()=>prompt('Ссылка на партию',url));
else prompt('Ссылка на партию',url)})"""
    Page(title)
      .css("mod.misc")
      .css("bits.form3")
      .js(Esm("bits", embedJsUnsafeLoadThen(shareJs))):
        main(cls := "page-menu")(
          modMenu("logic-colors"),
          div(cls := "page-menu__content box box-pad", st.style := "min-width:0;overflow-wrap:anywhere")(
            h1(cls := "box__top")(title),
            p(
              "Одна строка — одна ловушка. Слева от черты ходы партии, последний из них опасный. Справа подсказка: ученик увидит её в окне перед этим ходом. Фигуры можно писать по-русски: Кр, Ф, Л, С, К. Время, пока окно открыто, игроку возвращается."
            ),
            p(
              "Допишите в конец строки «| запрет», и ход станет нельзя сделать: ученик увидит подсказку, а фигура вернётся на место. Без этой приписки ученик может подтвердить ход."
            ),
            p(
              "Ловушку проще ставить с доски: откройте ",
              a(href := "/analysis")("доску анализа"),
              ", сделайте ходы до опасного включительно и нажмите в меню «Ловушка на этот ход»."
            ),
            pre(st.style := "white-space:pre-wrap")(
              "1.e4 e5 2.Кf3 Кc6 3.Кc3 Кf6 4.Сc4 | На 4...К:e4 5.К:e4 у чёрных вилка 5...d5\n1.e4 e5 2.Кf3 Кc6 3.Кc3 Кf6 4.Сc4 | На 4...К:e4 5.К:e4 у чёрных вилка 5...d5 | запрет"
            ),
            parsed.errors.nonEmpty.option(
              div(cls := "flash flash-failure")(parsed.errors.map(e => div(e)))
            ),
            postForm(cls := "form3", action := s"${routes.Dev.logicColorsPost}?traps=1")(
              div(cls := "form-group")(
                textarea(
                  name := "traps",
                  rows := 12,
                  st.style := "width:100%;box-sizing:border-box;font-family:monospace"
                )(text)
              ),
              div(cls := "form-actions")(
                a(href := routes.Dev.logicColors)("← Настройки площадки"),
                submitButton(cls := "button")("Сохранить ловушки")
              )
            ),
            parsed.traps.nonEmpty.option(
              frag(
                h2(s"Сейчас действует: ${parsed.traps.size}"),
                p(
                  "Под каждой ловушкой — кто сделал опасный ход в партиях на площадке, свежие сверху. Учитываются законченные партии двух учеников, кроме партий с компьютером и со своей позиции."
                ),
                ul(parsed.traps.zipWithIndex.map: (t, i) =>
                  val games = caught.lift(i).getOrElse(Nil)
                  li(st.style := "margin-bottom:1em")(
                    strong(t.line),
                    t.block.option(" (запрет)"),
                    t.hint.nonEmpty.option(frag(" — ", t.hint)),
                    div(
                      if games.isEmpty then em("Пока никто не попался.")
                      else
                        frag(
                          em(s"Попались: ${games.size}"),
                          ul(
                            games
                              .take(50)
                              .map: g =>
                                li(
                                  date(g.at),
                                  " — ",
                                  a(href := s"/@/${g.player}")(g.player),
                                  s" (${side(g.color)}) против ",
                                  a(href := s"/@/${g.opponent}")(g.opponent),
                                  ": ",
                                  g.result,
                                  " · ",
                                  a(href := gameUrl(g, plies(i)))("открыть партию →"),
                                  " · ",
                                  button(
                                    tpe := "button",
                                    cls := "button button-empty button-thin logic-share",
                                    dataHref := gameUrl(g, plies(i)),
                                    attr(
                                      "data-text"
                                    ) := s"Партия ${g.player} против ${g.opponent}, ${date(g.at)}"
                                  )("Поделиться")
                                )
                          ),
                          (games.size > 50).option(p(s"…и ещё ${games.size - 50}"))
                        )
                    )
                  ))
              )
            ),
            often.nonEmpty.option(
              frag(
                h2("Кто чаще наступает на грабли"),
                table(cls := "slist")(
                  thead(tr(th("Ученик"), th("Партий с опасным ходом"), th("Партии"))),
                  tbody(
                    often
                      .take(50)
                      .map: (u, gs) =>
                        tr(
                          td(a(href := s"/@/$u")(u)),
                          td(gs.size),
                          td(
                            gs.zipWithIndex.map: (gp, k) =>
                              val (g, ply) = gp
                              frag(
                                (k > 0).option(", "),
                                a(
                                  href := gameUrl(g, ply),
                                  st.title := s"${date(g.at)}, ${side(g.color)}, ${g.result}"
                                )(
                                  date(g.at)
                                )
                              )
                          )
                        )
                  )
                )
              )
            )
          )
        )

  def ipTiers(form: Either[String, Form[?]])(using Context) =
    val title = "IP limit tiers"
    Page(title)
      .css("mod.misc")
      .css("bits.form3"):
        main(cls := "page-menu")(
          modMenu("ip-tiers"),
          div(id := "ip-tiers", cls := "page-menu__content box box-pad")(
            h1(cls := "box__top")(title),
            p(
              "Upgrade rate limits for specific IP addresses.",
              br,
              "Only necessary when more than 20 devices connect from the same IP at the same time.",
              br,
              "This requires a service to copy the lila file to the nginx server and reload nginx."
            ),
            p(
              "Format: ",
              br,
              code("{IP} {tier}; # contact info"),
              br,
              nl2br("""
  Tier 1: normal limits (default, up to 30 players)
  Tier 2: higher limits (well enough for schools and hotels)
  Tier 3: much higher limits (only for official bots like maia)
  """)
            ),
            standardFlash,
            postForm(action := routes.Dev.ipTiersPost, cls := "form3")(
              form match
                case Left(err) => p(cls := "error")(err)
                case Right(form) =>
                  val field = form("list")
                  frag(
                    div(cls := "form-group")(
                      form3.textarea(field)(spellcheck := "false"),
                      field.errors.map: err =>
                        p(cls := "error")(nl2br(err.message))
                    ),
                    br,
                    form3.submit(frag("Save and reload nginx"))
                  )
            )
          )
        )

  def cli(form: Form[?], res: Option[String])(using Context) =
    val title = "Command Line Interface"
    Page(title)
      .css("mod.misc")
      .css("bits.form3"):
        main(cls := "page-menu")(
          modMenu("cli"),
          div(id := "dev-cli", cls := "page-menu__content box box-pad")(
            h1(cls := "box__top")(title),
            p(
              "Run arbitrary lila commands.",
              br,
              "Only use if you know exactly what you're doing."
            ),
            res.map { pre(_) },
            postForm(action := routes.Dev.cliPost)(
              form3.input(form("command"))(autofocus),
              br,
              form3.submit(frag("Submit"))
            ),
            hr,
            postForm(action := routes.Dev.cliPost)(
              p("Same thing but with a textarea for multiline commands:"),
              form3.textarea(form("command"))(style := "height:8em"),
              br,
              form3.submit(frag("Submit"))
            ),
            h2("Command examples:"),
            pre(cliExamples)
          )
        )

  private val cliExamples = """uptime
announce 10 minutes Lichess will restart!
announce cancel
change asset version
fishnet client create {username}
msg multi {sender} {recipient1,recipient2} {message}
team members add {teamId} {username1,username2,username3}
notify url users {username1,username2,username3} {url} {link title} | {link description}
notify url titled {url} {link title} | {link description}
notify url titled-arena {url} {link title} | {link description}
patron lifetime {username}
patron gift-months {username} 3
patron remove {username}
patron set-months {username} {months}
tournament feature {id}
tournament unfeature {id}
payouts-dm {tournament or swiss url} {username1} {username2} {username3} {username...}
eval-cache drop standard 8/8/1k6/8/2K5/1P6/8/8 w - - 0 1
disposable test msumain.edu.ph
disposable reload msumain.edu.ph
test-email {primary | secondary} {email}
video sheet
puzzle issue {id} {longer-win | ambiguous | ...}
cache clear security.session.info
fide player sync
fide player rip 2026961 2025
fide player delete 2026961
fide player year delete 2026961
relay owner {group or tour id} {username}
user real-name {id} {real name}
"""
