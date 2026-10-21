package lila.web
package ui

import lila.core.id.CmsPageKey
import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class SitePages(helpers: Helpers):
  import helpers.{ *, given }

  def SitePage(title: String, active: String, contentCls: String = "")(using Context): Page =
    Page(title).wrap: body =>
      main(cls := "page-menu")(
        menu(active),
        div(cls := s"page-menu__content $contentCls")(body)
      )

  def menu(active: String)(using Translate) =
    val sep = div(cls := "sep")
    val external = frag(" ", iconTag(Icon.ExternalArrow))
    def activeCls(c: String) = cls := active.activeO(c)
    lila.ui.bits.pageMenuSubnav(
      a(activeCls("about"), href := "/about")("О площадке"),
      a(activeCls("tos"), href := routes.Cms.tos)(trans.site.termsOfService()),
      a(activeCls("privacy"), href := "/privacy")(trans.site.privacy()),
      sep,
      a(activeCls("source"), href := routes.Cms.source)(trans.site.sourceCode())
    )

  def webmasters(pieceNames: List[String])(using Context) =
    def parameters(extra: Modifier*) = frag(
      p("Параметры:"),
      ul(
        // actual supported board theme list from lila-gif/src/assets.rs
        li(strong("theme"), ": ", List("blue", "brown", "green", "ic", "purple").mkString(", ")),
        li(strong("pieceSet"), ": ", pieceNames.mkString(", ")),
        li(strong("bg"), ": light, dark, system"),
        extra
      )
    )
    SitePage(
      title = "Вебмастерам",
      active = "webmasters",
      contentCls = "page force-ltr"
    ).css("bits.page")
      .csp(_.copy(frameSrc = "https://play.kalugalogic.ru" :: Nil)):
        frag(
          st.section(cls := "box box-pad developers")(
            h1(cls := "box__top")("HTTP API"),
            p(
              "У «Логика» есть HTTP/JSON API в стиле REST, им можно пользоваться. Читайте ",
              a(href := "/api")("документацию HTTP API"),
              "."
            )
          ),
          br,
          st.section(cls := "box box-pad developers") {
            val args =
              """style="width: 400px; aspect-ratio: 10/11;" allowtransparency="true" frameborder="0""""
            frag(
              a(href := "#embed-tv")(
                h1(cls := "box__top", id := "embed-tv")("ТВ «Логика» на вашем сайте")
              ),
              div(cls := "body")(
                div(cls := "center")(raw(s"""<iframe src="/tv/frame?theme=brown&bg=dark" $args></iframe>""")),
                p("Добавьте на свой сайт этот HTML-код:"),
                copyMeInput(s"""<iframe src="$netBaseUrl/tv/frame?theme=brown&bg=dark" $args></iframe>"""),
                parameters(),
                p(
                  "Чтобы показать канал определённого варианта или контроля времени, добавьте " +
                    "в адрес ключ канала. Список каналов: ",
                  a(href := "/tv")("play.kalugalogic.ru/tv"),
                  ". Без ключа показывается партия с самым высоким рейтингом."
                ),
                copyMeInput(
                  s"""<iframe src="$netBaseUrl/tv/rapid/frame?theme=brown&bg=dark" $args></iframe>"""
                )
              )
            )
          },
          br,
          st.section(cls := "box box-pad developers") {
            val args =
              """style="width: 400px; aspect-ratio: 10/11;" allowtransparency="true" frameborder="0""""
            frag(
              a(href := "#embed-puzzle")(
                h1(cls := "box__top", id := "embed-puzzle")("Задача дня на вашем сайте")
              ),
              div(cls := "body")(
                div(cls := "center")(
                  raw(s"""<iframe src="/training/frame?theme=brown&bg=dark" $args></iframe>""")
                ),
                p("Добавьте на свой сайт этот HTML-код:"),
                copyMeInput(
                  s"""<iframe src="$netBaseUrl/training/frame?theme=brown&bg=dark" $args></iframe>"""
                ),
                parameters(),
                p("Текст автоматически переводится на язык посетителя.")
              )
            )
          },
          br,
          st.section(cls := "box box-pad developers") {
            val args = """style="width: 100%; aspect-ratio: 3/2;" frameborder="0""""
            frag(
              a(href := "#embed-study")(
                h1(cls := "box__top", id := "embed-study")("Шахматный анализ на вашем сайте")
              ),
              div(cls := "body")(
                div(cls := "center"):
                  raw(s"""<iframe src="/study/embed/XtFCFYlM/GCUTf2Jk?bg=auto&theme=auto" $args></iframe>""")
                ,
                p(
                  "Создайте ",
                  a(href := routes.Study.allDefault())("студию"),
                  " и нажмите кнопку «Поделиться», чтобы получить HTML-код текущей главы."
                ),
                parameters(),
                p("Текст автоматически переводится на язык посетителя.")
              )
            )
          },
          br,
          st.section(cls := "box box-pad developers") {
            val args = """style="width: 100%; aspect-ratio: 3/2;" frameborder="0""""
            frag(
              a(href := "#embed-game")(
                h1(cls := "box__top", id := "embed-game")("Шахматная партия на вашем сайте")
              ),
              div(cls := "body")(
                div(cls := "center"):
                  raw(s"""<iframe src="/embed/game/MPJcy1JW?bg=auto&theme=auto" $args></iframe>""")
                ,
                p(
                  "На странице анализа партии откройте внизу вкладку ",
                  em("FEN & PGN"),
                  ", затем ",
                  "\"",
                  em(trans.site.embedInYourWebsite(), "\".")
                ),
                parameters(),
                p("Текст автоматически переводится на язык посетителя.")
              )
            )
          },
          br,
          st.section(cls := "box box-pad developers", id := "broadcast") {
            val args = """style="width: 100%; aspect-ratio: 4/3;" frameborder="0""""
            frag(
              a(href := "#embed-broadcast")(
                h1(cls := "box__top", id := "embed-broadcast")("Embed a broadcast in your site")
              ),
              div(cls := "body")(
                div(cls := "center"):
                  raw:
                    s"""<iframe src="https://play.kalugalogic.ru/embed/broadcast/fide-world-rapidblitz-team-championships-2024--rapid-matches-1-10/G1YjiG7j" $args></iframe>"""
                ,
                p(
                  "On a broadcast page, select the embed iframe code, then optionally add query parameters to customize the appearance."
                ),
                parameters(),
                p("Текст автоматически переводится на язык посетителя.")
              )
            )
          },
          br,
          st.section(cls := "box box-pad developers", id := "analysis") {
            val args = """style="width: 100%; aspect-ratio: 4/3;" frameborder="0""""
            val iframe =
              s"""<iframe src="https://play.kalugalogic.ru/embed/analysis" $args></iframe>"""
            frag(
              a(href := "#embed-analysis")(
                h1(cls := "box__top", id := "embed-analysis")("Доска анализа на вашем сайте")
              ),
              div(cls := "body")(
                div(cls := "center")(raw(iframe)),
                p(
                  "Встраивает ",
                  a(href := routes.UserAnalysis.index)("полноценную доску анализа «Логика»"),
                  " с оценкой Stockfish, дебютной базой и эндшпильными таблицами."
                ),
                copyMeInput(iframe),
                parameters(
                  li(strong("fen"), ": своя начальная позиция в FEN, пробелы заменены подчёркиваниями"),
                  li(strong("color"), ": начальная ориентация доски, black или white")
                ),
                div(
                  "Пример со своей начальной позицией:",
                  copyMeInput:
                    s"""<iframe src="https://play.kalugalogic.ru/embed/analysis?fen=r1bqkb1r/pp2pppp/2np1n2/6B1/3NP3/2N5/PPP2PPP/R2QKB1R_b_KQkq_-_1_6&color=black" $args></iframe>"""
                ),
                p("Текст автоматически переводится на язык посетителя.")
              )
            )
          }
        )

  private val repoRoot = "https://github.com/lichess-org/lila"

  def source(pageTitle: String, rendered: Frag, version: Option[WebConfig.LilaVersion])(using
      Context
  ) =
    SitePage(title = pageTitle, active = "source", contentCls = "page force-ltr")
      .css("bits.source")
      .js(esmInitBit("setAssetInfo")):
        frag(
          st.section(cls := "box")(
            h1(cls := "box__top")(pageTitle),
            table(cls := "slist slist-pad", id := "version")(
              thead(
                tr(
                  th("Current versions"),
                  th(colspan := 2)("Last boot: ", momentFromNow(lila.common.Uptime.startedAt))
                )
              ),
              tbody(
                version.map: v =>
                  tr(
                    td(
                      span("Server"),
                      timeTag(v.date)
                    ),
                    td(span(a(href := s"$repoRoot/commits/${v.commit}"):
                      pre(v.commit.take(7)))),
                    td(
                      a(href := s"$repoRoot/compare/${v.commit}...master", title := "Upcoming changes")(
                        pre("...")
                      )
                    )
                  ),
                tr(
                  td(
                    "Assets",
                    timeTag(id := "asset-version-date")
                  ),
                  td(a(id := "asset-version-commit")(pre)),
                  td(a(id := "asset-version-upcoming", title := "Upcoming changes")(pre("...")))
                )
              )
            )
          ),
          br,
          st.section(cls := "box box-pad body")(rendered)
        )

  def lag(using Context) =
    import trans.lag as trl
    SitePage(title = trl.isLichessLagging.txt(), active = "lag")
      .css("bits.lag")
      .js(esmInit("chart.lag")):
        div(cls := "box box-pad lag")(
          h1(cls := "box__top")(
            trl.isLichessLagging(),
            span(cls := "answer short")(
              span(cls := "waiting")(trl.measurementInProgressThreeDot()),
              span(cls := "nope-nope none")(trl.noAndYourNetworkIsGood()),
              span(cls := "nope-yep none")(trl.noAndYourNetworkIsBad()),
              span(cls := "yep none")(trl.yesItWillBeFixedSoon())
            )
          ),
          div(cls := "answer long")(
            trl.andNowTheLongAnswerLagComposedOfTwoValues()
          ),
          div(cls := "sections")(
            st.section(cls := "server")(
              h2(trl.lichessServerLatency()),
              div(cls := "meter")(canvas(cls := "server-chart")),
              p(
                trl.serverLatencyAndLoadExplanation()
              )
            ),
            st.section(cls := "network")(
              h2(trl.networkBetweenLichessAndYou()),
              div(cls := "meter")(canvas(cls := "network-chart")),
              p(
                trl.networkBetweenLichessAndYouExplanation()
              )
            )
          ),
          div(cls := "last-word")(
            p(trl.youCanFindTheseValuesAtAnyTimeByClickingOnYourUsername()),
            h2(trl.lagCompensation()),
            p(trl.lagCompensationExplanation())
          )
        )

  def getFishnet =
    Page("fishnet API key request")
      .csp(_.withGoogleForm):
        main:
          iframe(
            src := "https://docs.google.com/forms/d/e/1FAIpQLSeGgDHgWGP0uobQknF92eCMXqebyNBTyzJoJqbeGjRezlbWOw/viewform?embedded=true",
            style := "width:100%;height:1400px",
            st.frameborder := 0,
            frame.credentialless
          )(spinner)

  def errorPage =
    Page("Внутренняя ошибка сервера"):
      main(cls := "page-small box box-pad")(
        h1(cls := "box__top")("На этой странице что-то пошло не так"),
        p(
          "Если ошибка повторяется, ",
          a(href := s"${routes.Main.contact}#help-error-page")("сообщите о ней"),
          "."
        )
      )
