package lila.web
package ui

import scalalib.model.Days
import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class TopNav(helpers: Helpers):
  import helpers.{ *, given }

  private def linkTitle(url: String, name: Frag)(using ctx: Context) =
    if ctx.blind then h3(name) else a(href := url)(name)

  def apply(seesClassMenu: Boolean, hasDgt: Boolean)(using ctx: Context) =
    st.nav(id := "topnav", cls := "hover")(
      st.section(
        linkTitle(
          "/",
          frag(
            span(cls := "play")(trans.site.play()),
            span(cls := "home")("Логик")
          )
        ),
        div(role := "group")(
          if ctx.noBot then a(href := s"${langHref("/")}?any#hook")(trans.site.createLobbyGame())
          else a(href := "/?any#friend")(trans.site.challengeAFriend()),
          Option.when(ctx.noBot):
            frag(
              a(href := langHref(routes.Tournament.home))(trans.arena.arenaTournaments()),
              a(href := langHref(routes.Swiss.home))(trans.swiss.swissTournaments()),
              a(href := langHref(routes.Simul.home))(trans.site.simultaneousExhibitions()),
              ctx.me.map: me =>
                val tab = if Granter.opt(_.Admin) then "created" else "upcoming"
                a(href := routes.UserTournament.path(me.username, tab))(trans.arena.myTournaments())
              ,
              a(href := routes.Tournament.calendar)(trans.site.tournamentCalendar()),
              hasDgt.option(a(href := routes.DgtCtrl.index)(trans.dgt.dgtBoard()))
            )
        )
      ),
      Option.when(ctx.noBot):
        val puzzleUrl = langHref(routes.Puzzle.home.url)
        st.section(
          linkTitle(puzzleUrl, trans.site.puzzles()),
          div(role := "group")(
            a(href := puzzleUrl)(trans.site.puzzles()),
            a(href := langHref(routes.Puzzle.themes))(trans.puzzle.puzzleThemes()),
            a(href := routes.Puzzle.dashboard(Days(30), "home", none))(trans.puzzle.puzzleDashboard()),
            a(href := langHref(routes.Puzzle.streak))("Серия задач"),
            a(href := langHref(routes.Storm.home))("Штурм задач"),
            a(href := langHref(routes.Racer.home))("Гонка задач")
          )
        )
      ,
      st.section(
        linkTitle(routes.Learn.index.url, trans.site.learnMenu()),
        div(role := "group")(
          Option.when(ctx.noBot):
            frag(
              a(href := langHref(routes.Learn.index))(trans.site.chessBasics()),
              a(href := langHref(routes.Coordinate.home))(trans.coordinates.coordinates())
            )
          ,
          a(href := langHref(routes.Study.allDefault()))(trans.site.studyMenu()),
          seesClassMenu.option(a(href := routes.Clas.index)(trans.clas.lichessClasses()))
        )
      ),
      st.section(
        linkTitle(routes.Tv.games.url, trans.site.watch()),
        div(role := "group")(
          a(href := langHref(routes.Tv.index))("TV"),
          a(href := routes.Tv.games)(trans.site.currentGames())
        )
      ),
      st.section(
        linkTitle(routes.User.list.url, trans.site.community()),
        div(role := "group")(
          a(href := routes.User.list)(trans.site.players()),
          ctx.me.map(me => a(href := routes.Relation.following(me.username))(trans.site.friends())),
          a(href := routes.Team.home())(trans.team.teams())
        )
      ),
      st.section(
        linkTitle(routes.UserAnalysis.index.url, trans.site.tools()),
        div(role := "group")(
          a(href := routes.UserAnalysis.index)(trans.site.analysis()),
          a(href := routes.Editor.index)(trans.site.boardEditor()),
          a(href := routes.Importer.importGame)(trans.site.importGame())
        )
      )
    )
