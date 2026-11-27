package views.lobby

import play.api.libs.json.Json

import lila.app.UiEnv.{ *, given }
import lila.app.mashup.Preload.Homepage
import lila.core.perf.UserWithPerfs

object home:

  // Памятка «Турниры каждую неделю» из «Настроек площадки» (/dev/logic-colors).
  // Стиль inline: блок выкатывается подменой jar, без пересборки CSS лобби.
  private val weeklyCss =
    ".logic-weekly__title{display:block;padding:.6em .8em .3em;font-size:1em;font-weight:bold}" +
      ".logic-weekly .tour-spotlight{white-space:normal}" +
      ".logic-weekly .tour-spotlight .name{font-weight:bold;line-height:1.3}" +
      ".logic-weekly .tour-spotlight .headline{line-height:1.3;margin:0}"

  private def weekly: Option[Frag] =
    val rows = lila.web.LogicWeekly.parse(env.web.settings.logicWeekly.get())
    rows.nonEmpty.option:
      div(cls := "lobby__spotlights logic-weekly")(
        raw(s"<style>$weeklyCss</style>"),
        h2(cls := "logic-weekly__title")("Турниры каждую неделю"),
        rows.map: r =>
          a(href := routes.Tournament.home, cls := "tour-spotlight little")(
            iconTag(Icon.Trophy)(cls := "img"),
            span(cls := "content")(
              span(cls := "name")(r.when),
              r.what.nonEmpty.option(span(cls := "headline")(r.what))
            )
          )
      )

  def apply(homepage: Homepage)(using ctx: Context) =
    import homepage.*
    Page("")
      .copy(fullTitle = s"$siteName • ${trans.site.freeOnlineChess.txt()}".some)
      .i18n(_.variant)
      .js(
        PageModule(
          "lobby",
          Json
            .obj(
              "data" -> data,
              "showRatings" -> ctx.pref.showRatings
            )
            .add("hasUnreadLichessMessage", hasUnreadLichessMessage)
            .add("bots", Granter.opt(_.Beta))
            .add("playban", playban.map(lila.playban.TempBan.lobbyJson))
        )
      )
      .css("lobby")
      .graph(
        OpenGraph(
          image = staticAssetUrl("logo/logik-tile-wide.png").some,
          title = siteName,
          url = netBaseUrl.into(Url),
          description = trans.site.siteDescription.txt()
        )
      )
      .hrefLangs(lila.ui.LangPath("/")):
        given Option[UserWithPerfs] = homepage.me
        main(
          cls := List(
            "lobby" -> true,
            "lobby-nope" -> (playban.isDefined || currentGame.isDefined || homepage.hasUnreadLichessMessage)
          )
        )(
          div(cls := "lobby__side")(
            weekly,
            ctx.blind.option(h2(trans.nvui.featuredEvents())),
            div(cls := "lobby__spotlights"):
              val eventTags = events.map(bits.spotlight)
              frag(
                eventTags,
                ctx.noBot.option {
                  val nbManual = eventTags.size
                  val simulBBB = simuls.find(isFeaturable(_) && nbManual < 4)
                  val nbForced = nbManual + simulBBB.size.toInt
                  val tourBBBs = if nbForced > 3 then 0 else if nbForced == 3 then 1 else 3 - nbForced
                  frag(
                    lila.tournament.Spotlight.select(tours, tourBBBs).map {
                      views.tournament.list.homepageSpotlight(_)
                    },
                    swiss.ifTrue(nbForced < 3).map(views.swiss.ui.homepageSpotlight),
                    simulBBB.map(views.simul.ui.homepageSpotlight)
                  )
                }
              )
            ,
            classes.nonEmpty.option:
              div(cls := "lobby__classes"):
                classes.map: clas =>
                  a(href := routes.Clas.show(clas.id), dataIcon := Icon.Group)(clas.name)
            ,
            if ctx.isAuth then
              div(cls := "lobby__timeline")(
                ctx.blind.option(h2(trans.site.timeline())),
                views.timeline.entries(userTimeline)
              )
            else
              div(cls := "about-side")(
                ctx.blind.option(h2(trans.site.about())),
                "Шахматная площадка клуба «Логик» из Калуги. Бесплатно и без рекламы. ",
                a(href := "/about")("О площадке")
              )
          ),
          currentGame
            .map(bits.currentGameInfo)
            .orElse:
              hasUnreadLichessMessage.option(bits.showUnreadLichessMessage)
            .orElse:
              playban.map(bits.playbanInfo)
            .getOrElse:
              if ctx.blind then blindLobby(blindGames) else bits.lobbyApp
          ,
          div(cls := "lobby__table")(
            div(cls := "lobby__start")(
              button(cls := "button lobby__start__button lobby__start__button--hook")(
                trans.site.createLobbyGame()
              ),
              button(cls := "button button-metal lobby__start__button lobby__start__button--friend")(
                trans.site.challengeAFriend()
              ),
              button(cls := "button button-metal lobby__start__button lobby__start__button--ai")(
                trans.site.playAgainstComputer()
              )
            )
          ),
          div(cls := "lobby__tv")(
            featured.map(g => views.game.mini(Pov.naturalOrientation(g), tv = true))
          ),
          div(cls := "lobby__puzzle")(
            puzzle.map(p => views.puzzle.bits.dailyLink(p)())
          ),
          ctx.noBot.option(bits.underboards(tours, simuls)),
          div(cls := "lobby__about")(
            ctx.blind.option(h2(trans.site.about())),
            a(href := "/about")("О площадке"),
            a(href := routes.Cms.tos)(trans.site.termsOfService()),
            a(href := "/privacy")(trans.site.privacy())
          )
        )
