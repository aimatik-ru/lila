package lila.challenge
package ui

import play.api.libs.json.{ JsObject, Json }

import lila.challenge.Challenge.Status
import lila.core.LightUser
import lila.core.game.GameRule
import lila.core.user.WithPerf
import lila.core.relation.Relation
import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class ChallengeUi(helpers: Helpers):
  import helpers.{ *, given }

  def page(c: Challenge, json: JsObject, owner: Boolean, color: Option[Color] = None)(using
      ctx: Context
  ) =
    val title = challengeTitle(c)
    Page(title)
      .graph(
        title = title,
        url = routeUrl(routes.Round.watcher(c.gameId, Color.white)),
        description = "Примите вызов или следите за партией здесь."
      )
      .js(Esm("bits.qrcode"))
      .js(
        PageModule(
          "bits.challengePage",
          Json.obj(
            "xhrUrl" -> routes.Challenge.show(c.id, color).url,
            "owner" -> owner,
            "data" -> json
          )
        )
      )
      .css("challenge.page")

  private def challengeTitle(c: Challenge)(using ctx: Context) =
    val perf = c.clock.map(_.config).fold(c.perfType.trans) { clock =>
      s"${c.perfType.trans} (${clock.show})"
    }
    val challenger = c.challengerUser.fold(trans.site.anonymous.txt()): reg =>
      s"${titleNameOrId(reg.id)}${ctx.pref.showRatings.so(s" (${reg.rating.show})")}"
    val players =
      if c.isOpen then "Открытый вызов"
      else
        c.destUser.fold(s"Вызов от $challenger"): dest =>
          val destRating = ctx.pref.showRatings.so(s" (${dest.rating.show})")
          s"$challenger вызывает ${titleNameOrId(dest.id)}$destRating"
    s"$perf • ${ratedName(c.rated)} • $players"

  private def details(c: Challenge, requestedColor: Option[Color])(using ctx: Context) =
    div(cls := "details-wrapper")(
      div(cls := "content")(
        div(
          cls := "variant",
          dataIcon := (if c.initialFen.isDefined then Icon.Feather else c.perfType.icon)
        )(
          div(
            variantLink(c.variant, c.perfType, c.initialFen),
            br,
            span(cls := "clock"):
              c.daysPerTurn
                .fold(shortClockName(c.clock.map(_.config))): days =>
                  if days.value == 1 then trans.site.oneDay()
                  else trans.site.nbDays.pluralSame(days.value)
          )
        ),
        div(cls := "mode")(
          c.open.fold(c.colorChoice.some)(_.colorFor(requestedColor)).map { colorChoice =>
            frag(colorChoice.trans(), br)
          },
          ratedName(c.rated)
        )
      ),
      c.rules.nonEmpty.option(
        div(cls := "rules")(
          h2("Особые правила:"),
          div(fragList(c.rules.toList.map(showRule), "/"))
        )
      )
    )

  private def showRule(r: GameRule) =
    val (text, flair) = getRuleStyle(r);
    div(cls := "challenge-rule")(
      iconFlair(flair),
      text
    )

  private def getRuleStyle(r: GameRule): (String, Flair) =
    r match
      case GameRule.noAbort => ("Без отмены партии", Flair("symbols.cross-mark"));
      case GameRule.noRematch => ("Без реванша", Flair("symbols.recycling-symbol"));
      case GameRule.noGiveTime => ("Без добавления времени", Flair("objects.alarm-clock"));
      case GameRule.noClaimWin => ("Без требования победы", Flair("objects.hourglass-done"));
      case GameRule.noEarlyDraw => ("Без ранней ничьей", Flair("people.handshake-light-skin-tone"));

  def mine(
      c: Challenge,
      json: JsObject,
      friends: Seq[LightUser],
      error: Option[String],
      color: Option[Color]
  )(using ctx: Context) =

    val cancelForm =
      postForm(action := routes.Challenge.cancel(c.id), cls := "cancel xhr"):
        submitButton(cls := "button button-red text", dataIcon := Icon.X)(trans.site.cancel())

    page(c, json, owner = true):
      val challengeLink = routeUrl(routes.Round.watcher(c.gameId, Color.white))
      main(cls := s"page-small challenge-page box box-pad challenge--${c.status.name}")(
        c.status match
          case Status.Created | Status.Offline =>
            div(id := "ping-challenge")(
              h1(cls := "box__top")(
                if c.isOpen then c.name | "Открытый вызов" else trans.challenge.challengeToPlay.txt()
              ),
              details(c, color),
              c.destUserId
                .map { destId =>
                  div(cls := "waiting")(
                    userIdLink(destId.some, cssClass = "target".some),
                    if c.clock.isEmpty then
                      div(cls := "correspondence-waiting text", dataIcon := Icon.Checkmark):
                        "Вызов отправлен"
                    else spinner,
                    p(trans.site.waitingForOpponent())
                  )
                }
                .getOrElse {
                  if c.isOpen then
                    div(cls := "waiting")(
                      spinner,
                      p(trans.site.waitingForOpponent())
                    )
                  else
                    div(cls := "invite")(
                      button(cls := "mobile-instructions button none")("Нажмите, чтобы поделиться"),
                      div(cls := "invite__url")(
                        h2(cls := "ninja-title", trans.site.toInviteSomeoneToPlayGiveThisUrl()),
                        br,
                        copyMeInput(challengeLink.value),
                        br,
                        p(trans.site.theFirstPersonToComeOnThisUrlWillPlayWithYou())
                      ),
                      ctx.isAuth.option(
                        div(cls := "invite__user")(
                          h2(cls := "ninja-title", trans.challenge.inviteLichessUser()),
                          friends.nonEmpty.option(
                            div(cls := "invite__user__recent")(
                              friends.map: user =>
                                button(cls := "button button-metal", dataUser := user.name):
                                  lightUserSpan(user, withOnline = true)
                            )
                          ),
                          br,
                          postForm(
                            cls := "user-invite complete-parent",
                            action := routes.Challenge.toFriend(c.id)
                          )(
                            input(
                              name := "username",
                              cls := "friend-autocomplete",
                              placeholder := trans.search.search.txt()
                            ),
                            error.map { p(cls := "error")(_) }
                          )
                        )
                      ),
                      div(cls := "invite__qrcode")(
                        h2(cls := "ninja-title", trans.site.orLetYourOpponentScanQrCode()),
                        qrcode(challengeLink, width = 150)
                      ),
                      genericUrl(c).map: url =>
                        div(cls := "invite__url")(
                          h2(cls := "ninja-title", trans.site.reusableChallengeUrl()),
                          br,
                          copyMeInput(url.value),
                          br,
                          p(trans.site.permanentLinkForAnyoneToChallengeYou())
                        )
                    )
                },
              c.notableInitialFen.map: fen =>
                frag(
                  br,
                  div(cls := "board-preview", chessgroundMini(fen.board, c.finalColor)(div))
                ),
              (!c.isOpen).option(cancelForm)
            )
          case Status.Declined =>
            div(cls := "follow-up")(
              h1(cls := "box__top")(trans.challenge.challengeDeclined()),
              blockquote(cls := "challenge-reason pull-quote")(
                p(c.anyDeclineReason.trans()),
                footer(userIdLink(c.destUserId))
              ),
              details(c, color),
              a(cls := "button button-fat", href := routes.Lobby.home)(trans.site.newOpponent())
            )
          case Status.Accepted =>
            div(cls := "follow-up")(
              h1(cls := "box__top")(trans.challenge.challengeAccepted()),
              details(c, color),
              a(
                id := "challenge-redirect",
                href := routes.Round.watcher(c.gameId, Color.white),
                cls := "button button-fat"
              ):
                trans.site.joinTheGame()
            )
          case Status.Canceled =>
            div(cls := "follow-up")(
              h1(cls := "box__top")(trans.challenge.challengeCanceled()),
              details(c, color),
              a(cls := "button button-fat", href := routes.Lobby.home)(trans.site.newOpponent())
            )
      )

  def theirs(
      c: Challenge,
      json: JsObject,
      user: Option[WithPerf],
      color: Option[Color],
      relation: Option[Relation]
  )(using ctx: Context) =
    page(c, json, owner = false, color):
      main(cls := "page-small challenge-page challenge-theirs box box-pad"):
        c.status match
          case Status.Created | Status.Offline =>
            frag(
              boxTop:
                h1:
                  if c.isOpen then c.name | "Открытый вызов"
                  else
                    user.fold[Frag](trans.site.anonymous()): u =>
                      frag(
                        userLink(u.user),
                        ctx.pref.showRatings.option(frag(" (", u.perf.glicko.display, ")"))
                      )
              ,
              details(c, color),
              c.notableInitialFen.map: fen =>
                div(cls := "board-preview", chessgroundMini(fen.board, !c.finalColor)(div)),
              if relation.has(Relation.Block) then badTag("Вы заблокировали этого игрока.")
              else if c.open.exists(!_.canJoin) then
                div(
                  "Ждём, когда ",
                  fragList((~c.open.flatMap(_.userIdList)).map(uid => userIdLink(uid.some)), " и "),
                  " начнут партию."
                )
              else if color.map(Challenge.ColorChoice.apply).has(c.colorChoice) then
                badTag(
                  s"Это ссылка не на тот цвет для открытого вызова: игрок за " +
                    s"${color.so(_.fold("белых", "чёрных"))} уже присоединился."
                )
              else if c.rated.no || ctx.isAuth then
                frag(
                  (c.rated.yes && c.unlimited)
                    .option(badTag(trans.site.bewareTheGameIsRatedButHasNoClock())),
                  postForm(cls := "accept", action := routes.Challenge.accept(c.id, color))(
                    submitButton(cls := "text button button-fat", dataIcon := Icon.PlayTriangle)(
                      trans.site.joinTheGame()
                    )
                  )
                )
              else
                frag(
                  hr,
                  badTag(
                    p(trans.site.thisGameIsRated()),
                    a(
                      cls := "button",
                      href := s"${routes.Auth.login}?referrer=${routes.Round.watcher(c.gameId, Color.white)}"
                    )(trans.site.signIn())
                  )
                )
            )
          case Status.Declined =>
            div(cls := "follow-up")(
              h1(cls := "box__top")(trans.challenge.challengeDeclined()),
              details(c, color),
              a(cls := "button button-fat", href := routes.Lobby.home)(trans.site.newOpponent())
            )
          case Status.Accepted =>
            div(cls := "follow-up")(
              h1(cls := "box__top")(trans.challenge.challengeAccepted()),
              details(c, color),
              a(
                id := "challenge-redirect",
                href := routes.Round.watcher(c.gameId, Color.white),
                cls := "button button-fat"
              )(
                trans.site.joinTheGame()
              )
            )
          case Status.Canceled =>
            div(cls := "follow-up")(
              h1(cls := "box__top")(trans.challenge.challengeCanceled()),
              details(c, color),
              a(cls := "button button-fat", href := routes.Lobby.home)(trans.site.newOpponent())
            )

  private def genericUrl(c: Challenge): Option[Url] =
    c.challengerUser.map: user =>
      import c.{ initialFen, rated, timeControl, variant, colorChoice }
      val params: Map[String, String] = List(
        List(
          "user" -> user.id.value,
          "variant" -> variant.key.value,
          "gameMode" -> rated.name
        ),
        timeControl.match
          case Challenge.TimeControl.Clock(config) =>
            List(
              "minutesPerSide" -> config.limitInMinutes.toString,
              "increment" -> config.increment.roundSeconds.toString
            )
          case Challenge.TimeControl.Correspondence(days) => List("days" -> days.value.toString)
          case Challenge.TimeControl.Unlimited => List("time" -> "unlimited"),
        initialFen.map(f => "fen" -> f.value),
        colorChoice.match
          case Challenge.ColorChoice.Random => Nil
          case choice => List("color" -> choice.trans.value)
      ).flatten.toMap
      Url(addQueryParams(routeUrl(routes.Lobby.home).value, params) + "#friend")
