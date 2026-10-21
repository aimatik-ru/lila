package lila.round
package ui

import chess.variant.{ Crazyhouse, Variant }
import chess.Square

import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class RoundUi(helpers: Helpers, gameUi: lila.game.ui.GameUi):
  import helpers.{ *, given }

  def RoundPage(variant: Variant, title: String)(using ctx: Context) =
    Page(title)
      .css(if variant == Crazyhouse then "round.zh" else "round")
      .css(ctx.pref.hasKeyboardMove.option("keyboardMove"))
      .css(ctx.pref.hasVoice.option("voice"))
      .css(ctx.blind.option("round.nvui"))
      .i18nOpt(ctx.speechSynthesis, _.nvui)
      .i18nOpt(ctx.blind, _.keyboardMove, _.variant)
      .flag(_.zoom)
      .csp(_.withWebAssembly)

  def povOpenGraph(pov: Pov)(using Translate) =
    OpenGraph(
      image = cdnUrl(routes.Export.gameThumbnail(pov.gameId, None, None).url).some,
      title = titleGame(pov.game),
      url = routeUrl(routes.Round.watcher(pov.gameId, pov.color)),
      description = describePov(pov)
    )

  def others(playing: UrgentGames, simul: Option[Frag])(using Context) =
    val switchId = "round-toggle-autoswitch"
    frag(
      h3(
        simul | frag(trans.site.currentGames()),
        form3.cmnToggleWrap(st.title := trans.site.automaticallyProceedToNextGameAfterMoving.txt())(
          trans.site.autoSwitch(),
          form3.cmnToggle(switchId, switchId, checked = false)
        )
      ),
      div(cls := "now-playing"):
        val (myTurn, otherTurn) = playing.value.partition(_.isMyTurn)
        (myTurn ++ otherTurn.take(8 - myTurn.size))
          .take(12)
          .map: pov =>
            a(href := routes.Round.player(pov.fullId), cls := pov.isMyTurn.option("my_turn"))(
              span(
                cls := s"mini-game mini-game--init ${pov.game.variant.key} is2d",
                gameUi.mini.renderState(pov)
              )(gameUi.mini.cgWrap),
              span(cls := "meta")(
                playerUsername(
                  pov.opponent.light,
                  pov.opponent.userId.flatMap(lightUserSync),
                  withRating = false,
                  withTitle = true
                ),
                span(cls := "indicator")(
                  if pov.isMyTurn then
                    pov.remainingSeconds
                      .fold[Frag](trans.site.yourTurn())(secondsFromNow(_, alwaysRelative = true))
                  else nbsp
                )
              )
            )
    )

  def describePov(pov: Pov)(using Translate) =
    import pov.*
    val p1 = playerText(game.whitePlayer, withRating = true)
    val p2 = playerText(game.blackPlayer, withRating = true)
    val plays = if game.finishedOrAborted then "сыграли" else "играют"
    def speedName(speed: chess.Speed) = speed match
      case chess.Speed.UltraBullet => trans.site.ultraBullet.txt()
      case chess.Speed.Bullet => trans.site.bullet.txt()
      case chess.Speed.Blitz => trans.site.blitz.txt()
      case chess.Speed.Rapid => trans.site.rapid.txt()
      case chess.Speed.Classical => trans.site.classical.txt()
      case chess.Speed.Correspondence => trans.site.correspondence.txt()
    val speedAndClock =
      if game.sourceIs(_.Import) then "импорт"
      else
        game.clock.fold(trans.site.correspondence.txt()): c =>
          s"${speedName(chess.Speed(c.config))} (${c.config.show})"

    val rated = if game.rated.yes then "рейтинговую" else "товарищескую"
    val variant =
      if game.variant == chess.variant.FromPosition
      then s", ${trans.variant.fromPosition.txt().toLowerCase}"
      else if game.variant.exotic
      then s", ${game.variant.variantTrans.txt()}"
      else ""
    import chess.Status.*
    val result = (game.winner, game.loser, game.status) match
      case (Some(w), _, Mate) => s"${playerText(w)} победил матом."
      case (_, _, Aborted | NoStart) => s"${gameUi.abortReason(game).txt()}."
      case (_, Some(l), Resign | Timeout | Cheat | NoStart) => s"${playerText(l)} сдался."
      case (_, Some(l), Outoftime) => s"У игрока ${playerText(l)} кончилось время."
      case (Some(w), _, UnknownFinish | VariantEnd) => s"${playerText(w)} победил."
      case (_, _, Draw | Stalemate | UnknownFinish) => "Партия закончилась вничью."
      case _ if game.finished => "Партия окончена."
      case _ => "Партия ещё идёт."
    val moves = (game.ply.value - game.startedAtPly.value + 1) / 2
    s"$p1 и $p2 $plays $rated партию ($speedAndClock$variant). $result Ходов: $moves. " +
      "Нажмите, чтобы посмотреть партию, разобрать её и обсудить!"

  def povChessground(pov: Pov)(using ctx: Context): Frag =
    val orient = pov.color
    val lastMove = pov.game.history.lastMove
      .map(_.origDest)
      .so: (orig, dest) =>
        List(orig, dest)
    chessgroundWrap:
      cgBoard:
        raw:
          if ctx.pref.is3d then ""
          else
            def top(p: Square) = orient.fold(7 - p.rank.value, p.rank.value) * 12.5
            def left(p: Square) = orient.fold(p.file.value, 7 - p.file.value) * 12.5
            val highlights = ctx.pref.highlight
              .so(lastMove.distinct.map { pos =>
                s"""<square class="last-move" style="top:${top(pos)}%;left:${left(pos)}%"></square>"""
              })
              .mkString("")
            val pieces =
              if pov.player.blindfold then ""
              else
                pov.game.position.pieces
                  .map: (pos, piece) =>
                    val klass = s"${piece.color.name} ${piece.role.name}"
                    s"""<piece class="$klass" style="top:${top(pos)}%;left:${left(pos)}%"></piece>"""
                  .mkString("")
            s"$highlights$pieces"

  def roundAppPreload(pov: Pov)(using Context): Tag =
    div(cls := "round__app")(
      div(cls := "round__app__board main-board")(povChessground(pov)),
      div(cls := "col1-rmoves-preload")
    )
