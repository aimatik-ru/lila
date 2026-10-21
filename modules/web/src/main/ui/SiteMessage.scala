package lila.web
package ui

import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class SiteMessage(helpers: Helpers):
  import helpers.{ *, given }

  def apply(title: String, back: Option[String] = None, icon: Option[Icon] = None) =
    Page(title).wrap: body =>
      main(cls := "box box-pad")(
        boxTop(
          h1(dataIcon := icon.ifTrue(back.isEmpty), cls := List("text" -> (icon.isDefined && back.isEmpty)))(
            back.map: url =>
              a(href := url, dataIcon := Icon.LessThan, cls := "text"),
            title
          )
        ),
        body
      )

  def noBot = apply("Ботам сюда нельзя"):
    p("Извините, учётным записям ботов сюда нельзя.")

  def noEngine = apply("Доступ закрыт"):
    p("Извините, игрокам, которые пользуются подсказками движка, сюда нельзя.")

  def noBooster = apply("Доступ закрыт"):
    p("Извините, игрокам, которые накручивают или занижают рейтинг, сюда нельзя.")

  def noLame(using me: Me) =
    if me.marks.boost then noBooster
    else if me.marks.engine then noEngine
    else // ?
      apply("Доступ ограничен"):
        p("Извините, доступ к этой странице ограничен.")

  def blacklistedMessage(using ctx: Context) =
    s"Извините, с вашего IP-адреса ${ctx.ip} нарушали правила площадки, и теперь он заблокирован."

  def blacklistedSnippet(using Context) = lila.ui.Snippet(frag(blacklistedMessage))

  def streamingMod = apply("Disabled while streaming"):
    p(
      "This moderation feature is disabled while streaming, ",
      "to avoid leaking sensible information."
    )

  def challengeDenied(msg: String)(using Context) =
    apply(
      title = trans.challenge.challengeToPlay.txt(),
      back = routes.Lobby.home.url.some
    )(p(msg))

  def insightNoGames(u: User)(using Context) =
    apply(
      title = s"${u.username} ещё не сыграл ни одной рейтинговой партии!",
      back = routes.User.show(u.id).url.some
    ):
      p(
        "Чтобы открыть шахматную аналитику, игрок ",
        userLink(u),
        " должен сыграть хотя бы одну рейтинговую партию."
      )

  def teamCreateLimit = apply("Нельзя создать команду"):
    p("На этой неделе вы уже создали команду.")

  def teamJoinLimit = apply("Нельзя вступить в команду"):
    p("Вы уже вступили в слишком много команд.")

  def relayPrivate = apply("This tournament is private", routes.RelayTour.index().url.some):
    p("Sorry, this tournament is private, or maybe it doesn't exist.")

  def authFailed = apply("403 — доступ запрещён!"):
    p("У вас нет доступа к этой странице.")

  def serverError(msg: String) = apply("Что-то пошло не так")(p(msg))

  def temporarilyDisabled = apply("Временно отключено"):
    p("Извините, эта функция временно отключена. Мы ищем способ её вернуть.")

  def rateLimited(msg: String = "Слишком много запросов") =
    apply(if msg.startsWith("Too many requests") then "Слишком много запросов" else msg):
      p(
        "С вашего устройства или из вашей сети пришло слишком много запросов " +
          "за короткое время. Попробуйте позже."
      )

  def notYet(text: String) = apply("Пока недоступно")(p(text))
