package lila.playban

final private class PlaybanFeedback(
    chatApi: lila.core.chat.ChatApi,
    lightUser: lila.core.LightUser.Getter
)(using Executor):

  private val tempBan = "может привести к временному запрету на игру."

  def abort(pov: Pov): Unit = tell(pov, s"Внимание, {user}! Слишком частая отмена партий $tempBan")

  def noStart(pov: Pov): Unit = tell(pov, s"Внимание, {user}! Если не начинать партии, это $tempBan")

  def rageQuit(pov: Pov): Unit = tell(pov, s"Внимание, {user}! Уход из партии без сдачи $tempBan")

  def sitting(pov: Pov): Unit =
    tell(
      pov,
      s"Напоминаем, {user}: если раз за разом ждать, пока кончится время, " +
        s"или тянуть со сдачей в проигранной позиции, это $tempBan"
    )

  def quickResign(pov: Pov): Unit = tell(pov, s"Внимание, {user}! Слишком быстрая сдача партий $tempBan")

  private def tell(pov: Pov, template: String): Unit =
    pov.player.userId.foreach { userId =>
      lightUser(userId).foreach { light =>
        val message = template.replace("{user}", light.fold(userId.value)(_.name.value))
        chatApi.volatile(pov.gameId.into(ChatId), message, _.round)
      }
    }

object PlaybanFeedback:

  val sittingAutoPreset = lila.core.msg.MsgPreset(
    name = "Warning: leaving games / stalling on time",
    text =
      """В вашей истории есть несколько партий, где вы ушли из игры или просто ждали, пока кончится время, вместо того чтобы играть или сдаться.
  Это очень неприятно для соперников. Если так будет продолжаться, нам придётся закрыть ваш аккаунт.""",
    mustRead = true
  )
