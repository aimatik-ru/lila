package lila.msg

import lila.core.LightUser
import lila.core.id.ForumCategId
import lila.core.team.LightTeam

object MsgPreset:

  import lila.core.msg.MsgPreset as Msg

  private val baseUrl = "https://play.kalugalogic.ru"

  def maxFollow(username: UserName, max: Max) =
    Msg(
      name = "Достигнут предел подписок!",
      text = s"""К сожалению, на «Логике» нельзя подписаться больше чем на $max игроков.
Чтобы подписаться на новых игроков, сначала отпишитесь от кого-нибудь здесь: $baseUrl/@/$username/following.

Спасибо за понимание.""",
      mustRead = false
    )

  def forumRelocation(title: String, newUrl: String) =
    s"""Модератор перенёс ваше сообщение «$title» в другой раздел форума. Оно здесь: $baseUrl$newUrl."""

  object forumDeletion:

    val presets = List(
      "публичное унижение",
      "неуважение к другим игрокам",
      "спам",
      "неподобающее поведение",
      "не тот раздел форума",
      "текст, созданный ИИ"
    )

    def byModerator = compose("Модератор")

    def byTeamLeader(forumId: ForumCategId) = compose(s"Руководитель команды $baseUrl/forum/$forumId")

    def byBlogAuthor(user: UserName) = compose(by = s"Автор блога $user")

    private def compose(by: String)(reason: String, forumPost: String) =
      s"""$by удалил ваше сообщение по причине: $reason.
Прочитайте правила форума: $baseUrl/page/forum-etiquette
----
$forumPost
    """

  def newPermissions(by: LightUser, team: LightTeam, perms: Iterable[String], teamUrl: Url) =
    s"""@${by.name} изменил ваши права руководителя в команде «${team.name}».
Ваши новые права: ${perms.mkString(", ")}.
${teamUrl}"""

  def payoutEligible(payoutsUrl: Url, msg: lila.core.msg.PayoutMessages) =
    import msg.*
    val deadline = finishedAt.atZone(java.time.ZoneOffset.UTC).toLocalDate.plusMonths(6)
    Msg(
      name = "Prize payout",
      text = s"""Congratulations on your finish in $tourName! $tourUrl

Lichess is offering prizes to top finishers in this tournament, and your performance means you may be eligible for a prize.

Please visit $payoutsUrl to provide the necessary information for payout. The deadline for claiming prizes is $deadline.""",
      mustRead = true
    )

  def apiTokenRevoked(url: String) =
    s"""Your Lichess API token has been found on GitHub

We detected one of your API tokens in a public code repository on GitHub at the following URL:

$url

We have automatically revoked the token to protect your account.
    """
