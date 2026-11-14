package lila.msg

import lila.memo.MongoCache
import lila.core.config.BaseUrl
import lila.core.i18n.I18nKey.msg as trans
import lila.core.msg.SystemMsg

final class MsgByLichess(
    mongoCache: MongoCache.Api,
    userApi: lila.core.user.UserApi,
    api: MsgApi,
    baseUrl: BaseUrl
)(using
    Executor,
    lila.core.i18n.Translator
):

  // «Логик»: напоминание о 2FA не шлём. Системного пользователя lichess на площадке нет,
  // отправка падала «Missing convo contact» и роняла /team/X/edit и /class/X (500).
  object twoFactorReminder:
    def apply(userId: UserId): Fu[Boolean] = fuccess(true)

  def lichobileDeprecationMessage(user: lila.core.user.User) =
    given play.api.i18n.Lang = user.realLang | lila.core.i18n.defaultLang
    val text =
      s"""${trans.lichobileNewAppAvailable.txt()}\n\n${trans.lichobileNewAppDownload.txt(s"$baseUrl/app")}"""
    api.systemPost(SystemMsg.mustRead(user.id, text))

  object chatTimeout:
    def apply(userId: UserId) = cache.get(userId)
    private val text = s"""Chat rules violation resulted in timeout

Please review the chat rules on ${lila.core.chat.etiquetteUrl}."""
    private val cache = mongoCache[UserId, Boolean](1024, "chat:timeout:msg", 1.day, _.value): loader =>
      _.expireAfterWrite(1.hour).buildAsyncFuture:
        loader: userId =>
          userApi
            .isTroll(userId)
            .not
            .flatMapz:
              api.systemPost(SystemMsg.mustRead(userId, text)).inject(true)
