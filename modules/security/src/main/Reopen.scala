package lila.security

import play.api.i18n.Lang
import scalatags.Text.all.*

import lila.core.config.*
import lila.core.i18n.I18nKey.emails as trans
import lila.mailer.Mailer
import lila.user.{ User, UserRepo }

final class Reopen(
    mailer: lila.mailer.Mailer,
    userRepo: UserRepo,
    routeUrl: RouteUrl,
    tokenerSecret: Secret
)(using Executor, lila.core.i18n.Translator):

  import Mailer.html.*

  def prepare(
      u: UserStr,
      email: EmailAddress,
      closedByMod: User => Fu[Boolean]
  ): FuRaise[(String, String), User] = for
    existing <- userRepo.byEmail(email.normalize)
    _ <- raiseIf(existing.exists(_.enabled.yes)):
      "emailUsed" -> "Этот адрес почты уже занят действующей учётной записью."
    user <- userRepo.byId(u)
    user <- user.raiseIfNone("noUser" -> "Учётная запись с таким именем не найдена.")
    _ <- raiseIf(user.enabled.yes):
      "alreadyActive" -> "Эта учётная запись уже активна."
    userEmail <- userRepo.currentOrPrevEmail(user.id)
    userEmail <- userEmail.raiseIfNone:
      "noEmail" -> "У этой учётной записи нет адреса почты, её нельзя восстановить."
    _ <- raiseIf(!email.similarTo(userEmail)):
      "differentEmail" -> "У этой учётной записи другой адрес почты."
    modClosed <- closedByMod(user)
    _ <- raiseIf(modClosed):
      "nope" -> "Извините, эту учётную запись больше нельзя восстановить."
    flags <- userRepo.closedFlags(user)
    _ <- raiseIf(flags.exists(_.forever)):
      "nope" -> "Извините, вы сами попросили, чтобы эту учётную запись нельзя было восстановить."
  yield user

  def send(user: User, email: EmailAddress)(using lang: Lang): Funit =
    tokener.make(user.id).flatMap { token =>
      lila.mon.email.send.reopen.increment()
      val url = routeUrl(routes.Account.reopenLogin(token))
      mailer.sendOrFail:
        Mailer.Message(
          to = email,
          subject = s"Восстановление учётной записи на play.kalugalogic.ru: ${user.username}",
          text = Mailer.txt.addServiceNote(s"""
${trans.passwordReset_clickOrIgnore.txt()}

$url

${trans.common_linkNotWorking.txt()}"""),
          htmlBody = emailMessage(
            p(trans.passwordReset_clickOrIgnore()),
            potentialAction(metaName("Log in"), Mailer.html.url(url)),
            serviceNote
          ).some
        )
    }

  def confirm(token: String): Fu[Option[User]] =
    tokener.read(token).flatMapz(userRepo.disabledById).flatMapz { user =>
      for
        flags <- userRepo.closedFlags(user)
        canReopen = flags.exists(_.forever).not
        _ <- canReopen.so(userRepo.reopen(user.id))
        reopened = canReopen.option(user)
      yield
        if reopened.isDefined
        then lila.common.Bus.pub(lila.core.security.ReopenAccount(user))
        reopened
    }

  private val tokener = StringToken.withLifetime[UserId](tokenerSecret, 20.minutes)
