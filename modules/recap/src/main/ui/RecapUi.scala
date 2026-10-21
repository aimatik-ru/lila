package lila.recap
package ui

import lila.ui.*
import lila.ui.ScalatagsTemplate.*
import lila.recap.Recap.Availability

final class RecapUi(helpers: Helpers):
  import helpers.*

  private def title(user: User)(using ctx: Context) =
    if ctx.is(user) then "Ваши итоги года"
    else s"Итоги года: ${user.username}"

  def home(av: Availability, user: User)(using Context) =
    val data = av match
      case Availability.Available(data) => data
      case Availability.Queued(data) => data
    Page(title(user))
      .css("recap")
      .js(esmInit("recap", data))
      .i18n(_.recap, _.variant, _.arena, _.swiss, _.patron, _.preferences)
      .csp(_.withInlineIconFont): // swiper's `data: font`
        main(cls := "recap"):
          div(id := "recap-swiper", cls := "swiper")

  def notAvailable(year: Int) =
    Page("Итоги года пока недоступны"):
      main(cls := "page-small box box-pad page")(
        h1(cls := "box__top")(s"Ваши итоги $year года на «Логике»"),
        div(
          p("Итоги появятся в конце года. Загляните тогда!")
        )
      )
