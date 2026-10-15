package lila.web
package ui

import lila.ui.*
import lila.ui.ScalatagsTemplate.*
import lila.core.i18n.Translate

object bits:

  object splitNumber extends NumberHelper:
    private val NumberFirstRegex = """(\d++)\s(.+)""".r
    private val NumberLastRegex = """\s(\d++)$""".r.unanchored

    def apply(s: Frag)(using ctx: Context)(using Translate): Frag =
      if ctx.blind then s
      else
        val rendered = s.render
        rendered match
          case NumberFirstRegex(number, html) =>
            frag(
              strong((~number.toIntOption).localize),
              br,
              raw(html)
            )
          case NumberLastRegex(n) if rendered.length > n.length + 1 =>
            frag(
              raw(rendered.dropRight(n.length + 1)),
              br,
              strong((~n.toIntOption).localize)
            )
          case h => raw(h.replaceIf('\n', "<br>"))

  lazy val stage = a(
    href := "https://play.kalugalogic.ru",
    style := """
background: #7f1010;
color: #fff;
position: fixed;
bottom: 0;
left: 0;
padding: .5em 1em;
border-top-right-radius: 3px;
z-index: 99;
"""
  ):
    "This is an empty Логик preview website, go to play.kalugalogic.ru instead"

  val connectLinks: Frag = div(cls := "connect-links")(
    a(href := "https://kalugalogic.ru/", targetBlank)("kalugalogic.ru")
  )

  val logo = raw:
    """<svg class="lichess-logo-svg" style="stroke:none" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 5 5"><g fill="#e9c205"><rect x="2" y="0" width="1" height="1"/><rect x="1" y="1" width="1" height="1"/><rect x="0" y="2" width="1" height="1"/><rect x="2" y="2" width="1" height="1"/><rect x="4" y="2" width="1" height="1"/><rect x="1" y="3" width="1" height="1"/><rect x="3" y="3" width="1" height="1"/><rect x="2" y="4" width="1" height="1"/></g><rect x="3" y="1" width="1" height="1" fill="#d4361b"/></svg>"""
