package views.base

import lila.app.UiEnv.{ *, given }

def notFound(msg: Option[String]) =
  Page(msg | "Страница не найдена").css("bits.not-found"):
    main(cls := "not-found page-small box box-pad")(
      header(
        h1("404"),
        div(
          strong("Страница не найдена!"),
          msg.map(em(_)),
          p(
            "Вернитесь ",
            a(href := routes.Lobby.home)("на главную"),
            span(cls := "or-play")(" или сыграйте в эту мини-игру")
          )
        )
      ),
      div(cls := "game")(
        iframe(
          src := staticAssetUrl(s"vendor/ChessPursuit/bin-release/index.html"),
          st.frameborder := 0,
          widthA := 400,
          heightA := 500,
          frame.credentialless
        ),
        p(cls := "credits")(
          a(href := "https://github.com/Saturnyn/ChessPursuit")("ChessPursuit"),
          " — автор ",
          a(href := "https://github.com/Saturnyn")("Saturnyn")
        )
      )
    )

def notFoundEmbed(msg: Option[String])(using EmbedContext) =
  views.base.embed.site(title = msg | "Страница не найдена", cssKeys = List("bits.not-found-embed"))(
    main(cls := "not-found page-small box box-pad")(
      header(
        h1("404"),
        div(
          strong("Страница не найдена!"),
          msg.map(em(_))
        )
      )
    )
  )
