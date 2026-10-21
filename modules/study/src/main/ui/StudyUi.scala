package lila.study
package ui

import lila.core.study.IdName
import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class StudyUi(helpers: Helpers):
  import helpers.{ *, given }

  def clone(s: Study)(using Context) =
    postForm(action := routes.Study.cloneApply(s.id))(
      p("Будет создана новая закрытая студия с теми же главами."),
      p("Вы станете владельцем новой студии."),
      p("Обе студии можно менять независимо друг от друга."),
      p("Удаление одной студии ", strong("не"), " удалит другую."),
      p(
        submitButton(
          cls := "submit button large text",
          dataIcon := Icon.StudyBoard,
          style := "margin: 30px auto; display: block; font-size: 2em;"
        )("Клонировать студию")
      ),
      p(
        a(href := routes.Study.show(s.id), cls := "text", dataIcon := Icon.LessThan)(trans.site.cancel())
      )
    )

  private def studyButton(s: IdName, chapterCount: Int) =
    val btn =
      if Study.maxChapters <= chapterCount then submitButton(cls := "disabled", st.disabled)
      else submitButton

    btn(name := "as", value := s.id, cls := "button button-empty study submit")(s.name)

  def create(
      data: lila.study.StudyForm.importGame.Data,
      owner: List[(IdName, Int)],
      contrib: List[(IdName, Int)]
  )(using Context) =
    div(cls := "study-create")(
      postForm(action := routes.Study.create)(
        form3.hidden("gameId", data.gameId),
        form3.hidden("orientation", data.orientation.map(_.key)),
        form3.hidden("fen", data.fen.map(_.value)),
        form3.hidden("pgn", data.pgnStr),
        form3.hidden("variant", data.variant.map(_.key)),
        form3.hidden("mode", data.mode.map(_.key)),
        h2(trans.study.whereDoYouWantToStudyThat()),
        p(
          submitButton(
            name := "as",
            value := "study",
            cls := "submit button large new text",
            dataIcon := Icon.StudyBoard
          )(trans.study.createStudy())
        ),
        div(cls := "studies")(
          div(
            h2(trans.study.myStudies()),
            if owner.nonEmpty then owner.map(studyButton)
            else p(cls := "placeholder")(trans.site.none())
          ),
          div(
            h2(trans.study.studiesIContributeTo()),
            if contrib.nonEmpty then contrib.map(studyButton)
            else p(cls := "placeholder")(trans.site.none())
          )
        )
      )
    )
