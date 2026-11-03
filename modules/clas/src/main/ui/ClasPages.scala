package lila.clas
package ui

import play.api.data.Form

import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class ClasPages(helpers: Helpers, clasUi: ClasUi, dashUi: DashboardUi):
  import helpers.{ *, given }
  import clasUi.ClasPage

  def teacherIndex(classes: List[Clas], closed: Boolean)(using Context) =
    val (active, archived) = classes.partition(_.isActive)
    val (current, others) = if closed then (archived, active) else (active, archived)
    ClasPage(trans.clas.lichessClasses.txt(), Right("classes"))(cls := "clas-index"):
      frag(
        div(cls := "box__top")(
          h1(trans.clas.lichessClasses()),
          a(
            href := routes.Clas.form,
            cls := "new button button-empty",
            title := trans.clas.newClass.txt(),
            dataIcon := Icon.PlusButton
          )
        ),
        if current.isEmpty then frag(hr, p(cls := "box__pad classes__empty")(trans.clas.noClassesYet()))
        else renderClasses(current),
        (closed || others.nonEmpty).option(
          div(cls := "clas-index__others")(
            a(href := s"${routes.Clas.index}?closed=${!closed}")(
              if closed then "Активные классы: " else "Классы в архиве: ",
              others.size.localize
            )
          )
        )
      )

  def studentIndex(classes: List[Clas])(using Context) =
    ClasPage(trans.clas.lichessClasses.txt(), Right("classes"))(cls := "clas-index"):
      frag(
        boxTop(h1(trans.clas.lichessClasses())),
        renderClasses(classes)
      )

  // Логик: дебюты учеников всех групп тренера или одной группы
  def openings(classes: List[Clas], selected: Option[Clas], stats: ClasOpenings.Stats)(using Context) =
    def insights(s: Student, color: Color, row: ClasOpenings.Row) =
      routes.Insight.path(
        s.userId.into(UserStr),
        "result",
        "openingVariation",
        s"color:${color.name}/openingFamily:${row.family.key.value}"
      )
    def colorTable(color: Color, rows: List[ClasOpenings.Row]) =
      st.section(cls := "clas-openings__color")(
        h2(cls := "box__pad")(if color.white then "Белыми" else "Чёрными"),
        if rows.isEmpty then p(cls := "box__pad")("Партий пока нет.")
        else
          table(cls := "slist slist-pad")(
            thead(
              tr(
                th("Дебют"),
                th("Партий"),
                th(title := "Победы / ничьи / поражения")("+ / = / −"),
                th("Очки"),
                th("Кто играет (партий)")
              )
            ),
            tbody(
              rows.map: row =>
                tr(
                  td(row.family.name),
                  td(row.games),
                  td(s"${row.wins} / ${row.draws} / ${row.losses}"),
                  td(s"${row.score}%"),
                  td(
                    row.players.zipWithIndex.map { case ((s, nb), i) =>
                      frag(
                        (i > 0).option(", "),
                        a(href := insights(s, color, row))(s.realName),
                        s" ($nb)"
                      )
                    }
                  )
                )
            )
          )
      )
    ClasPage("Дебюты клуба", Right("openings"))(cls := "clas-openings"):
      frag(
        boxTop(h1("Дебюты клуба")),
        div(cls := "box__pad")(
          p(
            "По завершённым партиям учеников в обычные шахматы. Партий: ",
            stats.nbGames.localize,
            ", учеников с партиями: ",
            stats.nbPlayers.localize,
            ". Имя ученика открывает его варианты в этом дебюте."
          ),
          form(method := "get", action := routes.Clas.index)(
            input(tpe := "hidden", name := "openings", value := "1"),
            st.select(name := "clas")(
              st.option(value := "")("Все группы"),
              classes.map: c =>
                st.option(value := c.id.value, selected.exists(_.id == c.id).option(st.selected))(c.name)
            ),
            " ",
            submitButton(cls := "button button-empty")("Показать")
          )
        ),
        colorTable(Color.white, stats.white),
        colorTable(Color.black, stats.black)
      )

  private def renderClasses(classes: List[Clas]) =
    div(cls := "classes")(
      classes.map { clas =>
        div(
          cls := List("clas-widget" -> true, "clas-widget-archived" -> clas.isArchived),
          dataIcon := Icon.Group
        )(
          a(cls := "overlay", href := routes.Clas.show(clas.id)),
          div(
            h3(clas.name),
            p(clas.desc)
          )
        )
      }
    )

  def create(form: Form[ClasForm.ClasData])(using Context) =
    ClasPage(trans.clas.newClass.txt(), Right("newClass"))(cls := "clas-create"):
      frag(
        div(cls := "box-pad box__top")(
          h1(trans.clas.newClass())
        ),
        postForm(cls := "form3 box-pad", action := routes.Clas.create)(
          clasForm(form, none),
          form3.actions(
            a(href := routes.Clas.index)(trans.site.cancel()),
            form3.submit(trans.site.apply())
          )
        )
      )

  def edit(c: lila.clas.Clas, students: List[Student.WithUser], form: Form[ClasForm.ClasData])(using
      Context
  ) =
    dashUi.teacher.TeacherPage(c, students, "edit")()(
      div(cls := "box-pad")(
        postForm(cls := "form3 clas-edit", action := routes.Clas.update(c.id))(
          clasForm(form, c.some),
          form3.actions(
            a(href := routes.Clas.show(c.id))(trans.site.cancel()),
            form3.submit(trans.site.apply())
          )
        ),
        hr,
        c.isActive.option(
          postForm(
            action := routes.Clas.archive(c.id, v = true),
            cls := "clas-edit__archive"
          ):
            form3.submit(trans.clas.closeClass(), icon = none)(
              cls := "yes-no-confirm button-red button-empty"
            )
        )
      )
    )

  private def clasForm(form: Form[ClasForm.ClasData], clas: Option[Clas])(using ctx: Context) =
    frag(
      form3.globalError(form),
      form3.group(form("name"), trans.clas.className())(form3.input(_)(autofocus)),
      form3.group(
        form("desc"),
        frag(trans.clas.classDescription()),
        help = trans.clas.visibleByBothStudentsAndTeachers().some
      )(form3.textarea(_)(rows := 5)),
      clas match
        case None => form3.hidden(form("teachers"), UserId.raw(ctx.userId))
        case Some(_) =>
          form3.group(
            form("teachers"),
            trans.clas.teachersOfTheClass(),
            help = trans.clas.addLichessUsernames().some
          )(form3.textarea(_)(rows := 4)),
      form3.checkboxGroup(
        form("canMsg"),
        frag(trans.clas.allowMessagingBetweenStudents()),
        help = trans.clas.allowMessagingBetweenStudentsDesc().some
      ),
      form3.checkboxGroup(
        form("hasTeam"),
        frag("Создать команду «Логик» для этого класса"),
        help = frag(
          "Команды «Логик» могут проводить турниры. Ученики класса автоматически " +
            "вступят в команду и её турниры."
        ).some
      )(id := "clas-team")
    )
