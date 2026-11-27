package lila.web

// Памятка «Турниры каждую неделю» на главной площадки «Логик».
// Админ задаёт её на /dev/logic-colors, по строке на турнир:
//   Сб. 16:30 швейцарка, 10+2, 7 туров
// Всё до времени включительно — когда, остальное — что за турнир. Пустая настройка прячет блок.
object LogicWeekly:

  case class Row(when: String, what: String)

  val default =
    """Сб. 16:30 швейцарка, 10+2, 7 туров
      |Вс. 11:00 арена, дебют, 5+3, 90 минут
      |Вс. 18:00 онлайн-кубок, лично-командный, 5+3, 90 минут""".stripMargin

  private val withTime = """^(.*?\d{1,2}:\d{2})[\s,—–-]*(.*)$""".r

  def clean(text: String): String =
    text.linesIterator.map(_.trim).filter(_.nonEmpty).take(20).mkString("\n").take(2000)

  def parse(text: String): List[Row] =
    clean(text).linesIterator.toList.map:
      case withTime(when, what) => Row(when.trim, what.capitalize)
      case line => Row(line, "")
