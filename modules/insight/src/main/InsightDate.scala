package lila.insight

case class DateRange(min: Instant, max: Instant)

case class Period(days: Int):
  def max = nowInstant
  def min = max.minusDays(days)

  override def toString =
    days match
      case 1 => "Последние 24 часа"
      case d if d < 14 => s"Последние дни: $d"
      case d if d == 14 => s"Последняя неделя"
      case d if d < 30 => s"Последние недели: ${d / 7}"
      case d if d == 30 => s"Последний месяц"
      case d if d < 365 => s"Последние месяцы: ${d / 30}"
      case d if d == 365 => s"Последний год"
      case d => s"Последние годы: ${d / 365}"

object Period:

  val selector: List[Period] = List(
    1,
    2,
    7,
    15,
    30,
    60,
    182,
    365,
    365 * 2,
    365 * 3,
    365 * 5,
    365 * 10
  ).map(Period.apply)
