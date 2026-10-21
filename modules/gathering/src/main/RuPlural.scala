package lila.gathering

// Русские формы числительных для зашитых в код подписей (форк одноязычный).
object RuPlural:

  def apply(n: Int, one: String, few: String, many: String): String =
    val m100 = n.abs % 100
    val m10 = m100 % 10
    val word =
      if m100 >= 11 && m100 <= 14 then many
      else if m10 == 1 then one
      else if m10 >= 2 && m10 <= 4 then few
      else many
    s"$n $word"

  def minutes(n: Int) = apply(n, "минута", "минуты", "минут")
  def seconds(n: Int) = apply(n, "секунда", "секунды", "секунд")
  def hours(n: Int) = apply(n, "час", "часа", "часов")
  def days(n: Int) = apply(n, "день", "дня", "дней")
  def months(n: Int) = apply(n, "месяц", "месяца", "месяцев")
  def years(n: Int) = apply(n, "год", "года", "лет")
  def ratedGames(n: Int) = apply(n, "рейтинговая партия", "рейтинговые партии", "рейтинговых партий")
