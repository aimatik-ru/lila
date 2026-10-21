package lila.web

// Цвета площадки «Логик», которые админ задаёт на /dev/logic-colors.
// Пустая строка в настройке = цвет из темы (тёплый фон и зелёные кнопки).
object LogicColors:

  val defaultBg = "#f1f0ec"
  val defaultPrimary = "#82b74e"

  private val hexRe = "^#[0-9a-f]{6}$".r

  def clean(s: String): Option[String] = Some(s.trim.toLowerCase).filter(hexRe.matches)

  def hue(hex: String): Int =
    def ch(i: Int) = Integer.parseInt(hex.substring(i, i + 2), 16) / 255d
    val (r, g, b) = (ch(1), ch(3), ch(5))
    val max = r.max(g).max(b)
    val d = max - r.min(g).min(b)
    if d == 0 then 0
    else
      val h =
        if max == r then ((g - b) / d) % 6
        else if max == g then (b - r) / d + 2
        else (r - g) / d + 4
      ((math.round(h * 60).toInt % 360) + 360) % 360

  def lightness(hex: String): Double =
    def ch(i: Int) = Integer.parseInt(hex.substring(i, i + 2), 16) / 255d
    val (r, g, b) = (ch(1), ch(3), ch(5))
    (r.max(g).max(b) + r.min(g).min(b)) / 2

  // Фон задаёт оттенок всей гаммы (---site-hue) и точный цвет страницы в светлой теме.
  // Тёмный цвет кнопок (чёрный, тёмно-синий) на тёмной теме не виден — там остаётся цвет темы.
  def css(bg: String, primary: String): Option[String] =
    val parts = List(
      clean(bg).map: c =>
        s":root{---site-hue:${hue(c)}deg}" +
          s"html.light{--c-bg-page:$c;--c-body-gradient:hsl(from $c h s calc(l - 6))}",
      clean(primary).map: c =>
        val dark =
          if lightness(c) >= 0.3 then s":root:not(.light){--c-primary:hsl(from $c h s calc(l + 10))}"
          else ""
        s"html.light{--c-primary:$c}$dark"
    ).flatten
    Option.when(parts.nonEmpty)(parts.mkString)
