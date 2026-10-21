package lila.insight

enum InsightPosition(val tellNumber: String, val short: String):

  case Game extends InsightPosition("Число партий", "games")
  case Move extends InsightPosition("Число ходов", "moves")

  def name = toString.toLowerCase
