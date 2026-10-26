package lila.rating

import chess.{ ByColor, IntRating, Outcome }
import chess.rating.glicko.Glicko

// logic: game ratings follow the Russian Chess Federation (FSHR) rules from EVSK:
// R = Rn + (N - PD) * K, PD from the conversion table, K by rating range, floor 1000.
// Deviation and volatility still come from Glicko-2 (provisional "?" and rankings).
object RuRating:

  // upper bounds of the rating difference for PD 0.50, 0.51 ... 0.91; above them PD is 0.92
  private val diffBounds = Vector(3, 10, 17, 25, 32, 39, 46, 53, 61, 68, 76, 83, 91, 98, 106, 113, 121, 129,
    137, 145, 153, 162, 170, 179, 188, 197, 206, 215, 225, 235, 245, 256, 267, 278, 290, 302, 315, 328, 344,
    357, 374, 391)

  // expected score of the higher rated player, in hundredths
  private def higherPd(diff: Int): Int =
    val i = diffBounds.indexWhere(diff <= _)
    50 + (if i < 0 then diffBounds.size else i)

  def expected(own: IntRating, opponent: IntRating): Double =
    val pd = higherPd((own.value - opponent.value).abs)
    (if own.value >= opponent.value then pd else 100 - pd) / 100d

  def k(rating: IntRating): Int =
    val r = rating.value
    if r < 1200 then 60
    else if r < 1400 then 50
    else if r < 1600 then 40
    else if r < 1800 then 35
    else if r < 2000 then 30
    else if r < 2200 then 25
    else if r < 2400 then 20
    else 10

  def next(own: IntRating, opponent: IntRating, score: Double): IntRating =
    val delta = Math.round(k(own) * (score - expected(own, opponent))).toInt
    IntRating(own.value + delta).atLeast(lila.rating.Glicko.playerMinRating)

  def apply(before: ByColor[Glicko], after: ByColor[Glicko], outcome: Outcome): ByColor[Glicko] =
    val ratings = before.map(_.intRating)
    after.mapWithColor: (color, glicko) =>
      val score = outcome.winner.fold(0.5)(w => if w == color then 1d else 0d)
      glicko.copy(rating = next(ratings(color), ratings(!color), score).value.toDouble)
