package lila.clas

import chess.opening.{ OpeningDb, OpeningFamily }

import lila.core.game.Game

// «Дебюты клуба» (Логик): дебюты учеников по их партиям, без индекса insights
object ClasOpenings:

  case class Row(
      family: OpeningFamily,
      games: Int,
      wins: Int,
      draws: Int,
      losses: Int,
      players: List[(Student, Int)]
  ):
    def score = (wins * 2 + draws) * 50 / games

  case class Stats(white: List[Row], black: List[Row], nbGames: Int, nbPlayers: Int)

  private case class Pov(color: Color, family: OpeningFamily, student: Student, win: Option[Boolean])

  def compute(games: List[Game], students: Map[UserId, Student]): Stats =
    val povs = games.flatMap: g =>
      val family = if g.fromPosition then none else OpeningDb.search(g.sans).map(_.opening.family)
      for
        fam <- family.toList
        color <- Color.all
        student <- g.player(color).userId.flatMap(students.get).toList
      yield Pov(color, fam, student, g.winnerColor.map(_ == color))
    def rows(color: Color) =
      povs
        .filter(_.color == color)
        .groupBy(_.family.key)
        .values
        .toList
        .map: ps =>
          Row(
            family = ps.head.family,
            games = ps.size,
            wins = ps.count(_.win.contains(true)),
            draws = ps.count(_.win.isEmpty),
            losses = ps.count(_.win.contains(false)),
            players = ps
              .groupBy(_.student.userId)
              .values
              .toList
              .map(l => l.head.student -> l.size)
              .sortBy((s, nb) => (-nb, s.realName.value))
          )
        .sortBy(r => (-r.games, r.family.name.value))
    Stats(rows(Color.white), rows(Color.black), games.size, povs.map(_.student.userId).distinct.size)
