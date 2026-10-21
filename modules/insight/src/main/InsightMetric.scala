package lila.insight

import reactivemongo.api.bson.*

import InsightMetric.DataType.*
import InsightPosition.*
import InsightEntry.BSONFields as F

enum InsightMetric(
    val key: String,
    val name: String,
    val dbKey: String,
    val position: InsightPosition,
    val per: InsightPosition,
    val dataType: InsightMetric.DataType,
    val description: String
):

  case MeanCpl
      extends InsightMetric(
        "acpl",
        "Средняя потеря сантипешек",
        F.moves("c"),
        Move,
        Move,
        Average,
        """Точность ваших ходов. Чем меньше, тем лучше."""
      )

  case CplBucket
      extends InsightMetric(
        "cplBucket",
        "Группы потерь сантипешек",
        F.moves("c"),
        Move,
        Move,
        Percent,
        InsightDimension.CplRange.description
      )

  case MeanAccuracy
      extends InsightMetric(
        "accuracy",
        "Точность",
        F.moves("a"),
        Move,
        Move,
        Percent,
        InsightDimension.AccuracyPercentRange.description
      )

  case Movetime
      extends InsightMetric(
        "movetime",
        "Время на ход",
        F.moves("t"),
        Move,
        Move,
        Seconds,
        InsightDimension.MovetimeRange.description
      )

  case Result
      extends InsightMetric(
        "result",
        "Game result", // ui/insight/src/chart.ts сверяет это имя
        F.result,
        Game,
        Game,
        Percent,
        InsightDimension.Result.description
      )

  case Termination
      extends InsightMetric(
        "termination",
        "Окончание партии",
        F.termination,
        Game,
        Game,
        Percent,
        InsightDimension.Termination.description
      )

  case Performance
      extends InsightMetric(
        "performance",
        "Перформанс",
        F.opponentRating,
        Game,
        Game,
        Average,
        "Расчётный перформанс-рейтинг."
      )

  case RatingDiff
      extends InsightMetric(
        "ratingDiff",
        "Изменение рейтинга",
        F.ratingDiff,
        Game,
        Game,
        Average,
        "Сколько очков рейтинга вы получаете или теряете после партии."
      )

  case OpponentRating
      extends InsightMetric(
        "opponentRating",
        "Рейтинг соперника",
        F.opponentRating,
        Game,
        Game,
        Average,
        "Средний рейтинг соперника в этом виде игры."
      )

  case NbMoves
      extends InsightMetric(
        "nbMoves",
        "Ходов за партию",
        F.moves("r"),
        Move,
        Game,
        Average,
        "Сколько ходов вы делаете за партию, без ходов соперника."
      )

  case PieceRole
      extends InsightMetric(
        "piece",
        "Фигура, которой сделан ход",
        F.moves("r"),
        Move,
        Move,
        Percent,
        InsightDimension.PieceRole.description
      )

  case Awareness
      extends InsightMetric(
        "awareness",
        "Тактическая зоркость",
        F.moves("o"),
        Move,
        Move,
        Percent,
        "Как часто вы пользуетесь ошибками соперника."
      )

  case Luck
      extends InsightMetric(
        "luck",
        "Везение",
        F.moves("l"),
        Move,
        Move,
        Percent,
        "Как часто соперник не наказывает ваши ошибки."
      )

  case Material
      extends InsightMetric(
        "material",
        "Материальный перевес",
        F.moves("i"),
        Move,
        Move,
        Average,
        InsightDimension.MaterialRange.description
      )

  case ClockPercent
      extends InsightMetric(
        "clockPercent",
        "Цейтнот",
        F.moves("s"),
        Move,
        Move,
        Average,
        InsightDimension.ClockPercentRange.description
      )

  case Blurs
      extends InsightMetric(
        "blurs",
        "Уходы из окна",
        F.moves("b"),
        Move,
        Move,
        Percent,
        "Как часто вы уходите из окна игры перед ходом."
      )

  case TimeVariance
      extends InsightMetric(
        "timeVariance",
        "Разброс времени",
        F.moves("v"),
        Move,
        Move,
        Average,
        "Малый разброс значит, что вы тратите на ходы примерно одинаковое время"
      )

object InsightMetric:

  enum DataType:
    case Seconds, Count, Average, Percent
    def name = toString.toLowerCase

  val byKey = values.mapBy(_.key)

  val requiresAnalysis = Set(MeanCpl, CplBucket, MeanAccuracy, Awareness)
  val requiresStableRating = Set(Performance, RatingDiff, OpponentRating)
  val isStacked = Set(Result, Termination, PieceRole, CplBucket)

  def valuesOf(metric: InsightMetric): Seq[MetricValue] = metric match
    case Result =>
      lila.insight.Result.values.map { r =>
        MetricValue(BSONInteger(r.id), MetricValueName(r.name))
      }.toIndexedSeq
    case Termination =>
      lila.insight.Termination.values.map { r =>
        MetricValue(BSONInteger(r.id), MetricValueName(r.name))
      }.toIndexedSeq
    case PieceRole =>
      chess.Role.all.reverse.map { r =>
        MetricValue(BSONString(r.forsyth.toString), MetricValueName(r.toString))
      }
    case CplBucket =>
      lila.insight.CplRange.all.map { cpl =>
        MetricValue(BSONInteger(cpl.cpl), MetricValueName(cpl.name))
      }
    case _ => Nil

  opaque type MetricValueName = String
  object MetricValueName extends OpaqueString[MetricValueName]

  case class MetricValue(key: BSONValue, name: MetricValueName)
