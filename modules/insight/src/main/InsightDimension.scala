package lila.insight

import chess.Role
import chess.eval.WinPercent
import play.api.libs.json.*
import reactivemongo.api.bson.*

import lila.analyse.AccuracyPercent
import lila.common.Json.given
import lila.common.{ LilaOpeningFamily, SimpleOpening }
import lila.core.i18n.Translate
import lila.db.dsl.{ *, given }
import lila.insight.BSONHandlers.given
import lila.insight.InsightEntry.BSONFields as F
import lila.rating.BSONHandlers.perfTypeIdHandler
import lila.game.BSONHandlers.sourceHandler
import lila.rating.PerfType
import lila.core.game.Source

enum InsightDimension[A](
    val key: String,
    val name: String,
    val dbKey: String,
    val position: InsightPosition,
    val description: String
)(using val bson: BSONHandler[A]):
  def isInGame = position == InsightPosition.Game
  def isInMove = position == InsightPosition.Move

  case Period
      extends InsightDimension[Period](
        "period",
        "Дата",
        F.date,
        InsightPosition.Game,
        "Дата, когда сыграна партия"
      )

  case Date
      extends InsightDimension[lila.insight.DateRange](
        "date",
        "Дата",
        F.date,
        InsightPosition.Game,
        "Дата, когда сыграна партия"
      )

  case Perf
      extends InsightDimension[PerfType](
        "variant",
        "Вариант",
        F.perf,
        InsightPosition.Game,
        "Рейтинговая категория партии: пуля, блиц, шахматы 960 и т. д."
      )

  case Phase
      extends InsightDimension[Phase](
        "phase",
        "Стадия партии",
        F.moves("p"),
        InsightPosition.Move,
        "Часть партии: дебют, миттельшпиль или эндшпиль."
      )

  case Result
      extends InsightDimension[Result](
        "result",
        "Результат партии",
        F.result,
        InsightPosition.Game,
        "Выиграли вы, проиграли или сыграли вничью."
      )

  case Termination
      extends InsightDimension[Termination](
        "termination",
        "Окончание партии",
        F.termination,
        InsightPosition.Game,
        "Как закончилась партия: мат, сдача и т. д."
      )

  case Color
      extends InsightDimension[Color](
        "color",
        "Цвет",
        F.color,
        InsightPosition.Game,
        "За кого вы играете: белые или чёрные."
      )

  case OpeningFamily
      extends InsightDimension[LilaOpeningFamily](
        "openingFamily",
        "Дебют",
        F.openingFamily,
        InsightPosition.Game,
        "Дебют в целом, например «Сицилианская защита»."
      )

  case OpeningVariation
      extends InsightDimension[SimpleOpening](
        "openingVariation",
        "Вариант дебюта",
        F.opening,
        InsightPosition.Game,
        "Точный вариант дебюта, например «Сицилианская защита: вариант Найдорфа»."
      )

  case OpponentStrength
      extends InsightDimension[RelativeStrength](
        "opponentStrength",
        "Сила соперника",
        F.opponentStrength,
        InsightPosition.Game,
        "Рейтинг соперника по сравнению с вашим. Намного слабее: −200, слабее: −100, " +
          "сильнее: +100, намного сильнее: +200."
      )

  case PieceRole
      extends InsightDimension[Role](
        "piece",
        "Фигура, которой сделан ход",
        F.moves("r"),
        InsightPosition.Move,
        "Какой фигурой вы ходите."
      )

  case MovetimeRange
      extends InsightDimension[MovetimeRange](
        "movetime",
        "Время на ход",
        F.moves("t"),
        InsightPosition.Move,
        "Сколько секунд вы думаете над каждым ходом."
      )

  case MyCastling
      extends InsightDimension[Castling](
        "myCastling",
        "Моя рокировка",
        F.myCastling,
        InsightPosition.Game,
        "В какую сторону вы рокировали: короткая, длинная или без рокировки."
      )

  case OpCastling
      extends InsightDimension[Castling](
        "opCastling",
        "Рокировка соперника",
        F.opponentCastling,
        InsightPosition.Game,
        "В какую сторону рокировал соперник: короткая, длинная или без рокировки."
      )

  case QueenTrade
      extends InsightDimension[QueenTrade](
        "queenTrade",
        "Размен ферзей",
        F.queenTrade,
        InsightPosition.Game,
        "Разменялись ли ферзи до эндшпиля."
      )

  case MaterialRange
      extends InsightDimension[MaterialRange](
        "material",
        "Материальный перевес",
        F.moves("i"),
        InsightPosition.Move,
        "Ценность ваших фигур по сравнению с фигурами соперника. Пешка = 1, слон/конь = 3, " +
          "ладья = 5, ферзь = 9."
      )

  case Blur
      extends InsightDimension[Blur](
        "blur",
        "Уход из окна",
        F.moves("b"),
        InsightPosition.Move,
        "Уходили ли вы из окна игры перед этим ходом."
      )

  case TimeVariance
      extends InsightDimension[TimeVariance](
        "timeVariance",
        "Разброс времени",
        F.moves("v"),
        InsightPosition.Move,
        "Разброс времени на ход. Очень ровно: < 0.25, ровно: < 0.4, средне, " +
          "неровно: > 0.6, очень неровно: > 0.75."
      )

  case CplRange
      extends InsightDimension[CplRange](
        "cpl",
        "Потеря сантипешек",
        F.moves("c"),
        InsightPosition.Move,
        "Сколько сантипешек теряет каждый ход по оценке Stockfish."
      )

  case EvalRange
      extends InsightDimension[EvalRange](
        "eval",
        "Оценка",
        F.moves("e"),
        InsightPosition.Move,
        "Оценка позиции Stockfish для игрока, в сантипешках."
      )

  case AccuracyPercentRange
      extends InsightDimension[AccuracyPercentRange](
        "accuracy",
        "Точность",
        F.moves("a"),
        InsightPosition.Move,
        """Насколько точны ваши ходы по оценке Stockfish."""
      )

  case WinPercentRange
      extends InsightDimension[WinPercentRange](
        "winPercent",
        "Шансы на победу",
        F.moves("w"),
        InsightPosition.Move,
        "Шансы выиграть позицию по оценке Stockfish, они же Win%."
      )

  case ClockPercentRange
      extends InsightDimension[ClockPercentRange](
        "clockPercent",
        "Цейтнот",
        F.moves("s"),
        InsightPosition.Move,
        "Сколько времени осталось на часах с учётом добавления. 100% — всё время, 0% — флажок упал."
      )

  case GameSource
      extends InsightDimension[Source](
        "source",
        "Источник партии",
        F.source,
        InsightPosition.Game,
        "Как была создана партия."
      )

object InsightDimension:

  def requiresStableRating(d: InsightDimension[?]) = d match
    case OpponentStrength => true
    case _ => false

  def valuesOf[X](d: InsightDimension[X]): Seq[X] = d match
    case Period => lila.insight.Period.selector
    case Date => Nil // Period is used instead
    case Perf => lila.rating.PerfType.nonPuzzle
    case Phase => lila.insight.Phase.values.toIndexedSeq
    case Result => lila.insight.Result.values.toIndexedSeq
    case Termination => lila.insight.Termination.values.toIndexedSeq
    case Color => lila.insight.Color.values.toIndexedSeq
    case OpeningFamily => LilaOpeningFamily.familyList
    case OpeningVariation => SimpleOpening.openingList
    case OpponentStrength => RelativeStrength.values.toIndexedSeq
    case PieceRole => chess.Role.all.reverse
    case MovetimeRange => lila.insight.MovetimeRange.values.toIndexedSeq
    case CplRange => lila.insight.CplRange.all
    case AccuracyPercentRange => lila.insight.AccuracyPercentRange.all.toList
    case MyCastling => lila.insight.Castling.values.toIndexedSeq
    case OpCastling => lila.insight.Castling.values.toIndexedSeq
    case QueenTrade => lila.insight.QueenTrade.values.toIndexedSeq
    case MaterialRange => lila.insight.MaterialRange.values.toIndexedSeq
    case EvalRange => lila.insight.EvalRange.values.toIndexedSeq
    case WinPercentRange => lila.insight.WinPercentRange.all.toList
    case ClockPercentRange => lila.insight.ClockPercentRange.all.toList
    case Blur => lila.insight.Blur.values.toIndexedSeq
    case TimeVariance => lila.insight.TimeVariance.values.toIndexedSeq
    case GameSource =>
      Source.values.toIndexedSeq.filter:
        case Source.Ai | Source.Import | Source.ImportLive => false
        case _ => true

  def valueByKey[X](d: InsightDimension[X], key: String): Option[X] = d match
    case Period => key.toIntOption.map(lila.insight.Period.apply)
    case Date => None
    case Perf => PerfKey(key).map(PerfType.apply)
    case Phase => key.toIntOption.flatMap(lila.insight.Phase.byId.get)
    case Result => key.toIntOption.flatMap(lila.insight.Result.byId.get)
    case Termination => key.toIntOption.flatMap(lila.insight.Termination.byId.get)
    case Color => lila.insight.Color.fromName(key)
    case OpeningFamily => LilaOpeningFamily.find(key)
    case OpeningVariation => SimpleOpening.find(key)
    case OpponentStrength => key.toIntOption.flatMap(RelativeStrength.byId.get)
    case PieceRole => chess.Role.all.find(_.name == key)
    case MovetimeRange => key.toIntOption.flatMap(lila.insight.MovetimeRange.byId.get)
    case CplRange => key.toIntOption.flatMap(lila.insight.CplRange.byId.get)
    case AccuracyPercentRange => key.toIntOption.flatMap(lila.insight.AccuracyPercentRange.byPercent.get)
    case MyCastling => key.toIntOption.flatMap(lila.insight.Castling.byId.get)
    case OpCastling => key.toIntOption.flatMap(lila.insight.Castling.byId.get)
    case QueenTrade => lila.insight.QueenTrade(key == "true").some
    case MaterialRange => key.toIntOption.flatMap(lila.insight.MaterialRange.byId.get)
    case EvalRange => key.toIntOption.flatMap(lila.insight.EvalRange.byId.get)
    case WinPercentRange => key.toIntOption.flatMap(lila.insight.WinPercentRange.byPercent.get)
    case ClockPercentRange => key.toIntOption.flatMap(lila.insight.ClockPercentRange.byPercent.get)
    case Blur => lila.insight.Blur(key == "true").some
    case TimeVariance => key.toFloatOption.map(lila.insight.TimeVariance.byId)
    case GameSource => Source.byName.get(key)

  def valueToJson[X](d: InsightDimension[X])(v: X)(using Translate): JsObject =
    Json.obj(
      "key" -> valueKey(d)(v),
      "name" -> valueJson(d)(v)
    )

  def valueKey[X](d: InsightDimension[X])(v: X): String =
    d.match
      case Date => v.toString
      case Period => v.days.toString
      case Perf => v.key
      case Phase => v.id
      case Result => v.id
      case Termination => v.id
      case Color => v.name
      case OpeningFamily => v.key
      case OpeningVariation => v.key
      case OpponentStrength => v.id
      case PieceRole => v.name
      case MovetimeRange => v.id
      case CplRange => v.cpl
      case AccuracyPercentRange => v.bottom.toInt
      case MyCastling => v.id
      case OpCastling => v.id
      case QueenTrade => v.id
      case MaterialRange => v.id
      case EvalRange => v.id
      case WinPercentRange => v.bottom.toInt
      case ClockPercentRange => v.bottom.toInt
      case Blur => v.id
      case TimeVariance => v.id
      case GameSource => v.name
    .toString

  def valueJson[X](d: InsightDimension[X])(v: X)(using Translate): JsValue =
    d match
      case Date => JsNumber(v.min.toSeconds)
      case Period => JsString(v.toString)
      case Perf => JsString(v.trans)
      case Phase => JsString(v.name)
      case Result => JsString(v.name)
      case Termination => JsString(v.name)
      case Color => JsString(v.toString)
      case OpeningFamily => Json.toJson(v.name)
      case OpeningVariation => Json.toJson(v.name)
      case OpponentStrength => JsString(v.name)
      case PieceRole => JsString(v.toString)
      case MovetimeRange => JsString(v.name)
      case CplRange => JsString(v.name)
      case AccuracyPercentRange => JsString(v.name)
      case MyCastling => JsString(v.name)
      case OpCastling => JsString(v.name)
      case QueenTrade => JsString(v.name)
      case MaterialRange => JsString(v.name)
      case EvalRange => JsString(v.name)
      case WinPercentRange => JsString(v.name)
      case ClockPercentRange => JsString(v.name)
      case Blur => JsString(v.name)
      case TimeVariance => JsString(v.name)
      case GameSource => JsString(v.toString)

  def filtersOf[X](d: InsightDimension[X], selected: List[X]): Bdoc =

    def percentRange[V: BSONWriter](toRange: X => (V, V), fromPercent: Int => V) = selected match
      case Nil => emptyBdoc
      case many =>
        bdoc(
          "$or" -> many.map(toRange).map {
            case (min, max) if min == fromPercent(0) => bdoc(d.dbKey.lt(max))
            case (min, max) if max == fromPercent(100) => bdoc(d.dbKey.gte(min)) // hole at 90%? #TODO
            case (min, max) => bdoc(d.dbKey.gte(min).lt(max))
          }
        )
    d match
      case InsightDimension.MovetimeRange =>
        selected match
          case Nil => emptyBdoc
          case many =>
            bdoc(
              "$or" -> many.map(lila.insight.MovetimeRange.toRange).map { range =>
                bdoc(d.dbKey.gte(range._1).lt(range._2))
              }
            )
      case InsightDimension.Period =>
        selected.maximumByOption(_.days).fold(emptyBdoc) { period =>
          bdoc(d.dbKey.gt(period.min))
        }
      case InsightDimension.Date => // for tutor config date range filtering
        selected match
          case List(range) => bdoc(d.dbKey.gt(range.min).lte(range.max))
          case _ => emptyBdoc
      case InsightDimension.MaterialRange =>
        selected match
          case Nil => emptyBdoc
          case many =>
            bdoc(
              "$or" -> many.map { range =>
                val intRange = lila.insight.MaterialRange.toRange(range)
                if intRange._1 == intRange._2 then bdoc(d.dbKey -> intRange._1)
                else if range.negative then bdoc(d.dbKey.gte(intRange._1).lt(intRange._2))
                else bdoc(d.dbKey.gt(intRange._1).lte(intRange._2))
              }
            )
      case InsightDimension.EvalRange =>
        selected match
          case Nil => emptyBdoc
          case many =>
            bdoc(
              "$or" -> many.map { range =>
                val intRange = lila.insight.EvalRange.toRange(range)
                if range.eval < 0 then bdoc(d.dbKey.gte(intRange._1).lt(intRange._2))
                else bdoc(d.dbKey.gt(intRange._1).lte(intRange._2))
              }
            )
      case InsightDimension.WinPercentRange =>
        percentRange(lila.insight.WinPercentRange.toRange, WinPercent.fromPercent(_))
      case InsightDimension.AccuracyPercentRange =>
        percentRange(lila.insight.AccuracyPercentRange.toRange, AccuracyPercent.fromPercent(_))
      case InsightDimension.ClockPercentRange =>
        percentRange(lila.insight.ClockPercentRange.toRange, ClockPercent.fromPercent(_))
      case InsightDimension.TimeVariance =>
        selected match
          case Nil => emptyBdoc
          case many =>
            bdoc(
              "$or" -> many.map(lila.insight.TimeVariance.toRange).map { range =>
                bdoc(d.dbKey.gt(range._1).lte(range._2))
              }
            )
      case _ =>
        selected.flatMap(d.bson.writeOpt) match
          case Nil => emptyBdoc
          case List(x) => bdoc(d.dbKey -> x)
          case xs => d.dbKey.in(xs)

  def requiresAnalysis(d: InsightDimension[?]) =
    d match
      case CplRange => true
      case AccuracyPercentRange => true
      case EvalRange => true
      case WinPercentRange => true
      case _ => false

  def dataTypeOf[X](d: InsightDimension[X]): String =
    d match
      case Date => "date"
      case _ => "text"

  // these are not always present in an insight entry
  val optionalDimensions = List[InsightDimension[?]](OpeningFamily, OpeningVariation, OpponentStrength)
