package lila.web

import chess.Replay
import chess.format.Fen
import chess.format.pgn.PgnStr
import play.api.libs.json.*

// Ловушки площадки «Логик»: опасные ходы, перед которыми игрок видит предупреждение.
// Админ задаёт их на /dev/logic-colors?traps=1, по строке на ловушку:
//   1.e4 e5 2.Кf3 Кc6 3.Кc3 Кf6 4.Сc4 | подсказка
// Последний ход строки — опасный, позиция перед ним — где показывать окно.
object LogicTraps:

  case class Trap(line: String, board: String, uci: String, hint: String)
  case class Parsed(traps: List[Trap], errors: List[String])

  val default =
    "1.e4 e5 2.Кf3 Кc6 3.Кc3 Кf6 4.Сc4 | Чёрные могут взять 4...К:e4. На 5.К:e4 будет 5...d5 с вилкой: фигура вернётся, а центр останется у чёрных. Надёжнее 4.Сb5 или 4.d4."

  // Доска и очередь хода из FEN: «rnbqkbnr/... w». Рокировки и взятие на проходе не сравниваем.
  def boardKey(fen: String): String = fen.split(' ').take(2).mkString(" ")

  private val moveNumber = "^\\d+\\.+".r

  // Русская запись в SAN: Кр→K, К→N, С→B, Л→R, Ф→Q; кириллические а/с/е/х и «:» как латиница и взятие.
  def toSan(token: String): String =
    val t = moveNumber.replaceFirstIn(token.trim, "").filterNot(c => c == '!' || c == '?')
    val piece =
      if t.startsWith("Кр") then "K" + t.drop(2)
      else
        t.headOption match
          case Some('К') => "N" + t.drop(1)
          case Some('С') => "B" + t.drop(1)
          case Some('Л') => "R" + t.drop(1)
          case Some('Ф') => "Q" + t.drop(1)
          case _ => t
    piece.map:
      case 'а' => 'a'
      case 'с' => 'c'
      case 'е' => 'e'
      case 'х' | ':' => 'x'
      case 'Х' => 'x'
      case c => c

  def sans(moves: String): List[String] =
    moves.split("\\s+").toList.map(toSan).filter(_.nonEmpty)

  private def replay(sans: List[String]) =
    for
      parsed <- Replay.mainline(PgnStr(sans.mkString(" "))).toOption
      replay <- parsed.valid.toOption
      if replay.moves.size == sans.size
    yield replay

  def parseLine(raw: String): Either[String, Trap] =
    val (movesPart, hintPart) = raw.indexOf('|') match
      case -1 => (raw, "")
      case i => (raw.take(i), raw.drop(i + 1))
    val line = movesPart.trim
    val all = sans(line)
    if all.isEmpty then Left(s"Нет ходов: $raw")
    else
      (for
        before <- replay(all.init)
        full <- replay(all)
        last <- full.moves.headOption
      yield Trap(
        line = line,
        board = boardKey(Fen.writeOpening(before.state.position).value),
        uci = last.toUci.uci,
        hint = hintPart.trim
      )).toRight(s"Ходы не читаются: $line")

  def parse(text: String): Parsed =
    val results = text.linesIterator.map(_.trim).filter(_.nonEmpty).map(parseLine).toList
    Parsed(results.collect { case Right(t) => t }, results.collect { case Left(e) => e })

  def json(traps: List[Trap]): JsArray =
    JsArray(traps.map(t => Json.obj("board" -> t.board, "uci" -> t.uci, "hint" -> t.hint)))
