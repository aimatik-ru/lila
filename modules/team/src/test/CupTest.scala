package lila.team

import java.time.Instant

class CupTest extends munit.FunSuite:

  import Cup.*

  val t0 = Instant.parse("2026-11-29T15:00:00Z") // 18:00 МСК
  val dates = Map(0 -> t0, 1 -> t0.plusSeconds(7 * 86400))
  val none = (_: Int) => false

  test("phase: waiting, rosters, starting, playing"):
    assertEquals(phase(t0.minusSeconds(3600), dates, Run(), 20, none), Phase.Waiting(0, t0))
    assertEquals(phase(t0.minusSeconds(600), dates, Run(), 20, none), Phase.Rosters(0, t0))
    assertEquals(phase(t0, dates, Run(round = Some(0)), 20, none), Phase.Starting(0))
    assertEquals(phase(t0, dates, Run(round = Some(0), started = true), 20, none), Phase.Playing(0))

  test("phase: after a played round the next one waits"):
    val run = Run(played = Set(0))
    assertEquals(phase(t0.plusSeconds(7200), dates, run, 20, none), Phase.Waiting(1, dates(1)))
    assertEquals(phase(t0.plusSeconds(7200), dates, run, 20, _ == 0), Phase.Decide(0))
    assertEquals(phase(t0, dates, Run(played = Set(0, 1)), 20, none), Phase.Done)
    assertEquals(phase(t0, Map.empty, Run(), 20, none), Phase.Idle)

  test("postpone moves only its own round"):
    val run = Run(holdRound = Some(0), holdAt = Some(t0.plusSeconds(600)))
    assertEquals(run.startOf(0, t0), t0.plusSeconds(600))
    assertEquals(run.startOf(1, dates(1)), dates(1))
    assertEquals(phase(t0.minusSeconds(60), dates, run, 20, none), Phase.Rosters(0, t0.plusSeconds(600)))

  test("start is due 2 minutes before, battles not earlier than now + 90 s"):
    assert(!dueToStart(t0.minusSeconds(121), t0))
    assert(dueToStart(t0.minusSeconds(120), t0))
    assertEquals(battleStart(t0.minusSeconds(120), t0), t0)
    assertEquals(battleStart(t0, t0), t0.plusSeconds(90))

  test("draw is needed only for a changed set of teams and no results"):
    val slots = List(Some("a"), None, Some("b"), Some("c"))
    assert(!needsDraw(slots, Set("a", "b", "c"), hasResults = false))
    assert(needsDraw(slots, Set("a", "b", "c", "d"), hasResults = false))
    assert(needsDraw(Nil, Set("a", "b"), hasResults = false))
    assert(!needsDraw(slots, Set("a", "b", "c", "d"), hasResults = true))
    assert(!needsDraw(Nil, Set("a"), hasResults = false))

  test("late players never create teams"):
    val teams = List("k1" -> List(1500, 1400, 1300, 1200, 1100), "k2" -> List(1500, 1400), "k3" -> List(1000))
    val (placed, rest) = placeLate(teams, List("x" -> 1200, "y" -> 1300, "z" -> 900), 5)
    assertEquals(placed, List("y" -> "k3", "x" -> "k3", "z" -> "k2"))
    assertEquals(rest, Nil)
    val full = List("k1" -> List.fill(5)(1000))
    assertEquals(placeLate(full, List("x" -> 1200, "y" -> 900), 5), (Nil, List("x", "y")))

  test("duplicates"):
    val d = duplicates(List("k1" -> List("a", "b"), "k2" -> List("b", "c"), "k3" -> List("b")))
    assertEquals(d, Map("b" -> List("k1", "k2", "k3")))

  test("start plan: pairs, walkovers, friendly"):
    val p =
      startPlan(List(("0.0", "a", "b"), ("0.1", "c", "d"), ("0.2", "e", "f")), List("a", "b", "c", "g", "h"))
    assertEquals(p.pairs, List(("0.0", "a", "b")))
    assertEquals(p.walkovers, List("0.1" -> "c"))
    assertEquals(p.friendly, List("g", "h"))

  test("missed start is left to the admin"):
    assert(!missed(t0.plusSeconds(1800), t0))
    assert(missed(t0.plusSeconds(1801), t0))

  test("key round"):
    assertEquals(keyRound("2.1"), Some(2))
    assertEquals(keyRound("out0"), Some(0))
    assertEquals(keyRound("x"), None)

  test("move: all picked players must fit, those already in the team take no extra place"):
    assertEquals(moveError(Set("x"), List("a", "b"), 5), None)
    assertEquals(moveError(Set("x", "y", "z"), List("a", "b"), 5), None)
    assertEquals(moveError(Set("a", "x"), List("a", "b", "c", "d"), 5), None)
    assertEquals(moveError(Set("a", "b"), List("a", "b", "c", "d", "e"), 5), None)
    assert(moveError(Set("x", "y"), List("a", "b", "c", "d"), 5).isDefined)
    assert(moveError(Set("x"), List("a", "b", "c", "d", "e"), 5).isDefined)

  test("move: nobody picked is an error, a new team takes up to max"):
    assert(moveError(Set.empty[String], List("a"), 5).isDefined)
    assertEquals(moveError((1 to 5).toSet, Nil, 5), None)
    assert(moveError((1 to 6).toSet, Nil, 5).isDefined)
