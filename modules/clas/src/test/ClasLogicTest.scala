package lila.clas

import com.roundeights.hasher.Algo

// Вектор посчитан на стороне сайта (PHP: base64url без «=», hash_hmac sha256) —
// тест держит общий формат ссылки kl_play_login_token() <-> ClasLogic.
class ClasLogicTest extends munit.FunSuite:

  val body =
    "eyJzaWQiOjQyLCJuYW1lIjoi0JjQstCw0L3QvtCyINCf0ZHRgtGAIiwiZ3JvdXBzIjpbeyJpZCI6NywibmFtZSI6ItCc0LDRgNGCIDEgMjAyNiDQktGCXC_Qp9GCIiwidGVhY2hlciI6ImNvYWNoQGV4YW1wbGUuY29tIn1dLCJleHAiOjE5MDAwMDAwMDAsIm4iOiJhYmNkZWZnaGlqa2xtbm9wIn0"
  val sig = "1c436a0290de5ae7d27a9fcbbf1fd42d403d54a5d8cb19de9624b02a3212571e"

  test("hmac matches the site"):
    assert(Algo.hmac("test-secret").sha256(body).hash_=(sig))
    assert(!Algo.hmac("other-secret").sha256(body).hash_=(sig))

  test("parse payload from the site"):
    val p = ClasLogic.parse(body).get
    assertEquals(p.sid, 42)
    assertEquals(p.name, "Иванов Пётр")
    assertEquals(p.groups, List(ClasLogic.Group(7, "Март 1 2026 Вт/Чт", Some("coach@example.com"))))
    assertEquals(p.exp, 1900000000L)

  test("reject garbage"):
    assertEquals(ClasLogic.parse("not-base64!"), None)
    assertEquals(ClasLogic.parse(java.util.Base64.getUrlEncoder.encodeToString("{}".getBytes)), None)

  test("site data on top, teacher notes kept"):
    import ClasLogic.mergeNotes
    assertEquals(mergeNotes(Some("ФИО: А"), ""), "ФИО: А\n———")
    assertEquals(mergeNotes(Some("ФИО: Б"), "ФИО: А\n———\nсильный эндшпиль"), "ФИО: Б\n———\nсильный эндшпиль")
    assertEquals(mergeNotes(Some("ФИО: Б"), "старая заметка"), "ФИО: Б\n———\nстарая заметка")
    assertEquals(mergeNotes(None, "старая заметка"), "старая заметка")

  test("old payload without card fields still parses"):
    assertEquals(
      ClasLogic.parse(body).map(p => (p.login, p.info, p.rcf, p.tier)),
      Some((None, None, None, None))
    )

  test("rank tiers from the site"):
    import ClasLogic.Tier
    assertEquals(Tier.byKey("u2"), Some(Tier.Junior))
    assertEquals(Tier.byKey("1j"), Some(Tier.Senior))
    assertEquals(Tier.byKey("other"), None)
    assertEquals(Tier.Junior.clasId.value, "logic-u2")
    assertEquals(ClasLogic.clubTeam._1.value, "logic-all")
