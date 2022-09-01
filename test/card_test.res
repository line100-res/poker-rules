open Test
open TestUtils

let cardResultEqual = (~message=?, a, b) => {
  assertion(~message?, ~operator="cardResultEqual", (a, b) => a == b, a, b)
}

test("card isAnyOf", () => {
  let grouped = Card.cardGroup([(1, 0), (2, 0), (1, 1), (2, 1), (1, 3)])
  arrayEqual(~message="grouped array", grouped, [(1, 3), (2, 2)], ((v0, c0), (v1, c1)) =>
    v0 == v1 && c0 == c1
  )
})

test("one", () => {
  let ret = Card.match([(1, 0)])
  cardResultEqual(~message="match one", ret, Ok(Card.One(1)))

  let ret = Card.match([(1, 0), (1, 1)])
  cardResultEqual(~message="match one", ret, Ok(Card.Pair(1)))
})

test("JokerBomb", () => {
  let ret = Card.match([(16, 4), (17, 4)])
  cardResultEqual(~message="JokerBomb", ret, Ok(Card.JokerBomb))

  let ret = Card.match([(16, 4), (17, 4), (1, 0)])
  cardResultEqual(~message="JokerBomb Failed", ret, Error(Card.PatternAllBranchFailed))
})

test("Bomb", () => {
  let ret = Card.match([(3, 0), (3, 1), (3, 2), (3, 3)])
  cardResultEqual(~message="Bomb(3)", ret, Ok(Card.Bomb(3)))
})