open Nom

exception Err(string)

// (number, color)
// 数字 3, 4, 5, ... 11(J) 12(Q) 13(K) 14(A) 15(2) 16(JokerBlack) 17(JokerRed)
// 花色 ♥️(0)，♦️(1)，♠️(2)，♣️(3)
type t = (int, int)

// 检查大小时的类型
type rec result =
  | One(int)
  | Pair(int)
  | Three1(int)
  | Three2(int)
  | Four2(int)
  | Four4(int)
  | Progression(int, int) // (value, length) length >= 5
  | PairProgression(int, int) // (value, length) length >= 3
  | Plane(int, int) // (value, length) length >= 2
  | Bomb(int)
  | JokerBomb

type err =
  | ReachEndError
  | AnyGroupFailed
  | CardValueFailed
  | ProgressionTooShortFailed
  | PatternMatchFailed(string)
  | PatternNotReachEndFailed
  | PatternAllBranchFailed

let cardGroup = (arr: array<t>) => {
  let sortedValue = arr->Js.Array2.map(((v, _)) => v)->Js.Array2.sortInPlaceWith((a, b) => a - b)
  Util.Array.group(
    sortedValue,
    v => (v, 1),
    ((lastV, lastCount), v) =>
      if lastV == v {
        Some(lastV, lastCount + 1)
      } else {
        None
      },
  )
}

// return (value, n)
let isAnyOf = (n: int) => {
  satisfy(
    ((_: int, count)) => count == n,
    e =>
      switch e {
      | None => ReachEndError
      | Some(_) => AnyGroupFailed
      },
  )
}

let isValueEqual = (v: int) => {
  satisfy(
    ((value, _: int)) => value == v,
    e =>
      switch e {
      | None => ReachEndError
      | Some(_) => CardValueFailed
      },
  )
}

let isProgressionOf = (matcher: Nom.parser<t, t, err>, minLen) => {
  let p = Nom.Multi.reduceBy(
    matcher,
    ((maxValue, count), (v, _)) => {
      // acc is (maxValue, count)
      if maxValue + 1 == v {
        Some((v, count + 1))
      } else {
        None
      }
    },
    (0, 0),
  )
  Nom.mapPassed(p, (rest, v, i) => {
    let (_, count) = v
    if count >= minLen {
      Nom.Pass(rest, v, i)
    } else {
      Nom.Fail(rest, ProgressionTooShortFailed, i)
    }
  })
}

let noValue = p => mapValue(p, _ => (0, 0))
let combine = (p1, p2, name) =>
  Nom.allConsuming(Nom.Sequence.terminated(p1, p2, _ => PatternMatchFailed(name)), _ =>
    PatternNotReachEndFailed
  )
let single = p => Nom.allConsuming(p, _ => PatternNotReachEndFailed)

// 检测基本牌型
let isOne = isAnyOf(1)
let isPair = isAnyOf(2)
let isThree = isAnyOf(3)
let isFour = isAnyOf(4)
let is2Single = noValue(tuple(isOne, isOne))
let is2Pair = noValue(tuple(isPair, isPair))
let isJokerBlack = isValueEqual(16)
let isJokerRed = isValueEqual(17)
// 检测递增牌型
let isProgression1 = isProgressionOf(isOne, 5)
let isProgression2 = isProgressionOf(isPair, 3)
let isProgression3 = isProgressionOf(isThree, 2)
// 出牌组合牌型
let one = Nom.context(((v, _)) => One(v), single(isOne))
let pair = Nom.context(((v, _)) => Pair(v), single(isPair))
let three1 = Nom.context(((v, _)) => Three1(v), combine(isThree, isOne, "three1"))
let three2 = Nom.context(((v, _)) => Three2(v), combine(isThree, isPair, "tree2"))
let four2 = Nom.context(((v, _)) => Four2(v), combine(isFour, is2Single, "four2"))
let four4 = Nom.context(((v, _)) => Four4(v), combine(isFour, is2Pair, "four4"))
let progression1 = Nom.context(((v, c)) => Progression(v, c), single(isProgression1))
let progression2 = Nom.context(((v, c)) => PairProgression(v, c), single(isProgression2))
let plane = Nom.context(((v, c)) => Plane(v, c), single(isProgression3))
let bomb = Nom.context(((v, _)) => Bomb(v), single(isFour))
let jokerBomb = Nom.context(_ => JokerBomb, single(tuple(isJokerBlack, isJokerRed)))

let rule = Nom.alt(list{
  one,
  pair,
  three1,
  three2,
  four2,
  four4,
  progression1,
  progression2,
  plane,
  bomb,
  jokerBomb,
}, () => PatternAllBranchFailed)

let match = (cards: array<t>) => {
  let seq = cards->cardGroup->Seq.fromArray
  switch rule(seq, 0) {
  | Nom.Pass(_, v, _) => Ok(v)
  | Nom.Fail(_, e, _) => Error(e)
  }
}
