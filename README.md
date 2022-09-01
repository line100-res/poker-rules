# Poker Rules

使用ParserC来描述规则，可以获得巨大的灵活性。

## 斗地主的主要逻辑
```rescript
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
```