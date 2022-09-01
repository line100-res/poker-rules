// type of a candle
type t = {
  o: float, // open
  c: float, // close
  h: float, // high
  l: float, // low
  v: float, // volume
  t: int, // timestamp
}

// (valume, timestamp)
type point = (float, int)

type err =
  | ReachTailErr
  | MergeCandleInsufficientFailed(t)
  | ScanCandleInsufficientFailed(point)

exception Err(string)

let mergeCandle = (a: t, c: t) => {
  {
    o: a.o,
    c: c.c,
    h: Js.Math.max_float(a.h, c.h),
    l: Js.Math.min_float(a.l, c.l),
    v: a.v +. c.v,
    t: c.t,
  }
}

let toOpenSeq = (s: Seq.t<t>) => Seq.map(s, k => (k.o, k.t))
let toCloseSeq = (s: Seq.t<t>) => Seq.map(s, k => (k.c, k.t))
let toHighSeq = (s: Seq.t<t>) => Seq.map(s, k => (k.h, k.t))
let toLowSeq = (s: Seq.t<t>) => Seq.map(s, k => (k.l, k.t))

let getOpen = Nom.mapValue(Nom.identity(ReachTailErr), k => (k.o, k.t))
let getClose = Nom.mapValue(Nom.identity(ReachTailErr), k => (k.c, k.t))
let getHigh = Nom.mapValue(Nom.identity(ReachTailErr), k => (k.h, k.t))
let getLow = Nom.mapValue(Nom.identity(ReachTailErr), k => (k.l, k.t))

// reduce candles to 1 candle
let reducListe = (n, f) => {
  (input: Seq.t<t>, i) => {
    switch input {
    | Seq.Nil => Nom.Fail(input, ReachTailErr, i)
    | seq =>
      switch Seq.takeListCount(seq, n) {
      | (0, _) => raise(Err("unreachable"))
      | (count, lst) => {
          if (count < n) {
            Nom.Fail(input, MergeCandleInsufficientFailed(f(lst)), i)
          } else {
            Nom.Pass(Seq.drop(input, count), f(lst), i + count)
          }
        }
      }
    }
  }
}

// scan candles to 1 point
let scanList = (n, f) => {
  (input: Seq.t<t>, i) => {
    switch input {
    | Seq.Nil => Nom.Fail(input, ReachTailErr, i)
    | seq =>
      switch Seq.takeListCount(seq, n) {
      | (0, _) => raise(Err("unreachable"))
      | (count, lst) => {
          if (count < n) {
            Nom.Fail(input, ScanCandleInsufficientFailed(f(lst)), i)
          } else {
            Nom.Pass(Seq.drop(input, 1), f(lst), i + 1)
          }
        }
      }
    }
  }
}

let merge = (n) => {
  reducListe(n, lst => {
    switch lst {
    | list{first, ...tail} => Belt.List.reduce(tail, first, (a, v) => mergeCandle(a, v))
    | _ => raise(Err("unreachable"))
    }
  })
}

let sma = (n, getValue) => {
  if (n < 2) {
    raise(Err("ma(n, v) must be bigger than 1"))
  }
  scanList(n, (lst) => {
    let rec sum = (l, acc, count) => {
      switch l {
      | list{} => (acc, count)
      | list{first, ...rest} => {
          let (v0, _) = acc // v相加，timestamp向后移动
          sum(rest, (v0 +. getValue(first), first.t), count + 1)
        }
      }
    }
    let ((total, count), timestamp) = sum(lst, (0.0, 0), 0)
    (total /. float(count), timestamp)
  })
}

 