open Test

let identity = (a, b) => a == b

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="intEqual", identity, a, b)

let stringEqual = (~message=?, a: string, b: string) =>
  assertion(~message?, ~operator="stringEqual", identity, a, b)

let rec listToSeq = lst => {
  switch lst {
  | list{} => Seq.Nil
  | list{a} => Seq.make(a)
  | list{a, ...rest} => Seq.make(a, ~thunk=() => listToSeq(rest))
  }
}

let seqEqual = (~message=?, seq: Seq.t<'a>, lst: list<'a>) => {
  let len1 = List.length(lst)
  let mathcer = lst->listToSeq
  let ret = Seq.match(seq, mathcer)
  let (len0, isForced) = Seq.countForced(seq)
  assertion(~message?, ~operator="seqEqual", identity, isForced, true)
  assertion(~message?, ~operator="seqEqual", identity, len0, len1)
  assertion(~message?, ~operator="seqEqual", identity, ret, len1)
}

let resultOkEqual = (~message=?, result: Nom.parseResult<'a, 'b, 'c>, value, rest, index) => {
  switch result {
  | Nom.Pass(r, v, i) => {
      assertion(~message?, ~operator="resultEqual:Pass:Value", identity, v, value)
      assertion(~message?, ~operator="resultEqual:Pass:Rest", Seq.equal, r, rest)
      assertion(~message?, ~operator="resultEqual:Pass:Value", identity, i, index)
    }
  | Nom.Fail(rest, e, i) => {
      let _ = Js.log3(rest, e, i)
      fail(~message?, ())
    }
  }
}

let resultNoEqual = (~message=?, result: Nom.parseResult<'a, 'b, 'c>, err, rest, index) => {
  switch result {
  | Nom.Fail(r, e, i) => {
      assertion(~message?, ~operator="resultEqual:Fail:Value", identity, e, err)
      assertion(~message?, ~operator="resultEqual:Fail:Rest", Seq.equal, r, rest)
      assertion(~message?, ~operator="resultEqual:Fail:Value", identity, i, index)
    }
  | _ => fail(~message?, ())
  }
}

let arrayEqual = (~message=?, a, b, comp) => {
  assertion(~message?, ~operator="arrayEqual", (a, b) => Belt.Array.eq(a, b, comp), a, b)
}
