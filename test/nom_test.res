open Test
open TestUtils

// create a seq from string
let createCharSeq = str => {
  let rec gen = index => {
    switch Js.String2.charAt(str, index) {
    | "" => Seq.Nil
    | c => Seq.make(c, ~thunk=() => gen(index + 1))
    }
  }
  gen(0)
}

test("nom satisfy", () => {
  let pa = Nom.satisfy(v => v == "a", _ => "not a")
  let pb = Nom.satisfy(v => v == "b", _ => "not b")

  let seq0 = createCharSeq("abc")
  let seq1 = list{"b"}->listToSeq

  resultOkEqual(~message="test pa", pa(seq0, 0), "a", createCharSeq("bc"), 1)
  resultOkEqual(~message="test pa", pb(seq1, 0), "b", createCharSeq(""), 1)
})

test("nom return", () => {
  let pa = Nom.satisfy(v => v == "a", _ => "not a")
  let pe = Nom.bind(pa, _ => Nom.return(97))

  let seq0 = createCharSeq("abc")
  let seq1 = createCharSeq("b")

  resultOkEqual(~message="test pe", pe(seq0, 0), 97, createCharSeq("bc"), 1)
  resultNoEqual(~message="test pe", pe(seq1, 0), "not a", createCharSeq("b"), 0)
})

type rec operator =
  | Add
  | Sub
  | Num(int)
  | ParentheseLeft
  | ParentheseRight
  | Node(list<operator>)

let pNumber = Nom.satisfy(
  v => {
    switch v {
    | Num(_) => true
    | _ => false
    }
  },
  _ => "not number",
)

let pLeft = Nom.satisfy(
  v => {
    switch v {
    | ParentheseLeft => true
    | _ => false
    }
  },
  _ => "not left",
)

let pRight = Nom.satisfy(
  v => {
    switch v {
    | ParentheseRight => true
    | _ => false
    }
  },
  _ => "not right",
)

let pAdd = Nom.satisfy(
  v => {
    switch v {
    | Add => true
    | _ => false
    }
  },
  _ => "not add",
)

test("nom calculator", () => {
  let p = Nom.tuple3(pNumber, pAdd, pNumber)
  resultOkEqual(
    ~message="test 1 + 2",
    p(list{Num(1), Add, Num(2)}->listToSeq, 0),
    (Num(1), Add, Num(2)),
    Seq.Nil,
    3,
  )

  let pNode = Nom.Sequence.delimited(
    pLeft,
    Nom.mapValue(p, ((v0, v1, v3)) => Node(list{v0, v1, v3})),
    pRight,
    ((_, e, i)) => "not node: " ++ e ++ ", index: " ++ Belt.Int.toString(i),
  )

  resultOkEqual(
    ~message="test (1 + 2)",
    pNode(list{ParentheseLeft, Num(1), Add, Num(2), ParentheseRight}->listToSeq, 0),
    Node(list{Num(1), Add, Num(2)}),
    Seq.Nil,
    5,
  )
})
