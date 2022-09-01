module Array = {
  // array group
  let group = (arr: array<'a>, make: 'a => 'b, merge: ('b, 'a) => option<'b>) => {
    Js.Array2.reduce(
      arr,
      (acc, v) => {
        switch Js.Array.pop(acc) {
        | None => {
            let _ = Js.Array2.push(acc, make(v))
          }
        | Some(last) =>
          switch merge(last, v) {
          | None => {
              let _ = Js.Array2.push(acc, last)
              let _ = Js.Array2.push(acc, make(v))
            }
          | Some(merged) => {
              let _ = Js.Array2.push(acc, merged)
            }
          }
        }
        acc
      },
      [],
    )
  }
}
