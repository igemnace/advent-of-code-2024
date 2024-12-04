type checked_pair = CheckedPair of (int * int) * bool
let unit a b = CheckedPair ((a, b), true)
let map f c =
  match c with
  | CheckedPair ((a, b), checked) -> CheckedPair ((a, b), (f a b) && checked)
let unwrap c =
  match c with
  | CheckedPair(_, checked) -> checked
