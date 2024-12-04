(*
Day 2 Red-Nosed Reports

Part 1:

Input looks like:
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9

Each line is a report, and I need to check whether a report is safe or unsafe.
Safe is when:
- all numbers in the line are strictly increasing or decreasing
- any two adjacent numbers differ by at least 1 and at most 3

We only need to count how many safe reports there are. Simple enough! We can
stream through the entire file!

Algo I'm thinking of is

1. Read the file line-by-line.
2. Parse each line into a list of numbers.
3. Remember a count of safe reports.
4. Run through the list left-to-right. Keep state: increasing or decreasing.
  4a. First iteration, check the hop (is it up or down?). Set inc/dec state.
  4b. Each iteration, check if the hop is small enough, and if it follows
      inc/dec. Short circuit to unsafe if not.
  4c. If safe, increment the safe count.

Part 2:

Now it's possible to tolerate a single bad level. I actually think this can be
easy to insert into 4 above:
- 4. Keep additional state: whether we've tolerated a bad level or not.
- 4b. If we hit an unsafe case, check to see if we've already tolerated a bad
  level.
  - If so, short circuit to unsafe.
  - If not, SKIP the next element, and check against the next next element!

This means we can only really skip once, and if we run our checks to completion
and we STILL hit a roadbump, we know we're unsafe.

Erratum! I should've read the fine print. The requirement isn't to tolerate a
single bad level. The requirement is instead more accurately worded as

"Now, the same rules apply as before, except if removing a single level from an
unsafe report would make it safe, the report instead counts as safe."

I'd argue those two wordings are semantically different *grumble grumble*. A
good case would be [4; 5; 3; 2]. 4 isn't bad by ANY means, but it's the one
that gets removed to make it safe. C'est la vie.

Because of this difference in requirement, I actually think I should just
try to remove one element at a time, then recheck. I can think of adding
backtracking (if you SKIP b and you still fail the check, you backtrack and try
to skip a then recheck, and so on until you hit the first element), but I think
- worst case, that's the same time complexity, O(n^2)
- I think the problem creator had just removing one element at a time in mind!!!
  See this example:

"1 3 2 4 5: Safe by removing the second level, 3."

A skipping algorithm would've made do with 1 3 4 5, actually, removing 2! And
that's less passes -- no backtracking, no trying with 1 removed.

Annoying, but if I can divine what the problem creator ACTUALLY had in mind,
I'd just do that :/
*)

(* string -> string list *)
let tokenize = String.split_on_char ' '

(* NOTE: I got tripped up here. You can do g @@ f @@ x but not g
   @@ f and leave the last arg open to partial application. I feel
   like I'm mentioning `line` here almost just as a formality *)
(* string -> int list *)
let parse_line line =
  line
  |> tokenize
  |> List.map int_of_string

type order =
  | Neutral
  | Increasing
  | Decreasing
(* 4a. get order *)
let get_order a b =
  if a < b then Increasing
  else if a > b then Decreasing
  else Neutral

(* checkers: int -> int -> bool *)
let check_order order a b =
  match order with
  | Neutral -> a == b
  | Increasing -> a < b
  | Decreasing -> a > b
let check_distance min max a b =
  let distance = Int.abs(a - b) in
  distance >= min && distance <= max

(* Let's use a state monad so we can compose checkers *)
let check order a b =
  Day02.CheckedPair.unit a b
    |> Day02.CheckedPair.map (check_order order)
    |> Day02.CheckedPair.map (check_distance 1 3)
    |> Day02.CheckedPair.unwrap

let rec is_report_safe_acc order list =
  match list with
  (* 0 elements: vacuous truth *)
  | [] -> true
  | (a :: rest) ->
      match rest with
      (* 1 element: vacuous truth *)
      | [] -> true
      (* 2 or more elements: check pairs *)
      | (b :: _) ->
        match check order a b with
        (* short circuit clause! *)
        | false -> false
        | true -> is_report_safe_acc order rest

(* int list -> bool *)
let is_report_safe list =
  match list with
  | [] -> true
  | (_ :: []) -> true
  | (a :: b :: _) ->
    let order = get_order a b in
    is_report_safe_acc order list

let rec is_report_tolerable_acc i list =
  if i >= List.length list then false
  else
    let with_removed = List.filteri (fun j _ -> i != j) list in
    match is_report_safe with_removed with
    (* if we find a safe report, short circuit there! *)
    | true -> true
    (* if not, keep trying *)
    | false -> is_report_tolerable_acc (i + 1) list

(* Also: int list -> bool *)
let is_report_tolerable list = is_report_tolerable_acc 0 list

(* string -> bool *)
let is_safe line =
  line
  |> parse_line
  |> is_report_tolerable

(* NOTE: Yet again thinking: should I raise out of main, or handle
   and set exit code? How do I do the latter? *)
let () =
  let in_channel = open_in "./input" in
  let rec process_line ic safe_count =
    match input_line ic with
    | line -> process_line ic (if is_safe line then safe_count + 1 else safe_count)
    | exception End_of_file -> safe_count
  in
  try
    Printf.printf "%d\n" (process_line in_channel 0);
    close_in in_channel
  with e ->
    close_in_noerr in_channel;
    raise e
