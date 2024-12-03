import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some, None}
import gleam/result
import gleam/string
import simplifile

//// Day 1 Historian Hysteria
////
//// Part 1:
////
//// Input is expected in the following format:
//// 3   4
//// 4   3
//// 2   5
//// 1   3
//// 3   9
//// 3   3
////
//// Essentially we just want to:
//// - treat these as two parallel lists
//// - sort the lists
//// - zip each sorted list and take the difference between corresponding
////   elements
////
//// The sorting is unfortunate -- I don't think I can stream the files because
//// of it. Let's just slurp the input file with simplifile instead.
////
//// Part 2:
////
//// The second list is now used for existence checking to calculate a
//// similarity score! So, for each element in the left list, I have to check
//// how many times it appears in the right list.
////
//// I'm so glad I moved away from parsing into first-class tuples, because
//// that 1-to-1 relationship proved to be broken.
////
//// So, amendments to the algorithm:
//// - I don't want to sort the lists anymore (I don't need to compute
////   similarity scores in a certain order)
//// - I want to build a count map of the right list ahead of time (mapping
////   each element to the number of times it appears)
//// - I map the left list to a list of counts
//// - Sum that
////
//// Building the count map ahead of time means I can finish in O(n) instead of
//// traversing the entire right list each and every time, which is O(n^2).

/// Lines are newline-terminated, not -separated, but newline-separated is so
/// much easier to parse with string.split.
///
/// Remove final newline.
fn split_lines(input: String) -> List(String) {
  let nl_separated = case string.ends_with(input, "\n") {
    True -> string.trim_end(input)
    False -> input
  }

  string.split(nl_separated, "\n")
}

fn parse_lists(lines: List(String)) -> List(List(String)) {
  lines
    |> list.map(string.split(_, "   "))
    |> list.transpose
}

fn parse_int_list(l: List(String)) -> Result(List(Int), Nil) {
  l
  |> list.map(int.parse)
  |> result.all
}

fn get_distance(t: #(Int, Int)) -> Int {
  let #(a, b) = t
  int.absolute_value(a - b)
}

fn count_elements(l: List(t)) -> Dict(t, Int) {
  // heh, pretty easily taken from docs
  let increment = fn(x) {
    case x {
      Some(i) -> i + 1
      None -> 1
    }
  }

  list.fold(l, dict.new(), fn(seen, el) {
    dict.upsert(seen, el, increment)
  })
}

fn get_similarity(count_dict: Dict(Int, Int), num: Int) -> Int {
  let count = count_dict
    |> dict.get(num)
    |> result.unwrap(0)

  num * count
}

@deprecated("Original main for Part 1.")
pub fn part1() {
  let assert Ok(input) = simplifile.read(from: "./data")

  let assert Ok(int_lists) = input
    // String -> List(String)
    |> split_lines
    // List(String) -> List(List(String))
    |> parse_lists
    // List(List(String)) -> List(List(Int))
    |> list.map(parse_int_list)
    |> result.all

  // sort the lists
  let assert [left, right] = int_lists
    |> list.map(list.sort(_, by: int.compare))

  // List(List(Int)) -> List(Int)
  let sum = list.zip(left, right)
    |> list.map(get_distance)
    |> int.sum

  // print to stdout
  io.println(int.to_string(sum))
}

// NOTE: I wonder what's idiomatic for main
// I'm opting for functional core, imperative shell
// Assert all the errors, so I fail fast
pub fn main() {
  let assert Ok(input) = simplifile.read(from: "./data")

  let assert Ok([left, right]) = input
    // String -> List(String)
    |> split_lines
    // List(String) -> List(List(String))
    |> parse_lists
    // List(List(String)) -> List(List(Int))
    |> list.map(parse_int_list)
    |> result.all

  let count_dict = count_elements(right)

  let sum = left
    |> list.map(get_similarity(count_dict, _))
    |> int.sum

  // print to stdout
  io.println(int.to_string(sum))
}
