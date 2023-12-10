let file = "inputs/day9.in"

let rec go_to_zeros lists =
  let last = List.hd lists in
  let new_line =
    Advent.List.to_pairs last |> List.map (fun (el1, el2) -> el2 - el1)
  in
  if List.for_all (fun x -> x == 0) new_line then lists
  else go_to_zeros (new_line :: lists)

let rec extrapolate value lists =
  let list = List.hd lists in
  let last_elem = list |> List.rev |> List.hd in
  let new_elem = last_elem + value in
  if List.length lists == 1 then new_elem
  else extrapolate new_elem (List.tl lists)

let rec extrapolate_back value lists =
  let list = List.hd lists in
  let first_elem = list |> List.hd in
  let new_elem = first_elem - value in
  if List.length lists == 1 then new_elem
  else extrapolate_back new_elem (List.tl lists)

let part1 nums =
  List.map (fun line -> go_to_zeros [ line ] |> extrapolate 0) nums
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline

let part2 nums =
  List.map (fun line -> go_to_zeros [ line ] |> extrapolate_back 0) nums
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline

let run () =
  let read_numbers line =
    line |> String.split_on_char ' ' |> List.map int_of_string
  in
  let input_nums = Advent.read_lines file |> List.map read_numbers in
  part1 input_nums;
  part2 input_nums
