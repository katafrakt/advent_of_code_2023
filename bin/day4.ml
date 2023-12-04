let file = "inputs/day4.in"

let line_wins line =
  let (_, numbers) = Advent.String.split_in_two ':' line in
  let (left, right) = Advent.String.split_in_two '|' numbers in
  let left = String.trim left in
  let right = String.trim right in
  let winning = String.split_on_char ' ' left |> List.filter (fun x -> not (String.equal x "")) in
  let selected = String.split_on_char ' ' right |> List.filter (fun x -> not (String.equal x "")) in
  List.filter (fun n -> List.exists (fun w -> String.equal n w) winning) selected

let num_line_wins line = List.length (line_wins line)

let line_value line =
  let won = line_wins line in
  match List.length won with
  | 0 -> 0
  | 1 -> 1
  | _ -> (List.fold_left (fun acc _ -> acc * 2) 1 won) / 2

let part1 lines =
  lines
  |> List.map line_value
  |> List.fold_left (+) 0

let part2 lines =
  let n = List.length lines in
  let amounts = Array.make n 1 in
  List.iteri (fun i line ->
    let num_of_current = amounts.(i) in
    if i < n - 1 then
    for j = i + 1 to i + (num_line_wins line) do
      amounts.(j) <- amounts.(j) + num_of_current
    done
    ) lines;
  Array.fold_left (+) 0 amounts

let run () =
  let lines = Advent.read_lines file in
  let sum = part1 lines in
  print_int sum;
  let num_of_cards = part2 lines in
  print_endline "";
  print_int num_of_cards
