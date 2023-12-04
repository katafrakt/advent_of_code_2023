let file = "inputs/day4.in"

let line_value line =
  let (_, numbers) = Advent.String.split_in_two ':' line in
  let (left, right) = Advent.String.split_in_two '|' numbers in
  let left = String.trim left in
  let right = String.trim right in
  let winning = String.split_on_char ' ' left |> List.filter (fun x -> not (String.equal x "")) in
  let selected = String.split_on_char ' ' right |> List.filter (fun x -> not (String.equal x "")) in
  let won = List.filter (fun n -> List.exists (fun w -> String.equal n w) winning) selected in
  match List.length won with
  | 0 -> 0
  | 1 -> 1
  | _ -> (List.fold_left (fun acc _ -> acc * 2) 1 won) / 2

let part1 lines =
  lines
  |> List.map line_value
  |> List.fold_left (+) 0

let run () =
  let lines = Advent.read_lines file in
  let sum = part1 lines in
  print_int sum
