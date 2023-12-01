let file = "inputs/day1.in"

let string_to_char_list s =
  s |> String.to_seq |> List.of_seq

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let value_of c =
  match c with
  | '0' -> Some(0)
  | '1' -> Some(1)
  | '2' -> Some(2)
  | '3' -> Some(3)
  | '4' -> Some(4)
  | '5' -> Some(5)
  | '6' -> Some(6)
  | '7' -> Some(7)
  | '8' -> Some(8)
  | '9' -> Some(9)
  | _   -> None

let find_nums line =
  let process_char ((first : int option), (last : int option)) el =
    match (value_of el, first) with
    | (Some(x), None) -> (Some(x), Some(x))
    | (Some(x), Some(_)) -> (first, Some(x))
    | (None, _) -> (first, last) in

  let (first, last) = List.fold_left process_char (None, None) line in
  match (first, last) with
  | (Some(x), Some(y)) -> x * 10 + y
  | _ -> 0

let part1 =
  let input = read_lines file in
  let chars = List.map string_to_char_list input in
  let numbers = List.map find_nums chars in
  let result = List.fold_left (+) 0 numbers in
  print_endline (string_of_int result)

let maybe_replace_num pos word num str =
  if Str.string_match word str pos then Str.replace_first word num str else str

let rec replace_words str pos =
  if pos < String.length(str) then
    let new_str =
      str
      |> maybe_replace_num pos (Str.regexp {|one|}) "1ne"
      |> maybe_replace_num pos (Str.regexp {|two|}) "2wo"
      |> maybe_replace_num pos (Str.regexp {|three|}) "3hree"
      |> maybe_replace_num pos (Str.regexp {|four|}) "4our"
      |> maybe_replace_num pos (Str.regexp {|five|}) "5ive"
      |> maybe_replace_num pos (Str.regexp {|six|}) "6ix"
      |> maybe_replace_num pos (Str.regexp {|seven|}) "7even"
      |> maybe_replace_num pos (Str.regexp {|eight|}) "8ight"
      |> maybe_replace_num pos (Str.regexp {|nine|}) "9ine" in
    if new_str != str then replace_words new_str 0 else replace_words str (pos + 1)
  else
    str

let replace_words str = replace_words str 0

let part2 =
  let input = read_lines file in
  let replaced = List.map replace_words input in
  let chars = List.map string_to_char_list replaced in
  let numbers = List.map find_nums chars in
  let result = List.fold_left (+) 0 numbers in
  print_endline (string_of_int result)

let run =
  part1;
  part2;