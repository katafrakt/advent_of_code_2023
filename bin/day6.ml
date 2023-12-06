let file = "inputs/day6.in"

let parse_to_list_of_ints frag =
  frag |> String.trim |> String.split_on_char ' '
  |> List.filter (fun x -> not (String.equal x String.empty))
  |> List.map int_of_string

let find_record_beating_combinations pairs =
  List.map
    (fun (d, record) ->
      let x = ref 0 in
      for i = 1 to d do
        if i * (d - i) > record then x := x.contents + 1
      done;
      x.contents)
    pairs

let part1 times distances =
  distances |> List.combine times |> find_record_beating_combinations
  |> List.fold_left ( * ) 1 |> string_of_int |> print_endline

let rec find_min i time distance =
  if i * (time - i) > distance then i else find_min (i + 1) time distance

let rec find_max i time distance =
  if i * (time - i) > distance then i else find_max (i - 1) time distance

let part2 times distances =
  let time =
    times |> List.map string_of_int |> String.concat "" |> int_of_string
  in
  let distance =
    distances |> List.map string_of_int |> String.concat "" |> int_of_string
  in
  let min = find_min 0 time distance in
  let max = find_max time time distance in
  print_endline (string_of_int (max - min + 1))

let run () =
  match Advent.read_lines file with
  | [ time_line; distance_line ] ->
      let _, t = Advent.String.split_in_two ':' time_line in
      let _, d = Advent.String.split_in_two ':' distance_line in
      let times = parse_to_list_of_ints t in
      let distances = parse_to_list_of_ints d in
      part1 times distances;
      part2 times distances
  | _ -> raise (Invalid_argument "Bad input")
