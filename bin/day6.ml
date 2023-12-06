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

let run () =
  match Advent.read_lines file with
  | [ time_line; distance_line ] ->
      let _, t = Advent.String.split_in_two ':' time_line in
      let times = parse_to_list_of_ints t in
      let _, d = Advent.String.split_in_two ':' distance_line in
      parse_to_list_of_ints d |> List.combine times
      |> find_record_beating_combinations |> List.fold_left ( * ) 1
      |> string_of_int |> print_endline
  | _ -> raise (Invalid_argument "Bad input")
