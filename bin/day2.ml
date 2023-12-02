let file = "inputs/day2.in"

let split_in_two char str =
  match String.split_on_char char str with
  | [elem1; elem2] -> (elem1, elem2)
  | _ -> raise (Invalid_argument (String.cat "Splits into more than two: " str))

type result = {green : int; red : int; blue : int}
type game = {no : int; results : result list}

let is_result_legal (res : result) =
  res.red <= 12 && res.blue <= 14 && res.green <= 13

let get_color_num list_of_tuples color : int =
  match List.find_opt (fun (_, col) -> String.equal col color) list_of_tuples with
  | Some(n, _) -> n
  | _ -> 0

let parse_result res_string : result =
  let tuples =
    res_string
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.map (split_in_two ' ')
    |> List.map (fun (n, col) -> (int_of_string n, col)) in

  {green = get_color_num tuples "green";
    red = get_color_num tuples "red";
    blue = get_color_num tuples "blue"}

let parse_results line : result list =
  line
    |> String.split_on_char ';'
    |> List.map String.trim
    |> List.map parse_result

let parse_game line =
  let game_part, rest = split_in_two ':' line in
  let _, num = split_in_two ' ' game_part in
  let game_no = int_of_string num in
  let results = parse_results rest in
  { no = game_no ; results = results }

let max_number_list lst = List.fold_left max min_int lst

let game_power game =
  let min_green = game.results |> List.map (fun x -> x.green) |> max_number_list in
  let min_red = game.results |> List.map (fun x -> x.red) |> max_number_list in
  let min_blue = game.results |> List.map (fun x -> x.blue) |> max_number_list in
  min_green * min_red * min_blue

let run () =
  let games =
    file
    |> Advent.read_lines
    |> List.map parse_game in

  let sum =
    games
    |> List.filter (fun g -> List.for_all is_result_legal g.results)
    |> List.map (fun g : int -> g.no)
    |> List.fold_left (+) 0 in

  let powers_sum =
    games
    |> List.map game_power
    |> List.fold_left (+) 0 in

  print_endline (string_of_int sum);
  print_endline (string_of_int powers_sum)
