let file = "inputs/day3.in"

type position = {x : int; y : int}
type symbol = {character : char; position : position}
type number = {value : string; positions : position list}
type parsing_results = {symbols : symbol list; numbers : number list; cur_x : int; cur_y : int; current_number : number option}

let is_number c =
 c == '0' || c == '1' || c == '2' || c == '3' || c == '4' || c == '5' || c == '6' ||
   c == '7' || c == '8' || c == '9'

let is_symbol c =
   not (is_number c) && c != '.'

let empty_symbol_list : symbol list = []
let new_symbol char x y : symbol = {character = char; position = {x = x ; y = y}}
let get_symbols lines =
  let get_line_symbols line_no line =
    let (_, syms) = String.fold_left (fun (y, (symbols : symbol list)) c -> if is_symbol c then (y + 1, (new_symbol c line_no y) :: symbols) else (y + 1, symbols)) (0, empty_symbol_list) line in
    syms in

  let (_, symbols) = List.fold_left (fun (line_no, symbols) line -> (line_no + 1, (get_line_symbols line_no line) @ symbols)) (0, []) lines in
  symbols

let empty_number = {value = ""; positions = []}

let parse_line results line =
  let process_number_char results (char : char) : parsing_results =
    let current_position = {x = results.cur_x; y = results.cur_y} in
    let number = match results.current_number with
    | Some(number) -> number
    | None -> empty_number in
    let updated_number = {value = (String.cat number.value (String.make 1 char)); positions = current_position :: number.positions} in
    {results with current_number = Some(updated_number)} in

  let process_non_number_char results char =
    let results = match results.current_number with
    | Some(num) -> {results with numbers = num :: results.numbers; current_number = None}
    | _ -> results in

    if is_symbol char then
      let sym = new_symbol char results.cur_x results.cur_y in
      {results with symbols = sym :: results.symbols}
    else results in

  let process_char results char =
    let results = if (is_number char) then process_number_char results char else process_non_number_char results char in
    {results with cur_y = results.cur_y + 1} in

  let res = String.fold_left process_char results line in
  {res with cur_x = res.cur_x + 1; cur_y = 0}

let parse_input lines =
  let results = { symbols = []; numbers = []; cur_x = 0; cur_y = 0; current_number = None} in
  List.fold_left parse_line results lines

let are_positions_adjacent pos1 pos2 =
 (Int.abs (pos1.x - pos2.x)) <= 1 && (Int.abs (pos1.y - pos2.y)) <= 1

let numbers_adjacent_to_symbols results =
  let is_adjacent_to_symbol pos (symbols : symbol list) =
    List.exists (fun symbol -> are_positions_adjacent pos symbol.position) symbols in
  let check_number num =
    List.exists (fun pos -> is_adjacent_to_symbol pos results.symbols) num.positions in
  List.filter check_number results.numbers

let find_gears_sum results =
  List.filter (fun sym -> sym.character == '*') results.symbols
  |> List.map(fun sym ->
      let adjacent_numbers = List.filter (fun n -> List.exists (fun pos -> are_positions_adjacent pos sym.position) n.positions) results.numbers in
      if List.length adjacent_numbers == 2 then List.fold_left ( * ) 1 (List.map (fun x -> int_of_string x.value) adjacent_numbers) else 0
    )
  |> List.fold_left (+) 0

let run () =
  let lines = Advent.read_lines file in
  let results = parse_input lines in

  let sum =
    numbers_adjacent_to_symbols results
    |> List.map (fun x -> int_of_string x.value)
    |> List.fold_left (+) 0 in

  print_int sum;
  print_endline "";

  print_int (find_gears_sum results)
