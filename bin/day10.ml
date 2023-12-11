let file = "inputs/day10.in"

let read_to_hash_table lines =
  let height = List.length lines in
  let width = lines |> List.hd |> String.to_seq |> List.of_seq |> List.length in
  let area = Hashtbl.create (height * width) in
  List.iteri
    (fun y line ->
      List.iteri
        (fun x char -> Hashtbl.add area (x, y) char)
        (line |> String.to_seq |> List.of_seq))
    lines;
  area

let find_start positions =
  match positions |> Hashtbl.to_seq |> Seq.find (fun (_, ch) -> ch == 'S') with
  | Some (pos, _) -> pos
  | _ -> raise (Invalid_argument "wut")

let find_start_adjs (x, y) positions =
  let check_adj positions pos chars =
    match Hashtbl.find_opt positions pos with
    | None -> None
    | Some ch -> if List.exists (fun x -> x == ch) chars then Some pos else None
  in
  [
    check_adj positions (x - 1, y) [ 'L'; 'F'; '-' ];
    check_adj positions (x + 1, y) [ '-'; '7'; 'J' ];
    check_adj positions (x, y - 1) [ '7'; 'F'; '|' ];
    check_adj positions (x, y + 1) [ 'L'; 'J'; '|' ];
  ]
  |> List.filter Option.is_some |> List.map Option.get

let print_pos (x, y) =
  print_string "(";
  print_int x;
  print_string ",";
  print_int y;
  print_string ")"

type position = int * int
type branch = { steps : int; from : position; current : position }

let print_branch (br : branch) =
  print_string "{ steps: ";
  print_int br.steps;
  print_string ", from: ";
  print_pos br.from;
  print_string ", current: ";
  print_pos br.current;
  print_endline " }"

let pos_equal (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

let rec walk_pipes branches positions =
  let get_next branch : position =
    let x, y = branch.current in
    (match Hashtbl.find positions (x, y) with
    | 'L' -> [ (x + 1, y); (x, y - 1) ]
    | 'F' -> [ (x + 1, y); (x, y + 1) ]
    | 'J' -> [ (x - 1, y); (x, y - 1) ]
    | '7' -> [ (x - 1, y); (x, y + 1) ]
    | '|' -> [ (x, y - 1); (x, y + 1) ]
    | '-' -> [ (x + 1, y); (x - 1, y) ]
    | _ ->  [ (x, y); (x, y) ])
    |> List.filter (fun x -> not (pos_equal x branch.from))
    |> List.hd
  in

  let new_branches =
    List.map (fun b -> let next = get_next b in { steps = b.steps + 1; from = b.current; current = next }) branches
  in
  match new_branches with
  | [ b1; b2 ] ->
      if pos_equal b1.current b2.current then b1.steps
      else if b1.current == b2.from && b2.current == b1.from then b1.steps - 1
      else walk_pipes new_branches positions
  | _ -> raise (Invalid_argument "")

let run () =
  let input = file |> Advent.read_lines |> read_to_hash_table in
  let start = find_start input in
  let adjs = find_start_adjs start input in
  let branches =
    adjs |> List.map (fun x -> { steps = 1; from = start; current = x })
  in
  walk_pipes branches input |> print_int
