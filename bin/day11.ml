let file = "inputs/day11.in"

let read_galaxies lines =
  let read_galaxies_in_line y line =
    line |> String.to_seq |> List.of_seq
    |> List.mapi (fun i ch -> if ch == '#' then (i, y) else (-1, -1))
    |> List.filter (fun (x, y) -> x != -1 && y != -1)
  in

  lines |> List.mapi (fun i l -> read_galaxies_in_line i l) |> List.flatten

let read_dimensions lines =
  let height = List.length lines in
  let width = lines |> List.hd |> String.to_seq |> Seq.length in
  (width, height)

let list_pairs list =
  let rec pairs prs list =
    match list with
    | [] -> prs
    | h :: tl ->
        let new_pairs = List.map (fun x -> (h, x)) tl in
        pairs (prs @ new_pairs) tl
  in
  pairs [] list

let find_distances pairs =
  pairs
  |> List.map (fun ((x1, y1), (x2, y2)) ->
         Int.abs (x1 - x2) + Int.abs (y1 - y2))

let calc galaxies width height expansion_factor =
  let find_empty_lines galaxies height =
    let rec check_line galaxies line_no empty_list height =
      if line_no == height then empty_list
      else if List.exists (fun (_, y) -> y == line_no) galaxies then
        check_line galaxies (line_no + 1) empty_list height
      else check_line galaxies (line_no + 1) (empty_list @ [ line_no ]) height
    in
    check_line galaxies 0 [] height
  in

  let find_empty_columns galaxies width =
    let rec check_line galaxies col_no empty_list width =
      if col_no == width then empty_list
      else if List.exists (fun (x, _) -> x == col_no) galaxies then
        check_line galaxies (col_no + 1) empty_list height
      else check_line galaxies (col_no + 1) (empty_list @ [ col_no ]) height
    in
    check_line galaxies 0 [] width
  in

  let expand_lines nums galaxies =
    List.fold_right
      (fun i acc ->
        List.map (fun (x, y) -> if y > i then (x, y + expansion_factor) else (x, y)) acc)
      nums galaxies
  in

  let expand_columns nums galaxies =
    List.fold_right
      (fun i acc ->
        List.map (fun (x, y) -> if x > i then (x + expansion_factor, y) else (x, y)) acc)
      nums galaxies
  in

  let empty_lines = find_empty_lines galaxies height in
  let empty_columns = find_empty_columns galaxies width in

  galaxies |> expand_lines empty_lines
  |> expand_columns empty_columns
  |> list_pairs |> find_distances |> List.fold_left ( + ) 0 |> string_of_int
  |> print_endline

let run () =
  let lines = file |> Advent.read_lines in
  let width, height = read_dimensions lines in
  let galaxies = read_galaxies lines in
  calc galaxies width height 1;
  calc galaxies width height 999999;
  ()
