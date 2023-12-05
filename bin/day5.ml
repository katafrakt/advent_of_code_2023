let file = "inputs/day5.in"

type transformation = { drs : int; srs : int; length : int }

let read_seeds (lines : string list) =
  match lines with
  | line :: _ ->
      let _, nums = Advent.String.split_in_two ':' line in
      nums |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
  | _ -> raise (Invalid_argument "bad lines")

let write_seeds seeds =
  List.iter
    (fun x ->
      print_int x;
      print_char ' ')
    seeds

let skip_line lines =
  match lines with
  | _hd :: tail -> tail
  | _ -> raise (Invalid_argument "Cannot skip line, empty list")

let rec read_transformations transformations current_opt lines =
  let current =
    match current_opt with Some transformation -> transformation | None -> []
  in
  match lines with
  | line :: rest -> (
      match String.split_on_char ' ' line with
      | [ drs; srs; length ] ->
          let transformation =
            {
              drs = int_of_string drs;
              srs = int_of_string srs;
              length = int_of_string length;
            }
          in
          read_transformations transformations
            (Some (transformation :: current))
            rest
      | [ _; _ ] -> read_transformations (current :: transformations) None rest
      | _ -> raise (Invalid_argument "wut"))
  | [] -> List.rev (current :: transformations)

let transform_one acc transformation =
  List.map
    (fun seed ->
      match
        List.find_opt
          (fun t -> t.srs <= seed && seed - t.srs < t.length)
          transformation
      with
      | Some trans -> trans.drs + (seed - trans.srs)
      | None -> seed)
    acc

let process_seeds seeds transformations =
  List.fold_left transform_one seeds transformations

let run () =
  let lines = Advent.read_lines file in
  let seeds = read_seeds lines in
  let lines = skip_line lines in
  let transformations = read_transformations [] None lines in
  let processed = process_seeds seeds transformations in
  print_int (List.fold_left min max_int processed)
