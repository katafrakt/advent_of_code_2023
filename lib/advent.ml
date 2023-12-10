let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun s -> String.length (String.trim s) > 0)

let char_to_int c =
  match c with
  | '0' -> Some 0
  | '1' -> Some 1
  | '2' -> Some 2
  | '3' -> Some 3
  | '4' -> Some 4
  | '5' -> Some 5
  | '6' -> Some 6
  | '7' -> Some 7
  | '8' -> Some 8
  | '9' -> Some 9
  | _ -> None

module String = struct
  let split_in_two char str =
    match String.split_on_char char str with
    | [ elem1; elem2 ] -> (elem1, elem2)
    | _ ->
        raise (Invalid_argument (String.cat "Splits into more than two: " str))
end

module List = struct
  let to_pairs list =
    match list with
    | el1 :: el2 :: tail ->
        List.fold_left
          (fun acc el ->
            let _, y = List.hd acc in
            (y, el) :: acc)
          [ (el1, el2) ]
          tail
        |> List.rev
    | _ -> raise (Invalid_argument "List has less than two elements")

  let inspect_of_ints list =
    List.iter
      (fun x ->
        print_int x;
        print_string " ")
      list;
    print_newline ();
    list
end
