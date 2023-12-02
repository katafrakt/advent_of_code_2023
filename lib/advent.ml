let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun s -> (String.length (String.trim s)) > 0)

  let char_to_int c =
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
