let file = "inputs/day7.in"

type hand = { cards : int list; power : int; bid : int }

let count_unique_elements_hashtbl list =
  let counter = Hashtbl.create 10000 in
  let update_counter x =
    if Hashtbl.mem counter x then
      let current_count = Hashtbl.find counter x in
      Hashtbl.replace counter x (succ current_count)
    else Hashtbl.replace counter x 1
  in
  List.iter update_counter list;
  Hashtbl.to_seq counter |> List.of_seq

let card_char_to_int ch =
  match ch with
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | num -> int_of_string (String.make 1 num)

let card_char_to_int_with_jokers ch =
  let x = card_char_to_int ch in
  if x == 11 then 0 else x

let tallied_to_power tallied =
  match tallied with
  | [ (_, 5) ] -> 6
  | [ (_, 4) ] -> 5
  | [ (_, 3); (_, 2) ] -> 4
  | [ (_, 2); (_, 3) ] -> 4
  | [ (_, 3) ] -> 3
  | [ (_, 2); (_, 2) ] -> 2
  | [ (_, 2) ] -> 1
  | _ -> 0

let calculate_power cards =
  cards |> count_unique_elements_hashtbl
  |> List.filter (fun (_, x) -> x > 1)
  |> tallied_to_power

let calculate_power_with_jokers cards =
  let find_max vals =
    List.fold_left (fun acc (_, el) -> if el > acc then el else acc) 0 vals
  in
  let add_jokers_to_max (value, num_of_jokers, list) (x, current) =
    if current == value then
      let elem = (x, current + num_of_jokers) in
      (value, 0, list @ [ elem ])
    else (value, num_of_jokers, list @ [ (x, current) ])
  in
  let without_jokers : (int * int) list =
    cards |> List.filter (fun x -> x != 0) |> count_unique_elements_hashtbl
  in
  let num_of_jokers = List.length (List.filter (fun x -> x == 0) cards) in
  let max_repetitions = find_max without_jokers in
  let _, _, l =
    if num_of_jokers == 5 then (1, 2, [ (1, 5) ])
    else
      List.fold_left add_jokers_to_max
        (max_repetitions, num_of_jokers, [])
        without_jokers
  in
  List.filter (fun (_, x) -> x > 1) l |> tallied_to_power

let read_hand fn power_fn line =
  let cards, bid = Advent.String.split_in_two ' ' line in
  let card_values = cards |> String.to_seq |> List.of_seq |> List.map fn in
  { cards = card_values; bid = int_of_string bid; power = power_fn card_values }

let print_hand hand =
  let _ =
    hand.cards
    |> List.iter (fun x ->
           print_int x;
           print_char ' ');

    print_string " : ";
    print_int hand.power;
    print_endline ""
  in
  ()

let rec compare_cards_order cards1 cards2 =
  match (cards1, cards2) with
  | x :: rest1, y :: rest2 ->
      if x == y then compare_cards_order rest1 rest2 else x - y
  | _ -> 0

let cmp_hands h1 h2 =
  if h1.power == h2.power then compare_cards_order h1.cards h2.cards
  else h1.power - h2.power

let sort_hands hands = List.sort cmp_hands hands

let part1 () =
  file |> Advent.read_lines
  |> List.map (fun x -> read_hand card_char_to_int calculate_power x)
  |> sort_hands
  |> List.mapi (fun i el -> (i + 1) * el.bid)
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline

let part2 () =
  file |> Advent.read_lines
  |> List.map (fun x ->
         read_hand card_char_to_int_with_jokers calculate_power_with_jokers x)
  |> sort_hands
  |> List.mapi (fun i el -> (i + 1) * el.bid)
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline

let run () =
  part1 ();
  part2 ();
  ()
