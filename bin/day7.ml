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

let calculate_power cards =
  let non_uniq_cards =
    count_unique_elements_hashtbl cards |> List.filter (fun (_, x) -> x > 1)
  in
  match non_uniq_cards with
  | [ (_, 5) ] -> 6
  | [ (_, 4) ] -> 5
  | [ (_, 3); (_, 2) ] -> 4
  | [ (_, 2); (_, 3) ] -> 4
  | [ (_, 3) ] -> 3
  | [ (_, 2); (_, 2) ] -> 2
  | [ (_, 2) ] -> 1
  | _ -> 0

let read_hand line =
  let cards, bid = Advent.String.split_in_two ' ' line in
  let card_values =
    cards |> String.to_seq |> List.of_seq |> List.map card_char_to_int
  in
  {
    cards = card_values;
    bid = int_of_string bid;
    power = calculate_power card_values;
  }

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

let run () =
  file |> Advent.read_lines |> List.map read_hand |> sort_hands
  |> List.mapi (fun i el -> (i + 1) * el.bid)
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline
