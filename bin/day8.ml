let file = "inputs/day8.in"

type direction = { from : string; left : string; right : string }

let read_direction_line line =
  let regex = Str.regexp {|\(.+\) = (\(.+\), \(.+\))|} in
  let _ = Str.string_match regex line 0 in
  let source = Str.matched_group 1 line in
  let left = Str.matched_group 2 line in
  let right = Str.matched_group 3 line in
  { from = source; left; right }

let read_directions lines = lines |> List.map read_direction_line
let read_instructions chars = chars |> String.to_seq |> List.of_seq

let rec loop_over_map current_steps current_instructions current_location
    instructions directions =
  match current_instructions with
  | now :: rest ->
      let current_turn =
        List.find (fun x -> String.equal x.from current_location) directions
      in
      let new_location =
        if now == 'R' then current_turn.right else current_turn.left
      in
      let new_current_instructions =
        if List.length rest == 0 then instructions else rest
      in
      if String.equal new_location "ZZZ" then
        print_endline (string_of_int (succ current_steps))
      else
        loop_over_map (current_steps + 1) new_current_instructions new_location
          instructions directions
  | _ -> raise (Invalid_argument "Empty steps list")

let part1 instructions directions =
  loop_over_map 0 instructions "AAA" instructions directions

let run () =
  let lines = Advent.read_lines file in
  match lines with
  | instructions :: rest ->
      let instructions = read_instructions instructions in
      let directions = read_directions rest in
      part1 instructions directions
  | _ -> raise (Invalid_argument "Wrong input format")
