module type Day = sig
  val run : unit
end

let days : (module Day) Array.t = [|
  (module Day1)
|]

let () = 
  let argv = Sys.argv in
  let day = argv.(1) in
  match Array.get days (int_of_string day - 1) with
  | (module Day) -> Day.run