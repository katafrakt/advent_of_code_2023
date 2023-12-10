module type Day = sig
  val run : unit -> unit
end

let days : (module Day) Array.t =
  [|
    (module Day1);
    (module Day2);
    (module Day3);
    (module Day4);
    (module Day5);
    (module Day6);
    (module Day7);
    (module Day8);
    (module Day9);
  |]

let () =
  let argv = Sys.argv in
  let day = argv.(1) in
  match Array.get days (int_of_string day - 1) with (module Day) -> Day.run ()
