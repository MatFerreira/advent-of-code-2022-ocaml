open Base
open Stdio

(*
A = Rock (1)
B = Paper(2)
C = Scissors (3)

Y = Draw (3)
X = Lose (0)
Z = Win (6)
*)

let round_score round =
  let score_board = [
    "A X", 3;
    "A Y", 4;
    "A Z", 8;
    "B X", 1;
    "B Y", 5;
    "B Z", 9;
    "C X", 2;
    "C Y", 6;
    "C Z", 7;
  ]
  in List.Assoc.find_exn score_board round ~equal:String.equal

let total_score rounds =
  List.fold rounds ~init:0 ~f:(fun score round -> round_score(round) + score)

let main () =
  In_channel.read_lines "input.txt"
  |> total_score
  |> printf "\n%d\n"
