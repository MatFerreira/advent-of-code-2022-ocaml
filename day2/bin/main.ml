open Base
open Stdio

(*
Shape score: (1 for Rock, 2 for Paper, and 3 for Scissors)
Round score: (0 if you lost, 3 if the round was a draw, and 6 if you won)

Inimigo
A = Rock
B = Paper
C = Scissors

Literalmente eu
Y = Paper
X = Rock
Z = Scissors
*)

let round_score op pl =
  let opponent = ["A", "Rock";  "B", "Paper"; "C", "Scissors"] in
  let player = ["X", "Rock";  "Y", "Paper"; "Z", "Scissors"] in
  let shape_score = ["X", 1; "Y", 2; "Z", 3] in
  let score = match List.Assoc.(find_exn opponent op ~equal:String.equal, find_exn player pl ~equal:String.equal) with
    | (op, pl) when String.equal op pl-> 3
    | ("Rock", "Paper") | ("Paper", "Scissors") | ("Scissors", "Rock") -> 6
    | (_, _) -> 0
  in score + (List.Assoc.find_exn shape_score pl ~equal:String.equal)

let total_score rounds =
  List.fold rounds ~init:0 ~f:(fun score round ->
    let r = String.split round ~on:' 'in
    (round_score (List.hd_exn r) (List.last_exn r)) + score
  )

let () =
  In_channel.read_lines "input.txt"
  |> total_score
  |> printf "\n%d\n"

let () = Day2.Part2.main ()
