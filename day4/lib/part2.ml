open Base
open Stdio

let overlap pair =
  let (x, y) = List.hd_exn pair in
  let (a, b) = List.last_exn pair in
  if (x >= a && x <= b) || (a >= x && a <= y) then true else false

  let main' () =
    In_channel.read_lines "input.txt"
    |> Part1.parse_range
    |> List.filter ~f:overlap
    |> List.length
    |> printf "\n%d\n"
