open Base
open Stdio

let parse_range lines =
  List.map lines ~f:(fun line ->
    let range_list = String.split line ~on:',' in
    List.map range_list ~f:(fun (range) ->
      let range_bounds = String.split range ~on:'-' in
      let lower_bound = Int.of_string (List.hd_exn range_bounds) in
      let upper_bound = Int.of_string (List.last_exn range_bounds) in
      (lower_bound, upper_bound)))

let fully_contains pair =
  let (x, y) = List.hd_exn pair in
  let (a, b) = List.last_exn pair in
  if (x <= a && y >= b) || (a <= x && b >= y) then true else false

let main () =
  In_channel.read_lines "input.txt"
  |> parse_range
  |> List.filter ~f:fully_contains
  |> List.length
  |> printf "\n%d\n"
