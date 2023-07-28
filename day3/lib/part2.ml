open Base
open Stdio

let find_badge gp =
  let first = List.nth_exn gp 0 in
  let second = List.nth_exn gp 1 in
  let third = List.nth_exn gp 2 in
  String.fold_until first ~init:' ' ~f:(fun badge ch ->
    let search = fun c -> Char.equal c ch in
    match String.find second ~f:search, String.find third ~f:search with
    | (Some _, Some _) -> Stop(ch)
    | (_, _) -> Continue(badge) )
  ~finish:(fun badge -> badge)

let main () =
  In_channel.read_lines "input.txt"
  |> List.groupi ~break:(fun i _ _ -> i % 3 = 0)
  |> List.map ~f:(fun gp -> Part1.priority (find_badge gp))
  |> List.fold ~init:0 ~f:(fun sum p -> sum + p)
  |> printf "\n%d\n"
