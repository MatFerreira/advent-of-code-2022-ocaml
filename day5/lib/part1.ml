open Base
open Stdio

let stacks = [|
  ['M'; 'F'; 'C'; 'W'; 'T'; 'D'; 'L'; 'B'];
  ['L'; 'B'; 'N'];
  ['V'; 'L'; 'T'; 'H'; 'C'; 'J'];
  ['W'; 'J'; 'P'; 'S'];
  ['R'; 'L'; 'T'; 'F'; 'C'; 'S'; 'Z'];
  ['Z'; 'N'; 'H'; 'B'; 'G'; 'D'; 'W'];
  ['N'; 'C'; 'G'; 'V'; 'P'; 'S'; 'M'; 'F'];
  ['Z'; 'C'; 'V'; 'F'; 'J'; 'R'; 'Q'; 'W'];
  ['H'; 'L'; 'M'; 'P'; 'R']
|]

let move stacks (qt, s, s') =
  for _ = 1 to qt do
    match stacks.(s-1) with
    | crate :: tl ->
      stacks.(s'-1) <- crate :: stacks.(s'-1);
      stacks.(s-1) <- tl
    | [] -> ()
  done

(* part 2 *)
let ordered_move stacks (qt, s, s') =
  let (moved_crates, remaining) = List.split_n stacks.(s-1) qt in
  stacks.(s'-1) <- moved_crates @ stacks.(s'-1);
  stacks.(s-1) <- remaining

let parse_instruction line =
  String.split line ~on:' '
  |> List.filter_map ~f:(fun s ->
      Option.try_with((fun _ -> Int.of_string s)))

let main () =
  In_channel.read_lines "input.txt"
  |> List.map ~f:(fun (line) ->
    let inst = parse_instruction line in
    let qt = List.nth_exn inst 0 in
    let s = List.nth_exn inst 1 in
    let s' = List.nth_exn inst 2 in
    (qt, s, s' ))
  |> List.iter ~f:(fun t -> ordered_move stacks t);
  for i = 0 to (Array.length stacks) - 1 do
    printf "%c" (List.hd_exn stacks.(i))
  done
