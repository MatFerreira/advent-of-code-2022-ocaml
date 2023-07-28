open Base
open Stdio

let clean_input input =
  String.substr_replace_all input ~pattern:"$ ls\n" ~with_:""
  |> String.split ~on:'$'
  |> List.map ~f:String.split_lines
  |> List.filter_map ~f:(fun l ->
    match l with
    | hd :: tl ->
      let dir_name = List.last_exn (String.split hd ~on:' ') in
      if String.equal dir_name ".." then None else Some (dir_name, tl)
    | [] -> None)

let parse_sizes input =
  let input = List.rev input in
  let sizes = ref [] in
  List.iter input ~f:(fun (name, content) ->
    let size = List.fold content ~init:0 ~f:(fun acc terms ->
      let terms = String.split terms ~on:' ' in
      let (left, right) = (List.hd_exn terms, List.last_exn terms) in
      if String.equal left "dir" then
        acc + (List.Assoc.find_exn (!sizes) right ~equal:String.equal)
      else acc + (Int.of_string left)) in
    sizes := (name, size) :: !sizes);
  !sizes

let dir_sizes =
  In_channel.read_all "input.txt"
  |> clean_input
  |> parse_sizes

(* part 1 *)
let () =
  dir_sizes
  |> List.filter_map ~f:(fun (_, size) -> if size <= 100_000 then Some(size) else None)
  |> List.fold ~init:0 ~f:(+)
  |> printf "\n%d\n"

(* part 2 *)

let () =
  let disk_size = 70_000_000 in
  let update_size = 30_000_000 in
  let unused = disk_size - (List.Assoc.find_exn dir_sizes "/" ~equal:String.equal) in
  List.filter_map dir_sizes ~f:(fun (_, size) ->
    if (size + unused) >= update_size then Some size else None)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> printf "\n%d\n"


(* debug *)
(* let () =
  List.map dir_sizes ~f:(fun (name, size) ->
     name ^ " " ^ (Int.to_string size))
  |> String.concat ~sep:"\n"
  |> print_endline *)
