open Stdio
open Base

let build_elf_list items =
  let rec loop ?(elf=0) ?(elf_list=[]) items =
    match items with
    | calories :: tail ->
      if String.equal calories "" then
        loop tail ~elf:0 ~elf_list:(elf :: elf_list)
      else loop tail ~elf:((Int.of_string calories) + elf) ~elf_list:(elf_list)
    | [] -> elf_list
    in loop items

let () =
  let elfs = In_channel.read_lines "input.txt"
    |> build_elf_list
    |> List.sort ~compare
    |> List.rev
  in let top = List.take elfs 3
  in let sum = List.fold top ~init:0 ~f:(+)
  in printf "\n%d\n" sum;
