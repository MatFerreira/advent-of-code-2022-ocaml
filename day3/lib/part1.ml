open Base
open Stdio

let find_repeated rucksack =
  let compartment_size = (List.length rucksack / 2) in
  let (first_comp, second_comp) = List.split_n rucksack compartment_size in
  let repeated = ref ' ' in
  List.iter first_comp ~f:(fun c ->
    match List.find second_comp ~f:(fun d -> Char.equal d c) with
    | Some(ch) -> repeated := ch
    | None -> ()
  );
  !repeated

let priority ch =
  let letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHOJKLMNOPQRSTUVWXSZ" in
  String.fold_until letters ~init:0 ~f:(fun acc c ->
    if Char.equal c ch then
      Stop(acc + 1)
    else Continue(acc + 1))
  ~finish:(fun acc -> acc)

let main () =
  In_channel.read_lines "input.txt"
  |> List.map ~f:(fun rucksack -> priority (find_repeated (String.to_list rucksack)))
  |> List.fold ~init:0 ~f:(fun sum i -> sum + i)
  |> printf "\n%d\n"
