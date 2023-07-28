open Base
open Stdio

let detect_distinct_characters stream n =
  String.fold_until stream ~init:([], 0) ~f:(fun (buf, qt) ch ->
    if (List.length buf) = n then Stop (buf, qt) else
    let search = (fun _ c -> Char.equal c ch) in
    match List.findi buf ~f:search with
    | None -> Continue (ch :: buf, qt+1)
    | Some (i, _) ->
      let (buf, _) = List.split_n buf i in
      Continue ((ch :: buf), qt+1))
  ~finish:(fun x -> x)

let find_packet_marker stream = detect_distinct_characters stream 4

let find_message_marker stream = detect_distinct_characters stream 14

let () =
  let stream = In_channel.read_all "input.txt" in
  let (marker_p, qt_p) = find_packet_marker stream in
  let (marker_m, qt_m) = find_message_marker stream in
  printf "\n[%s]\n%d\n" (String.rev(String.of_char_list marker_p)) qt_p;
  printf "\n[%s]\n%d\n" (String.rev(String.of_char_list marker_m)) qt_m
