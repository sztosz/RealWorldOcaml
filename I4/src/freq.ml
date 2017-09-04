open Core

let build_counts () =
  let get_line_count lines_count line =
    match List.Assoc.find lines_count line ~equal:(=) with
    | None -> 0
    | Some x -> x in
  let count_lines counts_list line =
    let line_count = get_line_count counts_list line in
    List.Assoc.add counts_list line (line_count + 1) ~equal:(=) in
  In_channel.fold_lines In_channel.stdin ~init:[] ~f:count_lines

let () =
  build_counts ()
  |> List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)