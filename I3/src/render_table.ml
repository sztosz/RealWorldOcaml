open Core

let max_width header rows =
  let lenghts l = List.map ~f:String.length l in
  let max_rows_length previous row = List.map2_exn previous (lenghts row) ~f:Int.max in
  List.fold rows
    ~init:(lenghts header)
    ~f:max_rows_length

let render_separator widths =
  let dashes w = String.make (w + 2) '-' in
  let pieces = List.map widths ~f:dashes in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"

let pad string length =
  " " ^ string ^ String.make (length - String.length string + 1) ' '

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"

let render_table header rows =
  let widths = max_width header rows in
  let header = render_row header widths in
  let separator = render_separator widths in
  let proper_width_rendered_row row = render_row row widths in
  let rows = List.map rows ~f:proper_width_rendered_row in

  String.concat ~sep:"\n" (header::separator::rows)


let header = 
  ["first header"; "second header"; "third header"]

let rows =
  [
    ["1223123"; "23111111"; "3211111111111111111"];
    ["3123"; "111111111111"; "321111111111111111111111111111"];
    ["123123123"; "23111111111111"; "32111111111111111111"];
    ["3123123123"; "2311111111"; "321111111111111111111111111111"];
    ["12123123123"; "23111111111"; "321111111111"];
    ["1233123"; "1111111"; "3211111211111111"];
  ]
let () = Printf.printf "%s" (render_table header rows)
