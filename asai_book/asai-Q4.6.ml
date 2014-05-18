(* 目的：鶴の数から鶴の足を返す *)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi x = 2 * x;;

(* 目的：亀の数から亀の足を返す *)
(* kame_no_ashi : int -> int *)
let kame_no_ashi x = 4 * x;;

(* 目的：鶴、亀の数から亀の足を返す *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi x y = tsuru_no_ashi x + kame_no_ashi y;;

(* 目的：鶴と亀の数の合計と足の数の合計から、鶴の数を返す *)
(* tsurukame : int -> int -> int *)
let tsurukame x y = 2 * x - y / 2;;