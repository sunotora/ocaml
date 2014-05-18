(* 目的：名前と成績の組みを受け取り、文字列を返す *)
(* seiseki : (string * string) -> string *)
let seiseki temp = match temp with
    (x, y) -> x ^ "さんの成績は" ^ y ^ "です";;

