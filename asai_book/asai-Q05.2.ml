(* 目的：時間を受け取ったら午前か午後かを返す *)
(* jikan : int -> string *)
let jikan x = 
    if x < 12 then "午前"
              else "午後";;

