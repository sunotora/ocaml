(* やおやりすと *)
let yaoya_list = [("トマト", 300); ("たまねぎ", 200); ("にんじん", 150); ("ほうれん草", 200)];;

(* 売り切れを示す例外 *)
exception Urikire;;

(* 目的： item の値段を調べる *)
(* みつからないときには Urikire という例外を発生する *)
(* price : string -> (string * int) list -> int *)
let rec price item yaoya_list = match yaoya_list with
    [] -> raise Urikire
  | (yasai, nedan) :: rest ->
      if item = yasai then nedan
                      else price item rest;;

(* 目的： yasai_list を買った時の値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> int *)
let total_price yasai_list yaoya_list =
  let rec hojo yasai_list = match yasai_list with
      [] -> 0
    | first :: rest -> price first yaoya_list + hojo rest
  in try
       hojo yasai_list
     with Urikire -> 0;;

total_price ["たまねぎ"; "にんじん"] yaoya_list;;
total_price ["たまねぎ"; "にんじん"; "じゃがいも"] yaoya_list;;
