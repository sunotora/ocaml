(* 目的 ： n から1までのリストを作る *)
(* enumerate ： int -> int list *)
let rec enumerate n =
  if n = 0 then [] 
           else n :: enumerate (n - 1);;

(* ex) enumerate 10 = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1] *)

(* 目的 ： n の約数のリストを返す *)
(* divisor ： int -> int list *)
let divisor n =
  List.filter (fun x -> n mod x = 0) (enumerate n);;

(* ex) divisor 10 = [10; 5; 2; 1] *)


(* 目的 ： m 以下の完全数のリストを返す *)
(* perfect ： int -> int list *)
let perfect m =
  filter (fun n -> List.fold_right (+) (divisor n) 0 - n = n) (enumerate m);;
perfect 1000;;
(*
let perfect m =
  filter (fun n -> ( List.fold_right (+) (divisor n) 0 ) - n = n) (enumerate m)
*)

(*
let checkPerfect x = List.fold_right (+) (divisor x) 0 - x = x
let perfect m = filter checkPerfect (enumerate m)
*)

(*
let sum_divisor x = List.fold_right (+) (divisor x) 0
let checkPerfect x = (sum_divisor x - x = x)
let perfect m = filter checkPerfect (enumerate m)
*)

(*
let sum_divisor x = List.fold_right (+) (divisor x) 0
let perfect m = filter (sum_divisor) (enumerate m)

lstの要素(enumerate mの各要素)を受け取ってバインドする変数がないため、
中途半端な関数では置き換えができない。
filterの第一引数はあくまで、boolを返却する関数のみ、
# List.filter;;
# - : ('a -> bool) -> 'a list -> 'a list = <fun>

というかそもそもここでしか使用しないような関数をわざわざ名前を付けて定義する必要がない。
*)