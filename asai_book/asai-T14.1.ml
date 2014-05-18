(* 目的：リスト lst の中から 条件pを満たす要素のみを取り出す。 *)
(* filter：('a -> bool) -> 'a list -> 'a list *)
let rec filter p lst = match lst with
    [] -> []
  | first :: rest -> if p first then first :: filter p rest
    else filter p rest

(* 目的：3で割ると1余る要素の場合 trueを返す*)
(* is_mod3_1： int -> bool *)
(* let is_mod3_1 n = n mod 3 = 1 *)

(* 目的：リストlst から3で割ると1余る要素のみを取り出す *)
(* filter_mod3_1 ： int list -> int list *)

(* let rec filter_mod3_1 lst = filter is_mod3_1 lst *)
 let rec filter_mod3_1 lst =  filter (fun x -> x mod 3 = 1) lst
