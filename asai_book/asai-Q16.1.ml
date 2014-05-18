let datalst2 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

(* 目的： int型のリストを受け取り、それまでの数の合計からなるリストを返す *)
(* sum_list : int list -> int list*)
let sum_list lst = 
  (* hojo2 : int list -> int list *)
  let rec hojo2 lst total0 = match lst with
      [] -> []
    | first :: rest -> (first + total0) :: hojo2 rest (total0 + first)
  in hojo2 lst 0

sum_list datalst2

