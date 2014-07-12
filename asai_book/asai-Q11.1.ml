(* 目的：自然数ｎをうけとり、０から受け取った自然数までの２乗の和を求める。 *)
(* sum_of_square : int -> int *)
let rec sum_of_square n = 
  if n = 0 then 0
  else n * n + sum_of_square(n - 1)

(* テスト *)
let test1 = sum_of_square 0 = 0
let test2 = sum_of_square 1 = 1
let test3 = sum_of_square 2 = 5
let test4 = sum_of_square 3 = 14
let test5 = sum_of_square 4 = 30
let test6 = sum_of_square 5 = 55
let test7 = sum_of_square 6 = 91
