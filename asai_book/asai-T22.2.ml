(* 目的： 1から n までのリストを作る *)
(* enum2 : int -> int list *)
(* nowは現在値 *)
let enum2 n =
  let rec hojo n now = 
    if n = now then now :: []
                  else now :: hojo n (now + 1)
  in hojo n 1;;

(* フィボナッチ数列を求める *)
(* fib : int -> int *)
let rec fib n =
  if n < 2 then n else fib ( n - 1 ) + fib ( n - 2 );;

List.map fib (enum2 15);;

(****************************************************)
(* 値の書き換え *)
(* let count = ref 0;; *)
(* !count;; *)
(* count := 5;; *)
(* !count;; *)
(****************************************************)

let count = ref 0;;
let rec fib2 n =
  (count := !count + 1;
   if n < 2 then n else fib2 ( n - 1 ) + fib2 ( n - 2 ));;

fib2 15;;

List.map fib2 (enum2 15);;
