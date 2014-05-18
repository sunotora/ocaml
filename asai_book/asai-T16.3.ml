let datalst3 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

(* 目的：与えられたリストを逆順にして返す *)
(* reverse： 'a list -> 'a list *)
(*                                           *)
(*let rec reverse lst = match lst with       *)
(*    [] -> []                               *)
(*  | first :: rest -> []  reverse rest *)
(* *)
(* *)
(*let rec rev lst result = match lst with        *)
(*    [] -> result                               *)
(*  | first :: rest -> rev lst (first :: result) *)
(* *)

let reverse lst = 
  let rec rev lst result = match lst with
      [] -> result
    | first :: rest -> rev rest (first :: result)
  in rev lst [];;
(* *)

reverse datalst3;;
