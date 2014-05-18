(* 目的：リストを受け取ったら、そのリストの長さを返す *)
(* length : 'a list -> int *)
let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest

let testlst = [2; 1; 6; 4; 7; 1; 1; 1; 1; 1];;
length testlst;;
