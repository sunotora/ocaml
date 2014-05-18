(* 目的：整数のリストを受け取ったら、その中の偶数の要素飲みを含むリストを返す *)
(* even : int list -> int list *)
let rec even lst = match lst with
    [] -> []
  | first :: rest ->
     if first mod 2 = 0 then first :: even rest
                          else even rest;;
let testlst = [2; 1; 6; 4; 7];;
even testlst;;


(* 目的：文字列のリストを受け取ったら、その中の要素を前から順に全部くっつけた文字列を返す *)
(* concat : string lst -> string *)
let rec concat lst = match lst with
    [] -> ""
  | first :: rest -> first ^ concat rest;;

let testlst = ["春";"夏";"秋";"冬"];;
concat testlst;;