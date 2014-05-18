(*****************************************************************************)
(*
let hoge = 3
let piyo = 3.0

hoge + 3
piyo +. 2.5

*)
(*****************************************************************************)
(* 関数 *)

(* 目的： *)
(* 関数名： 型*)
let fuga x = x * x

let test1 = fuga 1 = 1
let test2 = fuga 2 = 4
let test3 = fuga 3 = 9

(*****************************************************************************)
(* 条件分岐 *)

(* 目的： *)
(* 関数名： 型*)
let rec hoge x = 
  if x = 0 then 0 
           else hoge 1

(*****************************************************************************)
(* パターンマッチ *)

(* 目的： *)
(* 関数名： 型*)
let hoge piyo = match piyo with
  (a, b) -> a + b

(*****************************************************************************)
(* レコード *)

(* ◯◯を表す型 *)
type hoge_t = {
  piyo : string;   (* piyo *)
  huga : int;      (* huga *)
  hage : string;   (* hage *)
}

(*****************************************************************************)
(* レコードのパターンマッチ *)

(* 目的： *)
(* 関数名： 型*)
let hoge hoge_t = match hoge_t with
  { piyo = p;  huga = h; hage =hg;} ->
  p ^ string_of_int h ^ hg

(*****************************************************************************)

(* 目的： *)
(* 関数名： 型*)
let hyouka gakusei = match gakusei with
  {namae = n; tensuu = t; seiseki = s} ->
    if t >= 80 then        {namae = n; tensuu = t; seiseki = "A"}
    else if t >= 70 then   {namae = n; tensuu = t; seiseki = "B"}
    else if t >= 60 then   {namae = n; tensuu = t; seiseki = "C"}
    else                   {namae = n; tensuu = t; seiseki = "D"}

(*****************************************************************************)
(* リスト *)
要素１ :: 要素２ :: ・・・ :: []

[要素１; 要素２; ・; ・; ・;]

関数に渡す時は([要素１; 要素２; ・; ・; ・;])カッコつける

(*****************************************************************************)
(* リストのパターンマッチ *)

(* 目的： *)
(* 関数名： 型*)
let hoge lst = match lst with
    [] -> []
  | first :: rest -> first 

(*****************************************************************************)
(* 再帰のパターンマッチ *)

(* 目的： *)
(* 関数名： 型*)
let rec sum lst = match lst with
    [] -> 0
  | first :: rest -> first + sum rest

(*****************************************************************************)
(* レコードのリストのパターンマッチ *)

(* 目的： *)
(* 関数名： 型 *)
let rec count_A lst = match lst with
    [] -> 0
  | {namae = n; tensuu = t; seiseki = s} :: rest
      -> if s = "A" then 1 + count_A rest
                    else count_A rest

(*****************************************************************************)
(* 局所変数定義 *)

(* 目的： *)
(* 関数名： 型 *)
let rec minimum lst = match lst with
    [] -> max_int
  | first :: rest ->
      let min_rest = minumun rest in
        if first <= min_rest
        then first
        else min_rest

(*****************************************************************************)
(* パターンマッチ付き局所変数定義 *)

(* 目的： *)
(* 関数名： 型 *)
let rec shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {namae =n; tensuu = t; seiseki = s} :: rest ->
      let (a, b, c, d) = shukei rest in
        if s = "A"      then (a + 1, b, c, d)
        else if s = "B" then (a, b + 1, c, d)
        else if s = "C" then (a, b, c + 1, d)
        else (* s = "D" *)   (a, b, c, d + 1)

(*****************************************************************************)
(* ２つの昇順に並んだリストをマージする関数 *)

(* 目的： *)
(* 関数名： 型 *)
let rec merge lst1 lst2 = match 8lst1, lst2) with
    ([], []) -> []
  | ([], first2 :: rest2) -> lst2
  | (first1 :: rest1 ,[]) -> lst1
  | (first1 :: rest1, first2 :: rest2) ->
      if first1 < first2
        then first1 :: merge rest1 lst2
        else first2 :: merge lst1  rest2

(*****************************************************************************)
(* 自然数の再帰 *)

(* 目的： *)
(* 関数名： 型 *)
let rec power m n =
  if n = 0 then 1
           else m * power m (n - 1)

(*****************************************************************************)
(* 局所関数定義 *)

(* 目的： *)
(* 関数名： 型 *)
let sum lst = 
  let add_int first rest_result = first + rest_result in
    List.fold_right add_int lst 0

(*****************************************************************************)
(* 名前のない関数 *)

(* 目的： *)
(* 関数名： 型 *)
let sum lst = 
  List.fold_right (fun first rest_result -> first + rest_result) lst 0

let sum lst = 
  List.fold_right (+) lst 0

let length lst = 
  List.fold_right (fun first rest_result -> 1 + rest_result) lst 0

(*****************************************************************************)
(* 新しい形の再帰 *)

(* 目的： *)
(* 関数名： 型 *)
let rec quick_sort lst =
  if (* 自明に答えが出るケースの条件 *)
  then (* 自明に答えが出るケース *)
  else (* それ以外のケース *)

let rec quick_sort lst = match lst with
    [] -> [] (* 自明に答えが出るケース *)
  | first :: rest -> [] (* それ以外のケース *)

let rec quick_sort lst = match lst with
    [] -> [] 
  | first :: rest -> quick_sort (take_less first rest)
                     @ [first]
                     @ quick_sort (take_greater first rest) 

let rec quick_sort lst = 
  let take n lst p = filter ( fun item -> p item n) lst 
    in let take_less    n lst = take n lst (<)
    in let take_greater n lst = take n lst (>)
    in match lst with
      [] -> [] 
    | first :: rest -> quick_sort (take_less first rest)
                     @ [first]
                     @ quick_sort (take_greater first rest) 

(*****************************************************************************)
(* 目的：与えられたリストを逆順にして返す *)
(* reverse： 'a list -> 'a list *)
let rec reverse lst = match lst with
    [] -> []
  | first :: rest -> [] (* reverse rest *)

(*****************************************************************************)
(* 木構造のテンプレート *)
(* 木を表す型 *)
type tree_t = Empty
            | Leaf of int
            | Node of tree_t * int * tree_t;;

(* 多層型の木構造を表す型 *)
type 'a tree_t = Empty
            | Leaf of 'a
            | Node of 'a tree_t * 'a * 'a tree_t;;

(* 目的：木構造のテンプレート *)
(* 関数名： 'a list -> hoge *)
let rec hoge tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (n)
  | Node (t1, n, t2) -> Node ( hoge t1, n, hoge t2);;


(*****************************************************************************)
(* 定義関数 *)
・string_of_int ： intをstringへ変換

(*****************************************************************************)
(* 参照型 *)

(* 定義 *)
let count = ref 0;;

(* 参照 *)
!count

(* 書き換え *)
count := 値

(*****************************************************************************)
(* for *)

let xl = ref [];;
for i = 1 to 10 do
  xl := i :: !xl;
done;
!xl

List.rev !xl

for i = 10 downto 1 do
  xl := i :: !xl
done;
!xl
