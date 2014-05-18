(* 目的： 逆fizzbuzz問題を解く*)
(* 逆fizzbuzz問題とは、*)
(* "fizz","buzz","fizzbuzz"というリストを受け取った際に *)
(* それらを満たす最小の数列を返す *)

(* 力技方法：*)
(* ① 1-1, 1-2, ... 1-100 *)
(*    2-2, 2-2, ... 2-100 *)
(*    ・ *)
(*    98-99, 98-100 *)
(*    99-100 *)
(*    100 とかいう数字リストを作る (範囲固定) *)
(* ② ①のすべてに対してfizzbuzzを求める *)
(* ③ fizzbuzz文字列リストに該当するリストを②から求める *)
(* ④ 最小の数列を③から求める *)

(* 目的： 入力整数がfizz, buzz, fizzbuzzとなるか判定する *)
(* isfizzbuzz : int -> bool *)
let isfizzbuzz n =
  match (n mod 3 , n mod 5) with
      (0, _) -> true
    | (_, 0) -> true
    | (_, _) -> false;;

(* 目的： 整数を受け取り、fizzbuzz結果を返す *)
(* fizzbuzzとは 3で割り切れる数字の時は fizz *)
(*              5で割り切れる数字の時は buzz *)
(*             15で割り切れる数字の時は fizzbuzz *)
(* を表示する *)
(* fizzbuzz : int -> string *)
let fizzbuzz n =
  match (n mod 3 , n mod 5) with
      (0, 0) -> "fizzbuzz"
    | (0, _) -> "fizz"
    | (_, 0) -> "buzz"
    | (_, _) -> "";;

(* 目的： 整数を受け取り、整数とfizzbuzz結果を返す *)
(* fizzbuzz : int -> ( int * string ) *)
let fizzbuzz_num n = ( n , fizzbuzz n);;

(* 目的： n から m までのリストを作る *)
(* makelst_n_to_m : int -> int -> int list *)
let makelst_n_to_m n m=
  let rec hojo n m = 
    if n = m then m :: []
                  else n :: hojo (n + 1) m
  in hojo n m;;

(* 1から60までのfizzbuzzを表示 *)
(* List.map fizzbuzz_num (makelst_n_to_m 1 60);; *)

(* 目的：整数 n と m を受け取り、n ~ m, n + 1 ~ m ・・・, ・・・ m - 1 ~ m, m というリストのリストを作る *)
let makelst n m = 
  let temp = ref [] in
  for i = n to m do
    for j = i to m do
      temp := (makelst_n_to_m i j) :: !temp;
    done;
  done;
  List.rev !temp;;

(* 目的：int list list を受け取り、(int list * string list(fizzbuzz化) )して返す *)
(* fizzbuzzes : int list -> (int list * string list) *)
let rec fizzbuzzes lst = match lst with
    [] -> []
  | first :: rest -> (first, List.map fizzbuzz (List.filter isfizzbuzz first)) :: (fizzbuzzes rest);;

(* 力技用のリストのリスト *)
(* int list list g*)
let all = makelst 1 100;;

(* io : (int list * string list) list *)
let io = fizzbuzzes all;;

(* 目的： string list を受け取り、(int list * string list) list から string list が一致するものを抽出する *)
(* tempanswer: string list -> (int list * string list) list *)
let tempanswer seq = 
      List.filter (fun tuple -> match tuple with (n, s) -> if s = seq then true else false) io;;

(* 目的： string list を受け取り、逆fizzbuzz した最小のリストを返したい *)
(* answer : string list -> int list *)
let answer seq =
  (* hojoの目的： (int list * string list) list を受け取り、listを最初から見ていき、int list の length が一番小さい list を返したい *)
  (* hojo : ('a list * 'b list) list -> 'a list -> 'a list *)
  let rec hojo lst result =
    match lst with
        [] -> result
      | first :: rest -> 
          match first with (n, s) -> 
            if result = [] then hojo rest n
                           else if (List.length result ) > (List.length n) 
                                  then hojo rest n 
                                  else hojo rest result
  in hojo (tempanswer seq) [];;

answer ["fizz"];;
answer ["fizz"; "buzz"];;
answer ["fizz"; "fizz"];;
answer ["fizz"; "buzz"; "fizz"];;
answer ["fizz"; "buzz"; "fizz"; "fizzbuzz"];;
answer ["fizz"; "buzz"; "fizz"; "fizzbuzz"; "fizz"];;

