(* 目的：受け取ったlstの要素のそれぞれの先頭に n をくっつける *)
(* add_to_each : int -> int list list -> int list list *)
let rec add_to_each n lst = match lst with
    [] -> []
  | first :: rest -> (n :: first) :: add_to_each n rest;;

(* 目的：受け取った lst の接頭語のリストを求める *)
(* prefix : int list -> int list list *)
let rec prefix lst = match lst with
    [] -> []
  | first :: rest -> [first] :: add_to_each first (prefix rest);;

(* こんなかんじでいけんじゃね？ *)
let makelst n m lst = 
    lst @ prefix (makelst_n_to_m (n + 1) m)


(* 目的： n から m までのリストを作る *)
(* makelst_n_to_m : int -> int -> int list *)
let makelst n m = 
  let makelst_n_to_m n m nown nowm=
    let rec hojo n m nown nowm= 
      if nown = nowm then nowm :: []
                     else nown :: hojo n m (nown + 1) (nowm + 1)
    in hojo n m nown nowm;;
  

(* 目的：整数 n と m を受け取り、n ~ m, n + 1 ~ m ・・・, ・・・ m - 1 ~ m, m というリストのリストを作る *)
(* makelstlst : int -> int -> int list list *)
let makelst n m = 
  (prefix (makelst_n_to_m n m);;

(* 目的： 1 ~ 100, 2 ~ 100 ・・・, ・・・ 99 ~ 100, 100 のリストを作る *)
(* makelst : int -> int -> int list list *)
let makelst n m = 
  let temp = ref [] in
  for i = n to m do
    for j = i to m do
      temp := (makelst_n_to_m i j) :: !temp;
    done;
  done;
  List.rev !temp;;

(* 力技用のリストのリストが完成 *)
(* 力技用のリストのリストをこれで完成させたい *)
let all = makelst 1 100;;

(* テスト *)

let test0 = makelst 0 0 
         = [[0]];;
let test1 = makelst 1 3
         = [ [1]; [1; 2]; [1; 2; 3]; [2]; [2; 3]; [3] ];;
let test2 = makelst 1 4
         = [ [1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]; [2]; [2; 3]; [2; 3; 4]; [3]; [3; 4]; [4] ];;
let test3 = makelst 1 1 
         = [[1]];;
let test4 = makelst 1 5 
         = [ [1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]; [1; 2; 3; 4; 5] [2]; [2; 3]; [2; 3; 4]; [2; 3; 4; 5]; [3]; [3; 4]; [3; 4; 5]; [4]; [4; 5] ];;

(* 目的： 1から n までのリストを作る *)
(* makelst_one_to_n : int -> int list *)
(* nowは現在値 *)
(*
let makelst_one_to_n n =
  let rec hojo n now = 
    if n = now then now :: []
                  else now :: hojo n (now + 1)
  in hojo n 1;;
*)

(* 目的： n から m までのリストを作る *)
(* makelst_n_to_m : int -> int -> int list *)
let makelst_n_to_m n m=
  let rec hojo n m = 
    if n = m then m :: []
                  else n :: hojo (n + 1) m
  in hojo n m;;

(* 目的： 1 - 100, 2 - 100 ・・・, ・・・ 99 - 100, 100 のリストを作る *)
(* makelst_100 : int list list *)
(*
let makelst_100 =
  let rec hojo n last = 
    if n = last then (makelst_n_to_m n last) :: []
                else (makelst_n_to_m n last) :: hojo (n + 1) last
  in hojo 1 100
*)

(* 目的： 1 - 100, 2 - 100 ・・・, ・・・ 99 - 100, 100 のリストを作る *)
(* makelstlst : int -> int -> int list list *)
let rec makelstlst first last = 
  if first = last then (makelst_n_to_m first last) :: []
                  else (makelst_n_to_m first last) :: makelstlst (first + 1) last;;

(* 力技用のリストのリストが完成 *)
let all = makelstlst 1 100;;

(*****************************************************************************)
(* 参照型 *)
(* 定義 *)
(* let count = ref 0;; *)
(* 参照 *)
(* !count *)
(* 書き換え *)
(* count := 値 *)
(*****************************************************************************)
(* for *)
(*
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
*)

(*
let all = ref [[]];;
for i = 1 to 100 do
  let temp = ref [];
  for j = i to 100 do
    all := j :: !all
  done;
  all := all :: temp
done;

for i = 0 to 100 do
  print_int i;
  print_newline ();
done;

for i = 0 to 10 do
  for j = i to 10 do
    Printf.printf("'");
    print_int i;
    Printf.printf(",");
    print_int j;
  done;
    print_newline ();
done;

let make = 
  let lst = ref [[]];;
  for i = 0 to 10 do
    for j = i to 10 do
      let templst = makelst i j in
      lst := !lst :: templst
    done;
  done;
  !lst;;

*)

(* let templst = [ [1;2;3]; [4;5;6] ] *)
(* let templst = (1 :: 2 :: 3 :: []) ::  (4 :: 5 :: 6 :: []) :: []  *)

(*
for i = 0 to 10 do
  for j = i to 10 do
    let templst = makelst i j
  done;
done;



let all = 
  for i = 1 to 100 do
    let temp = ref [];
    for j = i to 100 do
      temp := j :: !temp
    done;
    all := !all :: !temp
  done;


(* 目的： Ocamlでfizzbuzz問題を解く *)
(* fizzbuzzとは 3で割り切れる数字の時は fizz *)
(*              5で割り切れる数字の時は buzz *)
(*             15で割り切れる数字の時は fizzbuzz *)
(* と表示する *)
(* fizzbuzz : int -> string *)
let fizzbuzz n =
  match (n mod 3 , n mod 5) with
      (0, 0) -> "fizzbuzz"
    | (0, _) -> "fizz"
    | (_, 0) -> "buzz"
    | (_, _) -> "";;

let fizzbuzzlst = List.map fizzbuzz (makelst 30);;

(* 目的： 入力の値とfizzbuzz結果を返す *)
(* fizzbuzz : int -> string *)

let fizzbuzz_num n =
  ( n ,
    match (n mod 3 , n mod 5) with
        (0, 0) -> "fizzbuzz"
      | (0, _) -> "fizz"
      | (_, 0) -> "buzz"
      | (_, _) -> "" );;

let fizzbuzzlst = List.map fizzbuzz_num (makelst 60);;
*)

(* 目的： 逆fizzbuzz問題を解く*)
(* inversefizzbuzz : string list -> int list *)
(* 逆fizzbuzz問題とは、 *)
(* "fizz","buzz","fizzbuzz"というリストを受け取った際に *)
(* それらを満たす最小のfizzbuzz数列を返す *)

(* 力技方法：① 1-1, 1-2, ... 1-100 *)
(*              2-2, 2-2, ... 2-100 *)
(*              ・ *)
(*              98-99, 98-100 *)
(*              99-100 *)
(*              100 とかいう数字リストを作る *)
(* ② ①のすべてに大してfizzbuzzを求める *)


let templst = List.map makelst (makelst 100);;

let templst2 = let tempfunc lst = match lst with
    [] -> []
  | first :: rest -> List.map fizzbuzz lst :: tempfunc rest;;




(*
let rec fizzbuzzes lst = match lst with
    [] -> []
  | first :: rest -> List.map fizzbuzz (List.filter isfizzbuzz first) :: (fizzbuzzes rest);;

let rec fizzbuzzes lst = match lst with
    [] -> [""]
  | first :: rest -> 
      let templst = List.filter isfizzbuzz first in
        if List.length templst > 0 then List.map fizzbuzz templst
                                   else [""]
        :: (fizzbuzzes rest);;
*)


let is_no_string taple = match taple with
  (n, s) -> if s = [""] then false
                        else true;;

(*
let is_no_string taple = 
  match (n, s) with
    match s with
       [] -> false
     | first :: rest -> is s = [""] then false
                                    else true;;
*)

(* 目的：string list を受け取り、その最初の要素が空文字だった場合はfalse、それ以外はtrueを返す *)
let merge = List.filter (
              fun lst -> match lst with 
                  [] -> false
                | first :: rest -> 
                    if first = "" then false
                                  else true
              ) io;;



(* 目的：(int * string) list list を受け取り、stringがすべて空文字の場合 false, それ以外はtrueを返す *)
(* isNotEmpty : (int * string) list *)
let isNotEmpty lst = 
  let temp = false in
  let rec hojo lst temp =
    match lst with
      [] -> temp
      | first :: rest -> match (i, s) with
                     if not (s == "") then temp = true
                                      else hojo rest flg
  in hojo lst false;;



let answer = List.filter (fun taple -> match taple with (n, s) -> if s == ["fizz"] then true else false) io

let io_filtered seq = List.filter (fun taple -> match taple with (n, s) -> if s = seq then true else false) io


let temp = [([1; 2; 3], ["fizz"])];;
List.filter (fun taple -> match taple with (n, s) -> if s == ["fizz"] then true else false) temp

let temp2 = ([1; 2; 3], ["fizz"]);;
let hogea seq taple = match taple with (n, s) -> if s == seq then true else false;

let hogea seq taple = match taple with (n, s) -> s = seq;;

hogea ["fizz"] temp2;;

(*
(* 目的：(int list * string list)を受け取り、string listが[]の場合をフィルタリングする *)
let io_filtered = List.filter 
           (fun taple -> match taple with
             (n, s) -> if s = [] then false
                                   else true) io;;
*)



(* 目的：(int list * string list) list の中から 数列が最小のものを返したい*)
let answer seq = List.hoge (tempanswer seq ) nanka;;




(*
よしひろさんの
  def fizzbuzzes(range : Seq[Int]) =
    range.map(x => (x%3, x%5)).collect {
      case (0, 0) => "fizzbuzz"
      case (0, _) => "fizz"
      case (_, 0) => "buzz"
    }

  val all = for {
    a <- (1 to 100)
    b <- (a to 100)
  } yield (a to b)

  val io = all.map (input => (input, fizzbuzzes(input)))

  def answer(expected : Seq[String]) = io.
    filter { case (i, o) => o == expected }.
    minBy { case (i, _) => i.length }.
    _1
*)


  let rec hojo lst result =
      match lst with
         [] -> result
       | first :: rest ->
           match first with
             (n, s ) -> 
               if (List.length result) > (List.length n) then hojo rest n
                                                         else hojo rest result


let rec getmin lst result =
 match lst with
    [] -> result
   | first :: rest -> 
       match first with (n, s) -> 
           if (List.length result ) > (List.length n) 
             then hojo rest n 
             else hojo rest result;;
let answer seq = getmin (tempanswer seq) [1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1];;


let rec getmin lst result =
 match lst with
    [] -> result
   | first :: rest -> 
       match first with (n, s) -> 
           if result = [] then hojo rest n
                          else
                            if (List.length result ) > (List.length n) 
                              then hojo rest n 
                              else hojo rest result;;
let answer seq = getmin (tempanswer seq) [];;

(* let tempanswer seq = 
      List.filter (fun tuple -> match tuple with (n, s) -> if s = seq then true else false) io;; *)

(* 目的：string listを受け取り、 (int list * string list) list の string listと一致するものをリストとして返したい *)
let tempanswer seq = 
      List.filter (fun tuple -> match tuple with (n, s) -> if s = seq then true else false) (fizzbuzzes (makelst 1 100));;

(* tempanswer ["fizz"; "fizz"; "buzz"; "fizz"; "fizzbuzz"];; *)

(* 目的： (int list * string list) list を受け取り、listを最初から見ていき、int list の length が一番小さい list を返したい *)
(* answer : (int list * string list) list -> int list *)
let answer seq =
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

(* 力技用のリストのリストが完成 *)
(* int list list g*)
(* let all = makelst 1 100;; *)

(* io : (int list * string list) list *)
(* let io = fizzbuzzes all;; *)

