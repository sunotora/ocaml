(* 本情報を格納するデータを表す型 *)
type book_t = {
  title : string;
  auther : string;
  publisher : string;
  price : int;
  isbn : string;
};;

(* お小遣い情報を格納するデータを表す型 *)
type okozukai_t = {
  name : string;
  price : int;
  place : string;
  data : string;
};;

(* 人間の情報を格納するデータを表す型 *)
type person_t = {
  name : string;
  height : float;
  weight : float;
  birthday : string;
  bloodtype : string;
};;

let human = {name = "hogehoge"; height = 1.68; weight = 65.0; birthday = "2013/10/07"; bloodtype = "B"};;

(* 目的：person_t型を受け取り、文字列を返す *)
(* ketsueki_hyouji : person_t -> string*)
let ketsueki_hyouji person_t = match person_t with
    {name = n; height = h; weight = w; birthday = b; bloodtype = bt} ->
      n ^ "さんの血液型は" ^ bt ^ "です";;