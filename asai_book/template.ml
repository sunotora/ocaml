(*****************************************************************************)
(*
let hoge = 3
let piyo = 3.0

hoge + 3
piyo +. 2.5

*)
(*****************************************************************************)
(* �֐� *)

(* �ړI�F *)
(* �֐����F �^*)
let fuga x = x * x

let test1 = fuga 1 = 1
let test2 = fuga 2 = 4
let test3 = fuga 3 = 9

(*****************************************************************************)
(* �������� *)

(* �ړI�F *)
(* �֐����F �^*)
let rec hoge x = 
  if x = 0 then 0 
           else hoge 1

(*****************************************************************************)
(* �p�^�[���}�b�` *)

(* �ړI�F *)
(* �֐����F �^*)
let hoge piyo = match piyo with
  (a, b) -> a + b

(*****************************************************************************)
(* ���R�[�h *)

(* ������\���^ *)
type hoge_t = {
  piyo : string;   (* piyo *)
  huga : int;      (* huga *)
  hage : string;   (* hage *)
}

(*****************************************************************************)
(* ���R�[�h�̃p�^�[���}�b�` *)

(* �ړI�F *)
(* �֐����F �^*)
let hoge hoge_t = match hoge_t with
  { piyo = p;  huga = h; hage =hg;} ->
  p ^ string_of_int h ^ hg

(*****************************************************************************)

(* �ړI�F *)
(* �֐����F �^*)
let hyouka gakusei = match gakusei with
  {namae = n; tensuu = t; seiseki = s} ->
    if t >= 80 then        {namae = n; tensuu = t; seiseki = "A"}
    else if t >= 70 then   {namae = n; tensuu = t; seiseki = "B"}
    else if t >= 60 then   {namae = n; tensuu = t; seiseki = "C"}
    else                   {namae = n; tensuu = t; seiseki = "D"}

(*****************************************************************************)
(* ���X�g *)
�v�f�P :: �v�f�Q :: �E�E�E :: []

[�v�f�P; �v�f�Q; �E; �E; �E;]

�֐��ɓn������([�v�f�P; �v�f�Q; �E; �E; �E;])�J�b�R����

(*****************************************************************************)
(* ���X�g�̃p�^�[���}�b�` *)

(* �ړI�F *)
(* �֐����F �^*)
let hoge lst = match lst with
    [] -> []
  | first :: rest -> first 

(*****************************************************************************)
(* �ċA�̃p�^�[���}�b�` *)

(* �ړI�F *)
(* �֐����F �^*)
let rec sum lst = match lst with
    [] -> 0
  | first :: rest -> first + sum rest

(*****************************************************************************)
(* ���R�[�h�̃��X�g�̃p�^�[���}�b�` *)

(* �ړI�F *)
(* �֐����F �^ *)
let rec count_A lst = match lst with
    [] -> 0
  | {namae = n; tensuu = t; seiseki = s} :: rest
      -> if s = "A" then 1 + count_A rest
                    else count_A rest

(*****************************************************************************)
(* �Ǐ��ϐ���` *)

(* �ړI�F *)
(* �֐����F �^ *)
let rec minimum lst = match lst with
    [] -> max_int
  | first :: rest ->
      let min_rest = minumun rest in
        if first <= min_rest
        then first
        else min_rest

(*****************************************************************************)
(* �p�^�[���}�b�`�t���Ǐ��ϐ���` *)

(* �ړI�F *)
(* �֐����F �^ *)
let rec shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {namae =n; tensuu = t; seiseki = s} :: rest ->
      let (a, b, c, d) = shukei rest in
        if s = "A"      then (a + 1, b, c, d)
        else if s = "B" then (a, b + 1, c, d)
        else if s = "C" then (a, b, c + 1, d)
        else (* s = "D" *)   (a, b, c, d + 1)

(*****************************************************************************)
(* �Q�̏����ɕ��񂾃��X�g���}�[�W����֐� *)

(* �ړI�F *)
(* �֐����F �^ *)
let rec merge lst1 lst2 = match 8lst1, lst2) with
    ([], []) -> []
  | ([], first2 :: rest2) -> lst2
  | (first1 :: rest1 ,[]) -> lst1
  | (first1 :: rest1, first2 :: rest2) ->
      if first1 < first2
        then first1 :: merge rest1 lst2
        else first2 :: merge lst1  rest2

(*****************************************************************************)
(* ���R���̍ċA *)

(* �ړI�F *)
(* �֐����F �^ *)
let rec power m n =
  if n = 0 then 1
           else m * power m (n - 1)

(*****************************************************************************)
(* �Ǐ��֐���` *)

(* �ړI�F *)
(* �֐����F �^ *)
let sum lst = 
  let add_int first rest_result = first + rest_result in
    List.fold_right add_int lst 0

(*****************************************************************************)
(* ���O�̂Ȃ��֐� *)

(* �ړI�F *)
(* �֐����F �^ *)
let sum lst = 
  List.fold_right (fun first rest_result -> first + rest_result) lst 0

let sum lst = 
  List.fold_right (+) lst 0

let length lst = 
  List.fold_right (fun first rest_result -> 1 + rest_result) lst 0

(*****************************************************************************)
(* �V�����`�̍ċA *)

(* �ړI�F *)
(* �֐����F �^ *)
let rec quick_sort lst =
  if (* �����ɓ������o��P�[�X�̏��� *)
  then (* �����ɓ������o��P�[�X *)
  else (* ����ȊO�̃P�[�X *)

let rec quick_sort lst = match lst with
    [] -> [] (* �����ɓ������o��P�[�X *)
  | first :: rest -> [] (* ����ȊO�̃P�[�X *)

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
(* �ړI�F�^����ꂽ���X�g���t���ɂ��ĕԂ� *)
(* reverse�F 'a list -> 'a list *)
let rec reverse lst = match lst with
    [] -> []
  | first :: rest -> [] (* reverse rest *)

(*****************************************************************************)
(* �؍\���̃e���v���[�g *)
(* �؂�\���^ *)
type tree_t = Empty
            | Leaf of int
            | Node of tree_t * int * tree_t;;

(* ���w�^�̖؍\����\���^ *)
type 'a tree_t = Empty
            | Leaf of 'a
            | Node of 'a tree_t * 'a * 'a tree_t;;

(* �ړI�F�؍\���̃e���v���[�g *)
(* �֐����F 'a list -> hoge *)
let rec hoge tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (n)
  | Node (t1, n, t2) -> Node ( hoge t1, n, hoge t2);;


(*****************************************************************************)
(* ��`�֐� *)
�Estring_of_int �F int��string�֕ϊ�

(*****************************************************************************)
(* �Q�ƌ^ *)

(* ��` *)
let count = ref 0;;

(* �Q�� *)
!count

(* �������� *)
count := �l

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
