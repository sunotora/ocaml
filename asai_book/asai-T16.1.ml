(* 距離と距離の合計を持っている型 *)
type distance_t = {
  kyori : float; (* 距離 *)
  total : float; (* 距離の合計 *)
}

(* 距離データ *)
let datalst = 
[{kyori = 0.3; total = 0.}; {kyori = 0.9; total = 0.};
 {kyori = 1.4; total = 0.}; {kyori = 0.8; total = 0.};]


(* 目的：先頭からリスト中の各点までの距離の合計を計算する *)
(* total_distance : distance_t list -> distance_list *)
let total_distance lst = 
  (* hojoはtotal_distance関数内でのみの局所定義 *)
  (* ここでtotal0はこれまでの距離の合計 *)
  let rec hojo lst total0 = match lst with
      [] -> []
    | {kyori = k; total = t} :: rest ->
        {kyori = k; total = total0 +. k}
        :: hojo rest (total0 +. k)
      in hojo datalst 0.0

(* datalstに対して各点の合計を求める *)
total_distance datalst
