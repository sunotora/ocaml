(* 目的：身長(m)を与えられたら、標準体重を与えられたら標準体重を返す *)
(* hyoujun_taiju : float -> float *)
let hyoujun_taiju x= (x *. x) *. 22.0;;


(* 目的：身長(m)と体重(kg)を与えられたら, BMI指数を返す *)
(* bmi : float -> float -> float *)
let bmi x y = y /. (x *. x)
