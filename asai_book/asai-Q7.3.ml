(* 目的：平面座標を受け取ったら、x軸について対称な点を返す *)
(* taisyo_x : (int * int) -> (int * int) *)
let taisyo_x zahyou = match zahyou with
    (x, y) -> (x , -y);;

(* 目的：平面座標をふたつ受け取ったら、その中点の座標を返す *)
(* chuten : (int * int) -> (int * int) -> (int * int) *)
let chuten zahyou1 zahyou2 = match zahyou1 with
    (x1, y1) -> match zahyou2 with
      (x2, y2) -> ((x1 +. x2) /. 2.0 , (y1 +. y2) /. 2.0 );;
