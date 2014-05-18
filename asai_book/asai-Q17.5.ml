(* 木を表す型 *)
type tree_t = Empty
            | Leaf of int
            | Node of tree_t * int * tree_t;;

(* 目的：tree に含まれる整数をすべて二倍にした tree を返す *)
(* twice_tree : tree_t list -> tree_t list *)
let rec twice_tree tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf ( 2 * n)
  | Node (t1, n, t2) -> Node (twice_tree t1, 2 * n, twice_tree t2);;

(* test *)
let tree1 = Empty;;
let tree2 = Leaf(3);;
let tree3 = Node (tree1, 4, tree2);;
let tree4 = Node (tree2, 5, tree3);;

let test1 = twice_tree tree1 = Empty;;
let test2 = twice_tree tree2 = Leaf(6);;
let test3 = twice_tree tree3 = Node(twice_tree tree1,  8, twice_tree tree2);;
let test4 = twice_tree tree4 = Node(twice_tree tree2, 10, twice_tree tree3);;

