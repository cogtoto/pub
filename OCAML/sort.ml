(* sort functions *)
open List
open Printf
exception Err of string ;;

let rec is_ordered l =
 match l with
 | [] | [_] -> true
 | x :: xs ->  (x <= hd xs) && is_ordered xs ;;

let test = [1;2;3;4;5]   
in is_ordered test ;;

let test2 = [1;2;3;6;4] in
is_ordered test2 ;;

(* lower than*)
let rec lt e l = 
  match l with
  | [] -> []
  | x :: xs -> if (x>=e) then lt e xs else x :: lt e xs ;; 
  
(* greater than*)
let rec gt e l =
  match l with
  | [] -> []
  | x :: xs -> if (x<e) then gt e xs else x :: gt e xs ;;

let rec sort l =
  match l with
  | [] -> []
  | x :: xs ->
    let pivot = x in
    sort (lt pivot xs) @ [pivot] @ sort (gt pivot xs) ;;

let t = sort [1;2;3;4;5;3;4;1] ;; 
(* list max*)
let rec max l =
  match l with
  | [x] -> x
  | x :: xs -> if (x>max xs) then x else max xs 
  | [] -> raise (Err "max of empty list") ;;


let t2 = max [1;2;3;4;5;3;4;1] ;;

(* list min*)
let rec min l =
  match l with
  | [x] -> x
  | x :: xs -> if (x<min xs) then x else min xs
  | [] -> raise (Err "min of empty list") ;;


let t3 = min [1;2;3;4;5;3;4;1] ;;

(* incr list by 1*)
let rec incr l =
  match l with
  | [] -> []
  | x :: xs -> (x+1) :: incr xs ;;


  
(* union of 2 integer lists*)
let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | x :: xs ->
        if (mem x l2) then union xs l2
    else x :: union xs l2 ;;

let t4 = union [1;2;3;4;5] [1;2;3;6;4] ;;
  
let t5 = union [1;2;3;6;4] [] ;;


(* intersection of 2 integer lists*)
let rec intersection l1 l2 =
  match l1 with
  | [] -> []
  | x :: xs ->
        if (mem x l2) then intersection xs l2
        else x :: intersection xs l2 ;;

let t6 = intersection [1;2;3;4;5] [1;2;3;6;4] ;;

type int_tree = 
  | Leaf of int
  | Node of int * int_tree * int_tree

(* sum of int tree *)
let rec sum_tree t =
  match t with
  | Leaf x -> x
  | Node(x, l, r) -> sum_tree l + sum_tree r + x ;;

let tree1 = Node(1, Leaf(2), Leaf(3)) ;;
let tree2 = Node(4, Leaf(5), Leaf(6)) ;;
sum_tree tree1 ;;

(* fibonacci tree*)
let rec fib_tree n =
  match n with
  | 0 -> Leaf 0
  | 1 -> Leaf 1
  | n -> Node(n, fib_tree (n-1), fib_tree (n-2)) ;;

(* print tree *)
let rec print_tree t =
  match t with
    | Leaf x -> printf "%d" x
  | Node(x, l, r) ->
    printf "Node(%d, " x ;
    print_tree l ;
    printf ", " ;
    print_tree r ;
    printf ")" ;;

print_tree (fib_tree 5) ;;

(* sum of leaves of the tree *)
let rec sum_leaves t =
  match t with
  | Leaf x -> x
  | Node(x, l, r) -> sum_leaves l + sum_leaves r ;;

print_tree (fib_tree 6) ;;
sum_leaves (fib_tree 6) ;;

(* function which adds 2 lists *)
let rec add_list l1 l2 =
  match l1 with
  | [] -> []
  | x :: xs -> x + hd l2 :: add_list xs l2 ;;

  add_list [1;2;3] [4;5;6] ;;

let rec sub_list l1 l2 =
  match l1 with
  | [] -> []
  | x :: xs -> x - hd l2 :: sub_list xs l2 ;;

  sub_list [1;2;3] [4;5;6] ;;

let rec mul_list l1 l2 =
  match l1 with
  | [] -> []
  | x :: xs -> x * hd l2 :: mul_list xs l2 ;;
  
let rec even_members l =
  match l with
  | [] -> []
  | x :: [] -> []
  | x1 :: x2 :: xs -> x2 :: even_members xs ;;

let rec odd_members l =
  match l with
  | [] -> []
  | x :: xs -> x :: even_members xs ;;



  

