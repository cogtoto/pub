(* tri *)
open List ;;

let rec insert_tri elt li =
match li with
| [] -> elt::[]
| h::t -> if elt <= h then elt::h::t
          else h::(insert_tri elt t) 

let rec tri1 li =
  match li with
  | [] -> []
  | h::t -> insert_tri h (tri1 t)
;;


let rec inf elt l =
  match l with
  | [] -> []
  | h::t -> if h <= elt then h::(inf elt t)
  else inf elt t 

let rec sup elt l =
  match l with
  | [] -> []
  | h::t -> if h > elt then h::(sup elt t)
  else sup elt t 

let rec pair = function  
  | [] -> []
  | h::[] -> h::[]
  | h1::t -> h1::(pair (List.tl t))
and impair  = function
 | [] -> []
 | _::l -> pair l

let rec quick_sort li =
  match li with
  | [] -> []
  | h::[] -> li
  | h::t -> List.append (quick_sort(inf h t)) (h::(quick_sort(sup h t)))
  
;;

let rec app_sort l1 l2 = 
  match (l1,l2) with 
  | ([],_) -> l2
  | (_,[]) -> l1
  | (h1::t1, h2::t2) -> if (h1<=h2) then h1::(app_sort t1 l2) else h2::app_sort l1 t2
;;

let rec sort_fusion l = 
  match l with
| [] | [_] -> l
| _ -> app_sort (sort_fusion (pair l)) (sort_fusion (impair l))
;;


let popo = [2;3;4;1;5;2;4;9;0;5] in quick_sort  popo ;;


let pipi = [| 2;3;4;1;5;2;4;9;0;5 |] ;;


let tri_bulle tab =
  for j=((Array.length tab)-1) downto 0 do
  for i=0 to (j -1) 
   do
   if tab.(i) > tab.(i+1) then 
      let p = tab.(i) in
      tab.(i) <- tab.(i+1) ;
      tab.(i+1) <- p ;
   done ;
  done;
   tab ;;
   