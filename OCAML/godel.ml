let rec_prim g h = 
 let rec f m n = 
  if m=0 then g n 
  else h (f (m-1) n) (m-1) n
in f 

let s n = n+1 ;;
let pr_11 n = n ;;
let pr_31 x y z =  x ;;


let pr_11 n = n ;;
let pr_31 x y z =  x ;;

let g y = pr_11 y ;;
let h x y z =   s (pr_31 x y z)   ;;

let add = rec_prim g h ;;
add 5 8 ;;

let rec add  x y  = 
 if x=0 then g y
 else h (add (x-1) y) (x-1) y

let add_iter x y =
  let r = ref (g y) in
  ( print_int !r; print_string "\n" ;for i=1 to x do (r := h !r i y ; print_int i; print_string "  : "; print_int !r; print_string "\n") done ; 
    !r
  )

let add x y =
  let r = ref (g y) in
  (for i=1 to x do r := h !r i y done ; 
    !r
  )

  let rec ack = function
  | (0,p) -> p+1
  | (n,0) -> ack (n-1, 1)
  | (n,p) -> ack (n-1, ack (n, p-1))

  
  open String ;;

  (* quine *)
  (fun s -> Printf.printf "%s %s;;" s s) "(fun s -> Printf.printf \"%s %S;;\" s s)";;

open String ;;

let rec f n i =
  if i = 0 then n
  else n *. sqrt(1. +. (f (n +. 1.) (i-1)))
  
  let rec f_latex n i =
    if i = 0 then "\\ldots"  
    else (string_of_int n)^ "\\sqrt{1 + " ^ (f_latex (n + 1) (i-1)) ^ "}" ;;
  
  print_string (f_latex 1 10) ;;	