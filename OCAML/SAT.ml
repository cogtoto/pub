(* SAT *)
open List ;;
open Constraints ;;   
exception Erreur ;;
exception NotFound of string ;;

(*type atome =  string ;;
type env = (atome * bool) list ;;
type lit =  P of atome | N of atome ;;

type disj = lit list ;; 
type cnf = disj list ;;
*)

let eval a e = 
  try List.assoc a e 
   with Not_found -> false

let eval_lit e = function
| P a -> eval a e 
| N a -> not (eval a e) ;;

let eval_disj e d = fold_left ( || ) false (map (eval_lit e) d)  ;;

let eval_cnf e c = fold_left ( && ) true (map (eval_disj e) c)  ;;

let rec filtre elt l =
  match l with
  | [] -> []
  | hd::tl -> if elt=hd then tl else hd::filtre elt tl ;;
  (* warning not tail recursive *)

let rec eval_cnf_partiel (v,b) = function
| hd::tl -> if mem v hd then 
             if b then eval_cnf_partiel (v,b) tl
             else (filtre v hd) :: eval_cnf_partiel (v,b) tl
            else hd :: eval_cnf_partiel (v,b) tl 
| [] -> [] ;;

let cons_uniq xs x = if List.mem x xs then xs else x :: xs

let remove xs = (List.fold_left cons_uniq [] xs)

let recup_litteral c = 
let rec recup_aux l = 
 match l with
| P s::tl -> s::recup_aux tl
| N s::tl -> s::recup_aux tl
| [] -> []
in remove (recup_aux (remove (flatten c)))

(*************************)
(* propagation unitaire *)
(*************************)
(* cherche liste de clauses unitaires dans une cnf *)
let find_units_cnf c = 
let rec find_units_cnf_aux c acc =
  let find_unit_clause d =
    match d with 
    | P a::[] -> P a 
    | N a::[] -> N a 
    | _ -> raise (NotFound "find_unit_clause") 
  in
  match c with
  | [] -> acc
  | hd::tl -> try find_units_cnf_aux tl ((find_unit_clause hd)::acc) 
              with (NotFound "find_unit_clause") -> find_units_cnf_aux tl acc
in find_units_cnf_aux c []
;;

let find_units_cnf2 c = 
  let rec find_units_cnf_aux c acc =
    let find_unit_clause d =
      match d with 
      | P a::[] -> a 
      | N a::[] -> a
      | _ -> raise (NotFound "find_unit_clause") 
    in
    match c with
    | [] -> acc
    | hd::tl -> try find_units_cnf_aux tl ((find_unit_clause hd)::acc) 
                with (NotFound "find_unit_clause") -> find_units_cnf_aux tl acc
  in find_units_cnf_aux c []
  ;;
let init_env c =
  let rec trs u =
    match u with
    | P a::tl -> (a,true)::trs tl
    | N a::tl -> (a,false)::trs tl
    | _  -> []
  in trs (find_units_cnf c) ;;

let extend_env c e =
  let rec trs u acc =
    match u with
    | P a::tl ->trs tl (cons_uniq acc (a,true))
    | N a::tl ->trs tl (cons_uniq acc (a,false))
    | _  -> acc
  in trs (find_units_cnf c) e ;;

(* retire literale unitaire d'une clause *)
let rec retire_unit (unit:lit) (clause:disj) =
  let rec retire_unit_aux u c =
  match c with
  | hd::tl -> 
      begin
      match hd with 
       | P a -> if mem unit clause then [] 
                else if unit = N a then retire_unit_aux unit tl else hd::retire_unit_aux unit tl
       | N a -> if mem unit clause then [] 
                else if unit = P a then retire_unit_aux unit tl else hd::retire_unit_aux unit tl
      end
  | _ -> c
  in if length clause = 1 then clause else retire_unit_aux unit clause;; 

let rec propag_unitaire c =
  let units = find_units_cnf c 
  in 
  let rec propag_unitaire_aux c units =
  match units with 
    | [] -> c
    | hd::tl -> propag_unitaire_aux (map (retire_unit hd) c) tl 
in  let res = propag_unitaire_aux c units
in if c=res then (filter (fun x -> not (x=[])) c) else propag_unitaire res ;;
(* on propage jusqu'à l'obtention d'un point fixe *)

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let compare (l1,b1) (l2,b2) = if (String.sub l1 1 2) = (String.sub l2 1 2) then 0
                              else if (String.sub l1 1 2) <= (String.sub l2 1 2)  then 1 else -1 

let print_model ml =
let rec print_model_aux (ml:env) =
    match  ml with 
    | (sym, b)::tl -> 
        if (String.sub sym 2 1)="9" then 
           if b then print_model_aux tl  ^ (String.sub sym 3 1) ^ "\n" else print_model_aux tl 
      else if  (String.sub sym 2 1)="3" || (String.sub sym 2 1)="6"  then 
           if b then print_model_aux tl  ^ (String.sub sym 3 1) ^ "|"  else print_model_aux tl  
      else 
           if b then print_model_aux tl  ^ (String.sub sym 3 1) else print_model_aux tl  
    | [] -> ""
in print_model_aux (List.sort compare ml) 

(* sat prend une cnf et rend (bool*env) *)
let sat c : (bool*env) =
  let rec sat_aux c liste_litt e : (bool*env)  = 
    let c' = propag_unitaire c in
    let e' = extend_env c' e in
      if eval_cnf e' c' then (true,e') 
      else 
        let liste_litt' = (diff (recup_litteral c') (find_units_cnf2 c')) in
        match liste_litt' with 
         | hd::tl -> let (b1, e1) = sat_aux ([P hd]::c') tl ((hd,true)::e') in
                if b1 then (b1,e1) 
                else sat_aux ([N hd]::c') tl ((hd, false)::e')
         | [] -> (false, [])
   in sat_aux c  (diff (recup_litteral c) (find_units_cnf2 c)) (init_env c) ;;

let print_sudoku (b,e) =
begin
 if b then print_string "succès\n" else print_string "échec\n" ;
 print_string (print_model  e)
end
;;

let rec print_liste = function
| hd::tl -> (print_string hd ; print_string "; "; print_liste tl)
| _ -> () ;;


let sudok1 =[ 
[P "x116"]; [P "x121"]; [P "x135"];[P "x148"];[P "x154"];[P "x169"];[P "x177"];[P "x183"];[P "x192"];
[P "x213"]; [P "x228"]; [P "x237"];[P "x242"];[P "x255"];[P "x261"];[P "x279"];[P "x284"];[P "x296"];
[P "x312"]; [P "x329"]; [P "x334"];[P "x343"];[P "x357"];[P "x366"];[P "x375"];[P "x381"];[P "x398"];
[P "x414"]; [P "x423"]; [P "x432"];[P "x449"];[P "x458"];[P "x467"];[P "x471"];[P "x486"];[P "x495"];
[P "x515"]; [P "x526"]; [P "x531"];[P "x544"];[P "x553"];[P "x562"];[P "x578"];[P "x589"];[P "x597"];
[P "x618"]; [P "x627"]; [P "x639"];[P "x646"];[P "x651"];[P "x665"];[P "x674"];[P "x682"];[P "x693"];
[P "x711"]; [P "x724"]; [P "x738"];[P "x747"];[P "x756"];[P "x763"];[P "x772"];[P "x785"];[P "x799"];
[P "x819"]; [P "x825"]; [P "x836"];[P "x841"];[P "x852"];[P "x868"];[P "x873"];[P "x887"];[P "x894"];
[P "x917"]; [P "x922"]; [P "x933"];[P "x945"];[P "x959"];[P "x964"];[P "x978"];[P "x988"]] ;; (* ;[P "x991"]];;*) 


let sudok2 = 
  [[P "x115"]; [P "x123"]; [P "x157"];
   [P "x216"]; [P "x241"]; [P "x259"];[P "x265"]; 
   [P "x329"]; [P "x338"]; [P "x386"]; 
   [P "x418"]; [P "x456"]; [P "x493"]; 
   [P "x514"]; [P "x548"]; [P "x563"]; [P "x591"] ;
   [P "x617"]; [P "x652"]; [P "x696"]; 
   [P "x726"]; [P "x772"]; [P "x788"]; 
   [P "x844"]; [P "x851"]; [P "x869"]; [P "x895"] ;
   [P "x958"]; [P "x987"]; [P "x999"]] ;;
                

   (*
let c1 = [[P "x111"; P "x112"; P "x113"; P "x114"; P "x115"; P "x116"; P "x117"; P "x118"; P "x119"];
          [P "x121"; P "x122"; P "x123"; P "x124"; P "x125"; P "x126"; P "x127"; P "x128"; P "x129"];
          [P "x131"; P "x132"; P "x133"; P "x134"; P "x135"; P "x136"; P "x137"; P "x138"; P "x139"];
          [P "x141"; P "x142"; P "x143"; P "x144"; P "x145"; P "x146"; P "x147"; P "x148"; P "x149"];
          [P "x151"; P "x152"; P "x153"; P "x154"; P "x155"; P "x156"; P "x157"; P "x158"; P "x159"];
          [P "x161"; P "x162"; P "x163"; P "x164"; P "x165"; P "x166"; P "x167"; P "x168"; P "x169"];
          [P "x171"; P "x172"; P "x173"; P "x174"; P "x175"; P "x176"; P "x177"; P "x178"; P "x179"];
          [P "x181"; P "x182"; P "x183"; P "x184"; P "x185"; P "x186"; P "x187"; P "x188"; P "x189"];
          [P "x191"; P "x192"; P "x193"; P "x194"; P "x195"; P "x196"; P "x197"; P "x198"; P "x199"]];;

let c2 = [[N "x111"; N "x121"];[N "x111"; N "x131"]; [N "x111"; N "x141"];[N "x111"; N "x151"];[N "x111"; N "x161"];[N "x111"; N "x171"];[N "x111"; N "x181"];[N "x111"; N "x191"];
          [N "x121"; N "x131"];[N "x121"; N "x141"]; [N "x121"; N "x151"];[N "x121"; N "x161"];[N "x121"; N "x171"];[N "x121"; N "x181"];[N "x121"; N "x191"];
          [N "x131"; N "x141"];[N "x131"; N "x151"]; [N "x131"; N "x161"];[N "x131"; N "x171"];[N "x131"; N "x181"];[N "x131"; N "x191"];
          [N "x141"; N "x151"];[N "x141"; N "x161"]; [N "x141"; N "x171"];[N "x141"; N "x181"];[N "x141"; N "x191"];
          [N "x151"; N "x161"];[N "x151"; N "x171"]; [N "x151"; N "x181"];[N "x151"; N "x191"];
          [N "x161"; N "x171"];[N "x161"; N "x181"]; [N "x161"; N "x191"];
          [N "x171"; N "x181"];[N "x171"; N "x191"];
          [N "x181"; N "x191"];

[N "x112"; N "x122"];[N "x112"; N "x132"]; [N "x112"; N "x142"];[N "x112"; N "x152"];[N "x112"; N "x162"];[N "x112"; N "x172"];[N "x112"; N "x182"];[N "x112"; N "x192"];
          [N "x122"; N "x132"];[N "x122"; N "x142"]; [N "x122"; N "x152"];[N "x122"; N "x162"];[N "x122"; N "x172"];[N "x122"; N "x182"];[N "x122"; N "x192"];
          [N "x132"; N "x142"];[N "x132"; N "x152"]; [N "x132"; N "x162"];[N "x132"; N "x172"];[N "x132"; N "x182"];[N "x132"; N "x192"];
          [N "x142"; N "x152"];[N "x142"; N "x162"]; [N "x142"; N "x172"];[N "x142"; N "x182"];[N "x142"; N "x192"];
          [N "x152"; N "x162"];[N "x152"; N "x172"]; [N "x152"; N "x182"];[N "x152"; N "x192"];
          [N "x162"; N "x172"];[N "x162"; N "x182"]; [N "x162"; N "x192"];
          [N "x172"; N "x182"];[N "x172"; N "x192"];
          [N "x182"; N "x192"];

          [N "x113"; N "x123"];[N "x113"; N "x133"]; [N "x113"; N "x143"];[N "x113"; N "x153"];[N "x113"; N "x163"];[N "x113"; N "x173"];[N "x113"; N "x183"];[N "x113"; N "x193"];
          [N "x123"; N "x133"];[N "x123"; N "x143"]; [N "x123"; N "x153"];[N "x123"; N "x163"];[N "x123"; N "x173"];[N "x123"; N "x183"];[N "x123"; N "x193"];
          [N "x133"; N "x143"];[N "x133"; N "x153"]; [N "x133"; N "x163"];[N "x133"; N "x173"];[N "x133"; N "x183"];[N "x133"; N "x193"];
          [N "x143"; N "x153"];[N "x143"; N "x163"]; [N "x143"; N "x173"];[N "x143"; N "x183"];[N "x143"; N "x193"];
          [N "x153"; N "x163"];[N "x153"; N "x173"]; [N "x153"; N "x183"];[N "x153"; N "x193"];
          [N "x163"; N "x173"];[N "x163"; N "x183"]; [N "x163"; N "x193"];
          [N "x173"; N "x183"];[N "x173"; N "x193"];
          [N "x183"; N "x193"];

          [N "x114"; N "x124"];[N "x114"; N "x134"]; [N "x114"; N "x144"];[N "x114"; N "x154"];[N "x114"; N "x164"];[N "x114"; N "x174"];[N "x114"; N "x184"];[N "x114"; N "x194"];
          [N "x124"; N "x134"];[N "x124"; N "x144"]; [N "x124"; N "x154"];[N "x124"; N "x164"];[N "x124"; N "x174"];[N "x124"; N "x184"];[N "x124"; N "x194"];
          [N "x134"; N "x144"];[N "x134"; N "x154"]; [N "x134"; N "x164"];[N "x134"; N "x174"];[N "x134"; N "x184"];[N "x134"; N "x194"];
          [N "x144"; N "x154"];[N "x144"; N "x164"]; [N "x144"; N "x174"];[N "x144"; N "x184"];[N "x144"; N "x194"];
          [N "x154"; N "x164"];[N "x154"; N "x174"]; [N "x154"; N "x184"];[N "x154"; N "x194"];
          [N "x164"; N "x174"];[N "x164"; N "x184"]; [N "x164"; N "x194"];
          [N "x174"; N "x184"];[N "x174"; N "x194"];
          [N "x184"; N "x194"];

          [N "x115"; N "x125"];[N "x115"; N "x135"]; [N "x115"; N "x145"];[N "x115"; N "x155"];[N "x115"; N "x165"];[N "x115"; N "x175"];[N "x115"; N "x185"];[N "x115"; N "x195"];
          [N "x125"; N "x135"];[N "x125"; N "x145"]; [N "x125"; N "x155"];[N "x125"; N "x165"];[N "x125"; N "x175"];[N "x125"; N "x185"];[N "x125"; N "x195"];
          [N "x135"; N "x145"];[N "x135"; N "x155"]; [N "x135"; N "x165"];[N "x135"; N "x175"];[N "x135"; N "x185"];[N "x135"; N "x195"];
          [N "x145"; N "x155"];[N "x145"; N "x165"]; [N "x145"; N "x175"];[N "x145"; N "x185"];[N "x145"; N "x195"];
          [N "x155"; N "x165"];[N "x155"; N "x175"]; [N "x155"; N "x185"];[N "x155"; N "x195"];
          [N "x165"; N "x175"];[N "x165"; N "x185"]; [N "x165"; N "x195"];
          [N "x175"; N "x185"];[N "x175"; N "x195"];
          [N "x185"; N "x195"];

          [N "x116"; N "x126"];[N "x116"; N "x136"]; [N "x116"; N "x146"];[N "x116"; N "x156"];[N "x116"; N "x166"];[N "x116"; N "x176"];[N "x116"; N "x186"];[N "x116"; N "x196"];
          [N "x126"; N "x136"];[N "x126"; N "x146"]; [N "x126"; N "x156"];[N "x126"; N "x166"];[N "x126"; N "x176"];[N "x126"; N "x186"];[N "x126"; N "x196"];
          [N "x136"; N "x146"];[N "x136"; N "x156"]; [N "x136"; N "x166"];[N "x136"; N "x176"];[N "x136"; N "x186"];[N "x136"; N "x196"];
          [N "x146"; N "x156"];[N "x146"; N "x166"]; [N "x146"; N "x176"];[N "x146"; N "x186"];[N "x146"; N "x196"];
          [N "x156"; N "x166"];[N "x156"; N "x176"]; [N "x156"; N "x186"];[N "x156"; N "x196"];
          [N "x166"; N "x176"];[N "x166"; N "x186"]; [N "x166"; N "x196"];
          [N "x176"; N "x186"];[N "x176"; N "x196"];
          [N "x186"; N "x196"];

          [N "x117"; N "x127"];[N "x117"; N "x137"]; [N "x117"; N "x147"];[N "x117"; N "x157"];[N "x117"; N "x167"];[N "x117"; N "x177"];[N "x117"; N "x187"];[N "x117"; N "x197"];
          [N "x127"; N "x137"];[N "x127"; N "x147"]; [N "x127"; N "x157"];[N "x127"; N "x167"];[N "x127"; N "x177"];[N "x127"; N "x187"];[N "x127"; N "x197"];
          [N "x137"; N "x147"];[N "x137"; N "x157"]; [N "x137"; N "x167"];[N "x137"; N "x177"];[N "x137"; N "x187"];[N "x137"; N "x197"];
          [N "x147"; N "x157"];[N "x147"; N "x167"]; [N "x147"; N "x177"];[N "x147"; N "x187"];[N "x147"; N "x197"];
          [N "x157"; N "x167"];[N "x157"; N "x177"]; [N "x157"; N "x187"];[N "x157"; N "x197"];
          [N "x167"; N "x177"];[N "x167"; N "x187"]; [N "x167"; N "x197"];
          [N "x177"; N "x187"];[N "x177"; N "x197"];
          [N "x187"; N "x197"];

          [N "x118"; N "x128"];[N "x118"; N "x138"]; [N "x118"; N "x148"];[N "x118"; N "x158"];[N "x118"; N "x168"];[N "x118"; N "x178"];[N "x118"; N "x188"];[N "x118"; N "x198"];
          [N "x128"; N "x138"];[N "x128"; N "x148"]; [N "x128"; N "x158"];[N "x128"; N "x168"];[N "x128"; N "x178"];[N "x128"; N "x188"];[N "x128"; N "x198"];
          [N "x138"; N "x148"];[N "x138"; N "x158"]; [N "x138"; N "x168"];[N "x138"; N "x178"];[N "x138"; N "x188"];[N "x138"; N "x198"];
          [N "x148"; N "x158"];[N "x148"; N "x168"]; [N "x148"; N "x178"];[N "x148"; N "x188"];[N "x148"; N "x198"];
          [N "x158"; N "x168"];[N "x158"; N "x178"]; [N "x158"; N "x188"];[N "x158"; N "x198"];
          [N "x168"; N "x178"];[N "x168"; N "x188"]; [N "x168"; N "x198"];
          [N "x178"; N "x188"];[N "x178"; N "x198"];
          [N "x188"; N "x198"];

          [N "x119"; N "x129"];[N "x119"; N "x139"]; [N "x119"; N "x149"];[N "x119"; N "x159"];[N "x119"; N "x169"];[N "x119"; N "x179"];[N "x119"; N "x189"];[N "x119"; N "x199"];
          [N "x129"; N "x139"];[N "x129"; N "x149"]; [N "x129"; N "x159"];[N "x129"; N "x169"];[N "x129"; N "x179"];[N "x129"; N "x189"];[N "x129"; N "x199"];
          [N "x139"; N "x149"];[N "x139"; N "x159"]; [N "x139"; N "x169"];[N "x139"; N "x179"];[N "x139"; N "x189"];[N "x139"; N "x199"];
          [N "x149"; N "x159"];[N "x149"; N "x169"]; [N "x149"; N "x179"];[N "x149"; N "x189"];[N "x149"; N "x199"];
          [N "x159"; N "x169"];[N "x159"; N "x179"]; [N "x159"; N "x189"];[N "x159"; N "x199"];
          [N "x169"; N "x179"];[N "x169"; N "x189"]; [N "x169"; N "x199"];
          [N "x179"; N "x189"];[N "x179"; N "x199"];
          [N "x189"; N "x199"]];;

let mini_sudok = [ [P "x144"];[P "x155"]; [P "x166"];[P "x177"]; [P "x188"]];;
let c = c1 @ c2 @ mini_sudok ;;

print_sudoku ( sat ( c1 @ c2 @ mini_sudok ) )   ;; 

*)
let c = c1 @ c2 @ c3 @ c4 @ c5 @ sudok1 
 in  print_sudoku (sat c)  ;; 

                
