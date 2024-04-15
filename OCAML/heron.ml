(* algorithme de heron *)
let heron n iter =
  begin
  let res = ref n in
  for i=1 to iter do
    res := (!res +. n /. !res) /. 2. 
  done;
  !res 
end
;;

heron 3. 4 ;;
sqrt 3. ;;

(* 
   
definition fonction heron n iteration =
 resultat := n ;
 pour i=1 jusqu'à iterations faire
    resultat := (resultat + n / resultat) / 2
 retourner resultat

heron 3 10 
 >> 1.41421356237 

 sqrt 2 
 >> 1.41421356237

*)

 

   