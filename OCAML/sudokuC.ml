(* produce sudoku constraints *)
(*
C1 There is at least one number in each entry:
C2 Each number appears at most once in each row:
C3 Each number appears at most once in each column:
C4 & C5 : Each number appears at most once in each 3x3 sub-grid:
*)

let produce_C1 =
  print_string "let c1 = [";
  for x=1 to 9 do
    for y=1 to 9 do
      print_string "[" ;
      for z=1 to 9 do
        print_string ("P \"x"  ^ string_of_int x ^ string_of_int y ^ string_of_int z ^ (if z<>9 then "\"; " else "\"")  )
      done ;
      print_string ("]" ^ (if (x=9 && y=9)  then "" else ";") ^ "\n")  ;
    done
  done ;
  print_string "]\n";
;;

(* C2 Each number appears at most once in each column *)
let produce_C2 =
  print_string "let c2 = [";
  for y=1 to 9 do
    for z=1 to 9 do
      for x=1 to 8 do
        for i=(x+1) to 9 do
          print_string ("[ N \"x" ^ string_of_int x ^ string_of_int y ^ string_of_int z  ^ "\"; " ^
                       "N \"x" ^ string_of_int i ^ string_of_int y ^ string_of_int z  ^ "\"];\n ")
        done
      done
    done
  done ;
  print_string "]\n" 
;;

(* C3 Each number appears at most once in each column *)
let produce_C3 =
  print_string "let c3 = [";
  for x=1 to 9 do
    for z=1 to 9 do
      for y=1 to 8 do
        for i=(y+1) to 9 do
          print_string ("[ N \"x" ^ string_of_int x ^ string_of_int y ^ string_of_int z  ^ "\"; " ^
                       "N \"x" ^ string_of_int x ^ string_of_int i ^ string_of_int z  ^ "\"];\n ")
        done
      done
    done
  done ;
  print_string "]\n" 
;;

(* C4 Each number appears at most once in each 3x3 sub-grid *)
let produce_C4 =
  print_string "let c4 = [";
  for z=1 to 9 do
    for i=0 to 2 do
      for j=0 to 2 do
        for x=1 to 3 do
          for y=1 to 3 do
            for k=(y+1) to 3 do
          print_string ("[ N \"x" ^ string_of_int (3*i+x) ^ string_of_int (3*j+y) ^ string_of_int z  ^ "\"; " ^
                       "N \"x" ^ string_of_int (3*i+x) ^ string_of_int (3*j+k) ^ string_of_int z  ^ "\"];\n ")
         done
       done
     done
   done 
  done
done;
  print_string "]\n" 
;;
let produce_C5 =
  print_string "let c5 = [";
  for z=1 to 9 do
    for i=0 to 2 do
      for j=0 to 2 do
        for x=1 to 3 do
          for y=1 to 3 do
            for k=(x+1) to 3 do
              for l=1 to 3 do
          print_string ("[ N \"x" ^ string_of_int (3*i+x) ^ string_of_int (3*j+y) ^ string_of_int z  ^ "\"; " ^
                       "N \"x" ^ string_of_int (3*i+k) ^ string_of_int (3*j+l) ^ string_of_int z  ^ "\"];\n ")
         done
       done
     done
   done 
  done
done
done; print_string "]\n" 
;;
flush stdout ;;
