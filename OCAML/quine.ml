(* quine OCAML / self-reproducting program *)

(fun x -> Printf.printf "%s %S" x x) "(fun x -> Printf.printf \"%s %S\" x x)"
