(* lambda.ml *)
open Terme
 
let t1 =
  App
    ((Lam ("x", (App ((Lam ("y", (App ((Var "x"), (Var "y"))))), (Var "u"))))),
    (Var "z"))
  
let t2 =
  App ((Lam ("x", (App ((App ((Var "x"), (Var "x"))), (Var "x"))))),
    (Lam ("x", (App ((App ((Var "x"), (Var "x"))), (Var "x"))))))
  
let y =
  Lam ("f",
    (App ((Lam ("x", (App ((Var "f"), (App ((Var "x"), (Var "x"))))))),
       (Lam ("x", (App ((Var "f"), (App ((Var "x"), (Var "x"))))))))))
  
let ym =
  App
    ((Lam ("f",
        (App ((Lam ("x", (App ((Var "f"), (App ((Var "x"), (Var "x"))))))),
           (Lam ("x", (App ((Var "f"), (App ((Var "x"), (Var "x"))))))))))),
    (Lam ("a", (Lam ("b", (Var "b"))))))
  
  
let z =
  Lam ("f",
    (App
       ((Lam ("x",
           (App ((Var "f"),
              (Lam ("v", (App ((App ((Var "x"), (Var "x"))), (Var "v"))))))))),
       (Lam ("x",
          (App ((Var "f"),
             (Lam ("v", (App ((App ((Var "x"), (Var "x"))), (Var "v"))))))))))))
  
let zm =
  App
    ((Lam ("f",
        (App
           ((Lam ("x",
               (App ((Var "f"),
                  (Lam ("v", (App ((App ((Var "x"), (Var "x"))), (Var "v"))))))))),
           (Lam ("x",
              (App ((Var "f"),
                 (Lam ("v", (App ((App ((Var "x"), (Var "x"))), (Var "v"))))))))))))),
    (Lam ("a", (Lam ("b", (Var "b"))))))
  
let zero = Lam ("f", (Lam ("x", Var "x")))

let un = Lam ("f", (Lam ("x", (App ((Var "f"), (Var "x"))))))
 
let succ =
  Lam ("n",
    (Lam ("f",
       (Lam ("x",
          (App ((Var "f"), (App ((App ((Var "n"), (Var "f"))), (Var "x"))))))))))
  
let mult =
  Lam ("m",
    (Lam ("n",
       (Lam ("f",
          (Lam ("x",
             (App ((App ((Var "m"), (App ((Var "n"), (Var "f"))))),
                (Var "x"))))))))))
  
let pred =
  Lam ("n",
    (Lam ("f",
       (Lam ("x",
          (App
             ((App
                 ((App ((Var "n"),
                     (Lam ("g",
                        (Lam ("h",
                           (App ((Var "h"), (App ((Var "g"), (Var "f"))))))))))),
                 (Lam ("u", (Var "x"))))),
             (Lam ("u", (Var "u"))))))))))
  
let vrai = Lam ("a", (Lam ("b", (Var "a"))))
  
let faux = Lam ("a", (Lam ("b", (Var "b"))))
  
let si =
  Lam ("p",
    (Lam ("a", (Lam ("b", (App ((App ((Var "p"), (Var "a"))), (Var "b"))))))))
  
let isZero = Lam ("n", (App ((App ((Var "n"), (Lam ("x", faux)))), vrai)))
  
let int2Church n =
  let rec aux =
    function
    | 0 -> Lam ("f", (Lam ("x", (Var "x"))))
    | n -> App (succ, (aux (n - 1)))
  in betaNormal (aux n)
  
let rec church2Int terme =
  match terme with
  | Lam ("f", (Lam ("x", (Var "x")))) -> 0
  | _ ->  1 + church2Int (betaNormal (App (pred, terme))) 
  
let fact =
  App (y,
    (Lam ("f",
       (Lam ("n",
          (App ((App ((App (si, (App (isZero, (Var "n"))))), un)),
             (App ((App (mult, (Var "n"))),
                (App ((Var "f"), (App (pred, (Var "n"))))))))))))))
  
let fact2 =
  App (z,
    (Lam ("f",
       (Lam ("n",
          (App ((App ((App (si, (App (isZero, (Var "n"))))), un)),
             (App ((App (mult, (Var "n"))),
                (App ((Var "f"), (App (pred, (Var "n"))))))))))))))
  
let _ = betaValeur (App (isZero, (int2Church 5)))
  
 
let _ = betaValeurForte (App ((App (mult, (int2Church 5))), (int2Church 3)))
  
let _ =   church2Int (int2Church 10) 
  
let trois = int2Church 3
let deux = int2Church 2
let quatre = int2Church 4
  

let rec repl lexbuf  =
  print_string ">> " ; flush stdout ;
   try
     let ast = Lambdasimplified.line Lambdalexical.lexana lexbuf in
            betaNormalPrint ast ; flush stdout; repl lexbuf
   with
   | Parsing.Parse_error ->  (print_endline "Erreur de syntaxe"; flush stdout; repl lexbuf)

let main = let lexbuf = Lexing.from_channel stdin in repl lexbuf 


  