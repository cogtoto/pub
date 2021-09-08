module type MONOID =
sig
  type t
  val ( <+> ) : t -> t -> t
  val neutral : t
end


module String_monoid : MONOID with type t = string  =
struct
  type t = string
  let ( <+> ) = (^)
  let neutral = ""
end
;;

String_monoid.("abc" <+> "def" <+> neutral)


let concat l = List.fold_left String_monoid.(<+>) String_monoid.(neutral) l

concat ["ab"; "cd"; "ef"] ;;

open Str ;;

let count  t =  split (regexp " ") t   |> List.length ;;

let pageA = "Hello World "
let pageB = "Foo bar "
let pageC =  "O Caml " ;;

count String_monoid.( pageA <+> pageB <+>  pageC) ;;

count(String_monoid.(pageA))  + count(String_monoid.(pageB)) + count(String_monoid.(pageC));;


