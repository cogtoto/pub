open Complex ;;
(* {re=2.; im=4.} *)

let appartient c =
	let rec loop n z =
		if (n > 300) then true
		else if ((norm2 z) > 4.) then false
				 else loop (n+1) (add c (mul z z)) 
	in loop 0 c  
;;

#load "/home/vincent/.opam/ocaml-base-compiler/lib/graphics/graphics.cma" ;;

#require "graphics" ;; 

open Graphics ;;

Graphics.open_graph " 500x200+0-0" ;;
Graphics.set_window_title "Mandelbrot" ;;
Graphics.set_color Graphics.blue;;

let mandelbrot () =
	for i = (-200) to 50
	do
		for j=(-125) to 125
		do
			if (appartient {re=((float_of_int i)/.100.); im=((float_of_int j)/.100.)}) 
			then plot (200+i) (200+j) 
		done
		done ;;




