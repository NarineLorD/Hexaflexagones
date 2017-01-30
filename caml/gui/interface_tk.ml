(*Interface graphique écrite avec le module Tcl/Tk*)
open Tk;;

let top = openTk ();;
Wn.title_set top "Créateur d'Hexaflexagones";;

(*Ce bouton permet de fermer l'application proprement.*)
let b = Button.create
          ~text:"Sayonara"
          ~command: (fun () -> closeTk ();
                               Printf.printf "Bye \n";
                               exit 0)
          top;;
pack [b];;

let _ = Printexc.print mainLoop ();;
