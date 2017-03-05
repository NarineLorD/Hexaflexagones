(*Interface graphique écrite avec le module Tcl/Tk*)
open Tk;;
open Module;;


(*1er bloc:
Initialisation de la fenêtre*)
let top = openTk () ;;
appname_set "Constructeur d'Hexaflexagones";;
Wm.title_set top "Créateur de ZA WARUDO" ;;
Wm.geometry_set top "500x500";;
(*fin du 1er bloc*)

(*2e:
Entrée texte et le bouton d'action*)
let v = Textvariable.create ();;
Textvariable.set v "";;
let l = Label.create 
          ~textvariable:v top;;

let n = Entry.create ~width:10 ~relief:`Sunken top ;;

let ech = Button.create
            ~text: "Echo texte"
            ~command:(fun () -> 
              let e = Entry.get n in
              Textvariable.set v ( string_of_int (fonction (int_of_string e))))
            top;;
(*Fin du 2e bloc*)





  (*Troisième bloc:
affichage (echo) des raccourcis *)
  bind ~events:[`KeyPress]
  ~fields:[`KeySymString; `KeyCode]
  ~action:(fun e ->
    print_endline e.ev_KeySymString;
    Printf.printf"%d " e.ev_KeyCode ;
    flush stdout)
  top;;
(*Fin du 3e bloc*)

(*4e bloc:
Bouton d'arrêt.*)
let b = Button.create
    ~text:"KONO DIO DA"
    ~command:(fun () ->
      Printf.printf "%s\nBye.\n" (Entry.get n) ;
      closeTk () ; exit 0)
    top ;;
(*Fin du 4e bloc *)

(*5e bloc
Placement des widgets*)
pack [coe l; coe n; coe ech; coe b] ;;
(*fin du 5e bloc*)

let _ = Printexc.print mainLoop ();;
