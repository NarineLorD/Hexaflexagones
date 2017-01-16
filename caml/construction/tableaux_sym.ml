(*
Implémentations  de listes symétriques pour la représentation de flexagones
Représentaiton impérative
 *)

type liste_sym = {
    taille:int;
    liste:int list array; (*on doit toujours avoir taille <= Array.length(liste)*)
    debut:int; (*0 <= debut < taille*)
    fin:int; (*0 <= fin < taille*)
  };;

(*Remarque:
Avec  l une liste_sym où Array.length(l.liste) = n
on peut représenter n'importe quel flexagone de taille au plus n.
 *)



let rotation lsym k = {taille = lsym.taille;
                       liste = lsym.liste;
                       debut = lsym.debut + k mod lsym.taille;
                       fin = lsym.fin + k mod lsym.taille;
                      };;

let symetrie lsym = {taille = lsym.taille;
                     liste=  lsym.liste;
                     debut = lsym.fin;
                     fin = lsym.debut;
                    };;



