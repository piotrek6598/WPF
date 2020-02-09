(* Autor: Piotr Jasinski *)
(* Review: Stanislaw Gajda *)


(* Typ wariantowy zlaczalnej sparametryzowanej kolejki priorytetowej *)
(* reprezentowanej przez drzewo lewicowe, przechowuje: lewe podrzewo, *)
(* prawe podrzewo, priorytet korzenia, dlugosc skrajnie prawej sciezki *)
(* lub przyjmuje NULL gdy jest puste *)
type 'a queue = NULL | Node of 'a queue * 'a queue * 'a * int

(* Funkcja ktora dla dwoch kolejek bedacymi drzewami lewicowymi zwraca *)
(* zlaczenie kolejek, bedace kolejka reprezentowana przez drzewo lewicowe *)
let rec join q1 q2 =
    match q1,q2 with
    | NULL, q3 -> q3
    | q3, NULL -> q3
    | Node (l1, p1, pr1, _), Node (l2, p2, pr2, _) -> 
    if pr1 < pr2 then
        match join p1 q2 with
        | NULL -> Node (NULL, NULL, pr1, 0)
        | Node (l3, p3, pr3, dl3) ->
            match l1 with
            | NULL -> Node (Node (l3, p3, pr3, dl3), NULL, pr1, 0)
            | Node (_, _, _, dl4) ->
                if dl3 < dl4 
                    then Node (l1, Node (l3, p3, pr3, dl3), pr1, dl3 + 1)
                    else Node (Node (l3, p3, pr3, dl3), l1, pr1, dl4 + 1)
    else
        match join p2 q1 with
        | NULL -> Node (NULL, NULL, pr2, 0)
        | Node (l3, p3, pr3, dl3) ->
            match l2 with
            | NULL -> Node (Node (l3, p3, pr3, dl3), NULL, pr2, 0)
            | Node (_, _, _, dl4) ->
                if dl3 < dl4
                    then Node (l2, Node (l3, p3, pr3, dl3), pr2, dl3 + 1)
                    else Node (Node (l3, p3, pr3, dl3), l2, pr2, dl4 + 1)

                    
(* Konstruktor pustej kolejki priorytetowej *)
let empty = NULL

(* Funkcja dolaczajaca element a do danej kolejki priorytetowej *)
let add a q = join (Node (NULL, NULL, a, 0)) q

(* Wyjatek podnoszony przy probie usuniecia elementu z pustej kolejki *)
exception Empty


(* Funkcja usuwa pierwszy element z kolejki priorytetowej, zwraca *)
(* ten element i kolejke elementu lub podnosi wyjatek gdy kolejka pusta *)
let delete_min q =
    match q with
    | NULL -> raise Empty
    | Node (l, p, pr, _) -> (pr, join l p)
    
(* Funkcja sprawdza czy dana kolejka jest pusta *)
let is_empty q =
    if q = NULL then true else false
   
