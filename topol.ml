(*  Autor: Piotr Jasinski *)
(*  Code review: Michal Siennicki *)

open PMap

(*  Wyjatek zwracany jesli w grafie istnieje cykl *)
exception Cykliczne


(*  Funckja zmienia stopien wchodzacy wierzcholka wierz *)
(*  o ile oraz zwraca istniejaca mape z tym wierzcholkiem *)
(*  oraz liste wierzcholkow do ktorych nie wchodza krawedzie *)
let zmien_stopien wierz ile mapa lista_0 =
    if mem wierz mapa then
        if (ile + find wierz mapa) = 0 && ile = (-1)
        then (add wierz (ile + find wierz mapa) mapa, wierz::lista_0)
        else (add wierz (ile + find wierz mapa) mapa, lista_0)
    else
        (add wierz ile mapa, wierz::lista_0)

let topol lista =

(*  Mapa przechowujaca dla kazdego wierzcholka liste wierzcholkow *)
(*  do ktorych prowadza krawedzie wychodzace z niego *)
    let krawedzie =
        let f mapa (wierz, l) =
            add wierz ((try find wierz mapa with Not_found -> []) @ l) mapa in
        List.fold_left f empty lista in

(*  Mapa przechowujaca dla kazdego wierzcholka liczbe krawedzi *)
(*  do niego wchodzacych oraz liste wierzcholkow, do ktorych *)
(*  nie wchodza zadne nie przetworzone krawedzie *)
    let (ile_wchodzi, lista_0) = 
        let f mapa (wierz, l) =
            List.fold_left (fun m e -> fst (zmien_stopien e 1 m [])) mapa l in
        let mapa = List.fold_left f empty lista in
        let g (mapa, lista_0) (wierz, l) =
            zmien_stopien wierz 0 mapa lista_0 in
        List.fold_left g (mapa, []) lista in

(*  Funkcja usuwa z grafu wierzcholek, zwraca zaaktualizowana *)
(*  mape wierzcholkow i liczby krawedzi do nich wchodzacych oraz *)
(*  lista wierzcholkow do ktorych nie wchodza zadne krawedzie *)
    let usuwanie (ile_wchodzi, lista_0) wierz_usuwany =
        let wierzcholki = try find wierz_usuwany krawedzie with Not_found -> [] in
        let f (ile_wchodzi, lista_0) wierz_zmniejsz =
            zmien_stopien wierz_zmniejsz (-1) ile_wchodzi lista_0 in
        List.fold_left f (ile_wchodzi, lista_0) wierzcholki in
  
(*  Algorytm sortujacy, usuwa kolejno wierzcholki o stopniu wchodzacym 0 *)
(*  zwraca liste wierzcholkow posortowana topologicznie *)
    let rec sortowanie (ile_wchodzi, lista_0)  = 
        match lista_0 with
        | [] -> if fold (+) ile_wchodzi 0 = 0 then [] else raise Cykliczne
        | h::t -> h::(sortowanie (usuwanie (ile_wchodzi, t) h)) in
    sortowanie (ile_wchodzi, lista_0)
