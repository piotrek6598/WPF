(*  Autor: Piotr Jasinski *)
(*  Code review: Stanislaw Gajda *)
    
(*  Modul udostepniajacy set tablic *)
module ArraySet = Set.Make (
    struct
        let compare = Pervasives.compare
        type t = int array
    end)
    
(*  Funkcja obliczajaca NWD dwoch licz *)
let rec nwd a b = 
    if a < b then nwd b a else
    if b = 0 then a else nwd b (a mod b)

(*  Funkcja zwraca NWD wszystkich liczb z tablicy *)
let oblicz_nwd tablica = 
    let f acc l = nwd acc l in
    Array.fold_left f 0 tablica

(*  Funkcja sprawdza, czy kazda liczba z tablicy jest podzielna przez d *)
let podzielne_przez tablica d =
    let f acc l = acc && l mod d = 0 in
    Array.fold_left f true tablica
    
(*  Funkcja sprawdza, czy istnieje szklanka, ktora na koncu ma byc
    pusta lub pelna *)
let pelna_lub_pusta tablica = 
    let f acc (wys, zaw) = acc || wys = zaw || zaw = 0 in
    Array.fold_left f false tablica
    
(*  Funkcja zmienia tablice par, w dwie oddzielne tablice *)
let rozbij_na_dwie_tablice tablica = 
    let f (acc_l1, acc_l2) (w1, w2) = 
        (w1 :: acc_l1, w2 :: acc_l2) in
    let a, b = Array.fold_left f ([], []) tablica in
    (Array.of_list a, Array.of_list b)

(*  Funkcje nalewaj, wylewaj, przelej, dla danego stanu, pojemnosci szklanek, 
    zbioru stanow rozpatrzonych lub znajdujacych sie na kolejce, kolejki stanow
    nierozpatrzonych, liczby szklanek zwracaja kolejke, powiekszona o stany nierozpatrzone
    mozliwe do otrzymania w jednej operacji z biezacego stanu, zaaktualizowany zbior
    juz rozpatrzony oraz informacje czy stan koncowy zostal znaleziony *)
let rec nalewaj stan pojemnosci set kolejka n koncowy str = 
    if (n < 0 || str = true) then (set, kolejka,str) else
    let nowy_stan = Array.copy stan in
    Array.set nowy_stan n pojemnosci.(n);
    if not (ArraySet.mem nowy_stan set) 
    then 
        nalewaj stan pojemnosci (ArraySet.add nowy_stan set) 
        (nowy_stan :: kolejka) (n - 1) koncowy (str || nowy_stan = koncowy)
    else nalewaj stan pojemnosci set kolejka (n - 1) koncowy str
    
let rec wylewaj stan set kolejka n koncowy str = 
    if (n < 0 || str = true) then (set, kolejka, str) else
    let nowy_stan = Array.copy stan in
    Array.set nowy_stan n 0;
    if not (ArraySet.mem nowy_stan set)
    then 
        wylewaj stan (ArraySet.add nowy_stan set) (nowy_stan :: kolejka) 
        (n - 1) koncowy (str || nowy_stan = koncowy)
    else wylewaj stan set kolejka (n - 1) koncowy str

let przelej stan pojemnosci set kolejka n koncowy str =
    if str then (set, kolejka, true) else
    let nowa_kolejka = ref kolejka 
    and nowy_set = ref set 
    and nowy_str = ref str in
    for i = 0 to n
    do
        for j = 0 to n
        do
            if i <> j then 
            (let nowy_stan = Array.copy stan in
            if stan.(i) + stan.(j) > pojemnosci.(j)
            then 
                (Array.set nowy_stan j pojemnosci.(j); 
                Array.set nowy_stan i (stan.(i) + stan.(j) - pojemnosci.(j)))
            else 
                (Array.set nowy_stan j (stan.(i) + stan.(j)); 
                Array.set nowy_stan i 0);
            if not (ArraySet.mem nowy_stan !nowy_set)
            then 
                (nowy_set := ArraySet.add nowy_stan !nowy_set;
                nowa_kolejka := nowy_stan :: !nowa_kolejka;
                if nowy_stan = koncowy then nowy_str := true))
        done;
    done;
    (!nowy_set, !nowa_kolejka, !nowy_str)

(*  Algorytm BFS przechodzenia skierowanego grafu stanow, gdzie krawedzie lacza stany 
    mozliwe do otrzymania w jednej operacji *)
let rec bfs kolejka odleglosc set poj koncowy =
    let rec pom kolejka set poj koncowy nowa_kolejka = 
        match kolejka with
        | [] -> (set, nowa_kolejka, false)
        | h :: t ->
            let dl = Array.length h - 1 in
            let set, kol, str = wylewaj h set nowa_kolejka dl koncowy false in
            let set, kol, str = nalewaj h poj set kol dl koncowy str in
            let set, kol, str = przelej h poj set kol dl koncowy str in
            if str then (set, nowa_kolejka, true) else 
        pom t set poj koncowy kol
    in
    let nowy_set, nowa_kolejka, wynik = pom kolejka set poj koncowy [] in
    if wynik then odleglosc + 1 else
    if List.length nowa_kolejka = 0 then (-1) else
    bfs nowa_kolejka (odleglosc + 1) nowy_set poj koncowy
        
let usun_zerowe tablica = 
    let f acc (w1, w2) = 
        if w1 = 0 
        then acc
        else (w1, w2) :: acc
    in
    Array.of_list (Array.fold_left f [] tablica)
        
        
let przelewanka tablica = 
    let tablica = usun_zerowe tablica in
    if Array.length tablica = 0 
    then 0 
    else
      let poj, wys = rozbij_na_dwie_tablice tablica in
      let nwd_poj = oblicz_nwd poj in
      if not (pelna_lub_pusta tablica && podzielne_przez wys nwd_poj) 
      then (-1) 
      else
         let poczatek = Array.make (Array.length poj) 0 in
         if poczatek = wys then 0 else
            bfs [poczatek] 0 (ArraySet.add poczatek ArraySet.empty) poj wys
