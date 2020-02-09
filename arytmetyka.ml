(* Autor: Piotr Jasinski *)


(* Typ przechowujacy liczbe, jako sume dwoch przedzialow *)
type wartosc = {z1_inf : float; z1_sup : float; z2_inf : float; z2_sup : float}


(* Konstruktory liczb przyblizonych *)
let wartosc_od_do (x : float) (y : float) = 
    {z1_inf = x; z1_sup = y; z2_inf = nan; z2_sup = nan}

let wartosc_dokladna (x : float) = 
    {z1_inf = x; z1_sup = x; z2_inf = nan; z2_sup = nan}

let wartosc_dokladnosc (x : float) (p : float) = 
    if x >= 0.0 
        then {z1_inf = x *. (1.0 -. p /. 100.0); 
            z1_sup = x *. (1.0 +. p /. 100.0); 
            z2_inf = nan; z2_sup = nan}
        else {z1_inf = x *. (1.0 +. p /. 100.0); 
            z1_sup = x *. (1.0 -. p /. 100.0); 
            z2_inf = nan; z2_sup = nan}

(* Funkcja zamieniajaca -0.0 na 0.0 *)
let sprawdz (a : float) = 
    if a = -0.0 then 0.0 else a

(* Operacja dzielenia ktora dla dzielenia +- inf przez +- inf przyjmuje +- inf *)
let dzielenie (a : float) (b : float) =
    if classify_float a = FP_infinite && classify_float b = FP_infinite
        then
            if a = neg_infinity && b = neg_infinity
                then infinity
                else
                    if a = infinity && b = infinity
                        then infinity
                        else neg_infinity
        else a /. b
    
(* Funkcje mniejsze i wieksze zwracaja mniejszy/wiekszy element rozny od nan *)
let mniejsze (a : float) (b : float) =
    if classify_float a = FP_nan then sprawdz b else
        if classify_float b = FP_nan then sprawdz a else
            if a <= b then sprawdz a else sprawdz b
            
let wieksze (a : float) (b : float) =
    if classify_float a = FP_nan then sprawdz b else
        if classify_float b = FP_nan then sprawdz a else
            if a <= b then sprawdz b else sprawdz a

            
(* Minimum i maximum wyznaczaja najmniejsza/najwieksza liczbe nalezaca 
do iloczynu dwoch przedzialow *)
let minimum (a1, a2) (b1, b2) = 
    mniejsze (mniejsze (a1 *. b1) (a1 *. b2)) (mniejsze (a2 *. b1) (a2 *. b2))

let maximum (a1, a2) (b1, b2) = 
    wieksze (wieksze (a1 *. b1) (a1 *. b2)) (wieksze (a2 *. b1) (a2 *. b2))

    
(* Funkcja zmieniajaca przedzial o jednym koncu nan na przedzial [nan,nan] *)
let rec popraw_liste (l : (float * float) list) (acc : (float * float) list) = 
    match l with
    | [] -> acc
    | ((a1 : float), (a2 : float)) :: t -> 
        if classify_float a1 = FP_nan || classify_float a2 = FP_nan 
            then popraw_liste t (acc @ [(nan, nan)]) 
            else popraw_liste t (acc @ [(a1, a2)])

            
(* Funkcja zmienia sume przedzialow lacznych na sume przedzialow rozlacznych *)
let rec suma_przedzialow l acc (a1, a2) = 
    match l with
    | [] -> acc @ [(a1, a2)]
    | ((h1 : float), (h2 : float)) :: t -> 
        if classify_float h1 = FP_nan 
            then acc @ [(a1, a2)] 
            else 
                if h1 <= a2 
                    then suma_przedzialow t acc (a1, max a2 h2) 
                    else suma_przedzialow t (acc @ [(a1, a2)]) (h1,h2)

                    
(* Sortowanie rosnace listy po elemencie poczatkowym przedzialu *)
let rec sortowanie = function
    | [] -> []
    | ((a1 : float), (a2 : float)) :: l -> 
        wstaw (a1, a2) (sortowanie l)
    and
        wstaw ((b1 : float), (b2 : float)) = function
        | [] -> [(b1, b2)]
        | ((c1 : float), (c2 : float)) :: l -> 
            if warunek (b1, b2) (c1, c2) 
                then ((b1, b2) :: (c1, c2) :: l) 
                else ((c1, c2) :: wstaw (b1, b2) l)
        and
            warunek (a1, a2) (b1, b2) = 
                if classify_float a1 = FP_nan then false else
                    if classify_float b1 = FP_nan then true else
                        if a1 < b1 then true else
                            if a1 > b1 then false else a2 <= b2

                            
(* Funkcja dla danych dwoch zbiorow wykonuje zadane miedzy nimi dzialanie *)
let rec dzialanie (l1 : (float * float) list) (l2 : (float * float) list) 
    (acc : (float * float) list) f = 
    match l1 with
    | [] -> acc
    | ((a1 : float), (a2 : float)) :: t -> 
        dzialanie t l2 (acc @ dzialanie_pom (a1, a2) l2 [] f) f
    and
        dzialanie_pom ((a1 : float), (a2 : float)) (l2 : (float * float) list) 
        (acc : (float * float) list) f =
            match l2 with
            | [] -> acc
            | ((b1 : float), (b2 : float)) :: t -> 
                dzialanie_pom (a1, a2) t (acc @ (f (a1, a2) (b1, b2))) f

                
(* Selektory *)
let in_wartosc (w : wartosc) (x : float) =
    if classify_float w.z1_inf = FP_nan then false else
        if w.z1_inf <= x && x <= w.z1_sup then true else
            if classify_float w.z2_inf = FP_nan then false else
                w.z2_inf <= x && x <= w.z2_sup

let min_wartosc (w : wartosc) = 
    if classify_float w.z1_inf = FP_nan then nan else w.z1_inf

let max_wartosc (w : wartosc) = 
    if classify_float w.z2_sup = FP_nan then
        if classify_float w.z1_sup = FP_nan then nan else w.z1_sup
    else w.z2_sup
    
let sr_wartosc (w : wartosc) = 
    if min_wartosc w = neg_infinity && max_wartosc w = infinity 
        then nan else (min_wartosc w +. max_wartosc w) /. 2.0 

        
(* Konwersja typu wartosc na liste przedzialow i na odwrot *)
let zmien_wartosc_na_liste (w1 : wartosc) = 
    if classify_float w1.z1_inf = FP_nan then [] else
        if classify_float w1.z2_inf = FP_nan 
            then [(w1.z1_inf, w1.z1_sup)] 
            else [(w1.z1_inf, w1.z1_sup); (w1.z2_inf, w1.z2_sup)]
            
let zmien_liste_na_wartosc (ls : (float * float) list) = 
    match ls with
    | [] -> {z1_inf = nan; z1_sup = nan; z2_inf = nan; z2_sup = nan}
    | ((a1 : float), (a2 : float)) :: t ->
        match suma_przedzialow ls [] (a1, a2) with
        | [] -> {z1_inf = nan; z1_sup = nan; z2_inf = nan; z2_sup = nan}
        | ((b1 : float), (b2 : float)) :: tl ->
            match tl with
            | [] -> {z1_inf = b1; z1_sup = b2; z2_inf = nan; z2_sup = nan}
            | ((h1 : float), (h2 : float)) :: t2 ->
                {z1_inf = b1; z1_sup = b2; z2_inf = h1; z2_sup = h2}
            
(* Modyfikatory liczb przyblizonych *)
let plus (w1 : wartosc) (w2 : wartosc) = 
    let l1 = zmien_wartosc_na_liste w1
    and
        l2 = zmien_wartosc_na_liste w2
    and
        dodaj (a1, a2) (b1, b2) = [(a1 +. b1, a2 +. b2)]
    in
        let ls = sortowanie (popraw_liste (dzialanie l1 l2 [] dodaj) [])
        in
            zmien_liste_na_wartosc ls

let minus (w1 : wartosc) (w2 : wartosc) =
    let l1 = zmien_wartosc_na_liste w1
    and
        l2 = zmien_wartosc_na_liste w2
    and
        odejmij (a1, a2) (b1, b2) = [(a1 -. b2, a2 -. b1)]        
    in
        let ls = sortowanie (popraw_liste (dzialanie l1 l2 [] odejmij) [])
        in
            zmien_liste_na_wartosc ls

let razy (w1 : wartosc) (w2 : wartosc) = 
    let l1 = zmien_wartosc_na_liste w1
    and
        l2 = zmien_wartosc_na_liste w2
    and
        pomnoz (a1, a2) (b1, b2) =                                            
            if (b1 = 0.0 && b2 = 0.0) || (a1 = 0.0 && a2 = 0.0)
                then [(0.0, 0.0)] 
                else [(minimum (a1, a2) (b1, b2), maximum (a1, a2) (b1, b2))]
    in
        let ls = sortowanie (popraw_liste (dzialanie l1 l2 [] pomnoz) [])
        in
            zmien_liste_na_wartosc ls

let podzielic (w1 : wartosc) (w2 : wartosc) = 
    let l1 = zmien_wartosc_na_liste w1
    and
        l2 = zmien_wartosc_na_liste w2
    and
        podziel (a1, a2) (b1, b2) =                                             
        if b1 = 0.0 then
            if b2 = 0.0 then [(nan, nan)] else 
                if a1 < 0.0 then
                    if a2 <= 0.0 
                    then [(neg_infinity, a2 /. b2)] 
                    else [(neg_infinity, infinity)]
                else
                    if a2 > 0.0 
                    then [(a1 /. b2, infinity)] 
                    else [(0.0, 0.0)]
        else
            if b2 = 0.0 then
                if a2 > 0.0 then
                    if a1 >= 0.0 
                    then [(neg_infinity, a1 /. b1)] 
                    else [(neg_infinity, infinity)]
                else
                    if a1 < 0.0 
                    then [(a2 /. b1, infinity)] 
                    else [(0.0, 0.0)]
            else
                if b1 < 0.0 && b2 > 0.0 
                    then if a1 = 0.0 && a2 = 0.0
                        then [(0.0, 0.0)]
                        else [(neg_infinity, min (dzielenie a1 b1) 
                            (dzielenie a2 b2))] @ [(max (dzielenie a1 b2) 
                            (dzielenie a2 b1), infinity)] 
                    else 
                        if classify_float b1 = FP_infinite
                            then
                                if classify_float b2 = FP_infinite
                                    then [(minimum (a1, a2) (0.0, 0.0), 
                                        maximum (a1, a2) (0.0, 0.0))]
                                    else [(minimum (a1, a2) (0.0, 1.0 /. b2), 
                                        maximum (a1, a2) (0.0, 1.0 /. b2))]
                            else [(minimum (a1, a2) (1.0 /. b1, 1.0 /. b2), 
                                maximum (a1, a2) (1.0 /. b1, 1.0 /. b2))]
    in
        let ls = sortowanie (popraw_liste (dzialanie l1 l2 [] podziel) [])
        in
            zmien_liste_na_wartosc ls
                       
