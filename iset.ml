(* Autor: Piotr Jasinski *)

(*  Typ drzewa AVL, przechowuje lewe podrzewo,
    węzeł zawierający początek, koniec reprezentowanego
    przedziału, prawe podrzewo, liczbę elementów w  drzewie,
    wysokość drzewa *)
type t = 
    | NULL 
    | Node of t * (int * int) * t * int * int

(*  Konstruktor pustego drzewa *)
let empty = NULL

(*  Funkcja stwierdza czy dane drzewo jest puste *)
let is_empty s = s = NULL

(*  Funkcja pomocnicza zwraca wysokość danego drzewa *)
let height = function
    | Node (_, _, _, _, h) -> h
    | NULL -> 0
    
(*  Funkcja pomocnicza zwraca liczbę liczb w danym drzewie *)
let integers = function
    | Node (_, _, _, ile, _) -> ile
    | NULL -> 0

(*  Funkcja pomocnicza, tworzy węzeł drzewa o danych poddrzewach *)
let make l (p, k) r = 
    let x = integers l + integers r
    and y = k - p + 1 in
    if x < 0 || y <= 0 
    then Node (l, (p, k), r, max_int, max (height l) (height r) + 1) 
    else if x + y < 0 
    then Node (l, (p, k), r, max_int, max (height l) (height r) + 1)
    else Node (l, (p, k), r, x + y, max (height l) (height r) + 1)
    
(*  Funkcja pomocnicza balansująca drzewa, zmniejsza różnicę wysokości o 1 *)
let bal l k r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
        match l with
        | Node (ll, lk, lr, _, _) ->
            if height ll >= height lr then make ll lk (make lr k r)
            else
                (match lr with
                | Node (lrl, lrk, lrr, _, _) ->
                    make (make ll lk lrl) lrk (make lrr k r)
                | NULL -> assert false)
        | NULL -> assert false
    else if hr > hl + 2 then
        match r with
        | Node (rl, rk, rr, _, _) ->
            if height rr >= height rl then make (make l k rl) rk rr
            else
                (match rl with
                | Node (rll, rlk, rlr, _, _) ->
                    make (make l k rll) rlk (make rlr rk rr)
                | NULL -> assert false)
        | NULL -> assert false
    else make l k r

(*  Funkcja pomocnicza porządkująca rozłączne przedziały *)
let cmp (p1, k1) (p2, k2) = k1 - p2

(*  Funkcja pomocnicza, dodaje zadany przedział do drzewa *)
let rec add_one cmp (b, e) = function
    | Node (l, k, r, ile, h) ->
        let c = cmp (b, e) k in
        if c = 0 then Node (l, (b, e), r, ile, h)
        else if c < 0 then
            let nl = add_one cmp (b, e) l in
            bal nl k r
        else
            let nr = add_one cmp (b, e) r in
            bal l k nr
    | NULL -> 
        if e - b + 1 <= 0 then Node (NULL, (b, e), NULL, max_int, 1)
        else Node (NULL, (b, e), NULL, e - b + 1, 1)

(*  Funkcja sprawdzająca czy w zbiorze występuje wartość x *)
let mem x s = 
    let rec pom x s =
        match s with
        | NULL -> 0
        | Node (l, (p, k), r, _, _) ->
            if x >= p
            then if x > k then pom x r else 1
            else pom x l
    in
        1 = pom x s
        
(*  Funkcja zwracająca listę przedziałów w zbiorze *)
let elements s =
    let rec loop acc = function
        | NULL -> acc
        | Node (l, k, r, _, _) -> loop (k :: loop acc r) l in
    loop [] s
    
(*  Funkcja fold dla struktury drzewa *)
let fold f s a = 
    let rec loop acc = function
        | NULL -> acc
        | Node (l, k, r, _, _) ->
            loop (f k (loop acc l)) r in
    loop a s
    
(*  Funkcja aplikuje zadaną f kolejno do wszytkich przedziałów *)
let iter f s = 
    let rec loop = function
        | NULL -> ()
        | Node (l, k, r, _, _) -> loop l; f k; loop r in
    loop s
    
(*  Funkcja zwraca liczbę elementów w drzewie mniejszcyhc od zadanego *)
let below n s =
    let rec loop n acc = function
        | NULL -> acc
        | Node (l, (p, k), r, ile, _) ->
            if n < p then loop n acc l else
            if n > k then 
                if acc + integers l + k - p + 1 < acc 
                then loop n max_int r 
                else loop n (acc + integers l + k - p + 1) r
            else if acc + (integers l) + n - p + 1 <= 0
                then max_int else acc + (integers l) + n - p + 1
    in
        loop n 0 s
        
(*  Funkcja pomocnicza, z dwóch drzew i danego przedziału tworzy nowe drzewo *)
let rec join cmp l v r = 
    match l, r with
    | NULL, _ -> add_one cmp v r
    | _, NULL -> add_one cmp v l
    | Node (ll, lv, lr, lile, lh), Node (rl, rv, rr, rile, rh) ->
        if lh > rh + 2 then bal ll lv (join cmp lr v r) else
        if rh > lh + 2 then bal (join cmp l v rl) rv rr else
        make l v r
        
(*  Funkcja zwraca zbiór elementów mniejszych od zadanego, informacje czy
    zadany element znajdował się w zbiorze, zbiór elementów większych od zadanego*)
let split x s =
    let rec loop x = function
        | NULL -> (NULL, false, NULL)
        | Node (l, (p, k), r, ile, h) ->
            if x < p then
                let (ll, pres, rl) = loop x l in
                (ll, pres, join cmp rl (p, k) r) else
            if x > k then
                let (lr, pres, rr) = loop x r in 
                (join cmp l (p, k) lr, pres, rr) else
            if p = x
            then if k = x 
                then (l, true, r) 
                else (l, true, join cmp r (x + 1, k) empty)
            else if k = x 
                then (join cmp l (p, x - 1) empty, true, r) 
                else (join cmp l (p, x - 1) empty, true, join cmp r (x + 1, k) empty)
    in
        loop x s

(*  Funkcja pomocnicza, znajduje najmniejszy przedział *)
let rec min_elt = function
    | Node (NULL, k, _, _, _) -> k
    | Node (l, _, _, _, _) -> min_elt l
    | NULL -> raise Not_found

(*  Funkcja pomocnicza, usuwa najmniejszy przedział, zwraca zbalansowane drzewo *)
let rec remove_min_elt = function
    | Node (NULL, _, r, _, _) -> r
    | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
    | NULL -> invalid_arg "Iset.remove_min_elt"
    
(*  Funkcja łączy dwa drzewa w jedno *)
let merge t1 t2 = 
    match t1, t2 with
    | NULL, _ -> t2
    | _, NULL -> t1
    | _ ->
        let k = min_elt t2 in
        bal t1 k (remove_min_elt t2)
        
(*  Funkcja usuwa ze zbioru elementy z zadanego przedziału *)
let remove (x, y) s = 
    let t1, _, _ = split x s and
    _, _, t2 = split y s in
    merge t1 t2

(*  Funkcja pomocnicza, znajduje przedział zawierający dany element *)
let rec find_interval x s = 
    match s with
    | NULL -> (min_int,max_int)
    | Node (l, (p, k), r, _, _) ->
        if x < p then find_interval x l else
        if x > k then find_interval x r else
        (p, k)
        
(*  Funkcja pomocnicza, wyznacza przedział który należy wstawić do drzewa *)
let to_insert (x, y) s = 
    if mem (x - 1) s && x > min_int then
        if mem (y + 1) s && y < max_int
        then let (a,_) = find_interval (x - 1) s and (_,d) = find_interval (y + 1) s in (a,d)
        else let (a,_) = find_interval (x - 1) s in (a,y)
    else
        if mem (y + 1) s && y < max_int then let (_,b) = find_interval (y + 1) s in (x,b) else (x,y)
        
(*  Funkcja dodająca przedział [x,y] do danego drzewa,
    która zachowuje rozłączność przedziałów w drzewie *)
let add (x, y) s = 
    let (a, b) = to_insert (x, y) s in
    add_one cmp (a, b) (remove (min a x, max b y) s)
    
