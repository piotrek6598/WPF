(*  Autor: Piotr Jasinski                                                    
    Code review: Pawel Pawlik *)

open List

(*  Punkt na plaszczyznie *)
type point = float * float

(*  Poskładana kartka, funkcja zwraca liczbę warstw w danym punkcie *)
type kartka = point -> int

(* Stała służąca do sprawdzania równości float                               
    a = b <=> a - eps < b < a + eps *)
let eps = 0.0000000001

(*  Podnoszenie liczby do kwadratu *)
let do_kwadratu = fun a -> a *. a

(*  Funkcja sprawdza czy punkt znajduje się wewnątrz prostokąta *)
let cmp_prostokat (x1, y1) (x2, y2) (x3, y3) =
    x3 >= x1 -. eps && x3 <= x2 +. eps && y3 >= y1 -. eps && y3 <= y2 +. eps
    
(*  Funkcja tworząca kartkę w kształcie prostokąta *)                           
let prostokat p1 p2 = function p3 -> if cmp_prostokat p1 p2 p3 then 1 else 0

(*  Funkcja tworząca kartkę w kształcie kółka *)
let kolko (x1, y1) r = function (x2, y2) -> 
    if do_kwadratu (x2 -. x1) +. do_kwadratu (y2 -. y1) < do_kwadratu r +. eps
    then 1 else 0

(*  Funkcja wyznacza współczynniki równania prostej 
    (Ax + By + C = 0) przechodzącej przez dane punkty *)
let rownanie (x1, y1) (x2, y2) =
    y1 -. y2, x2 -. x1, x1 *. y2 -. x2 *. y1

(*  Funkcja zwraca wartość dodatnią gdy punkt jest po lewej stronie 
    prostej przechodzacej przez p1, p2, 0 gdy na niej lezy, 
    wartość ujemną, gdy punkt po prawej stronie *)
let strona p1 p2 (x, y) = 
    let a, b, c = rownanie p1 p2 in
    a *. x +. b *. y +. c

(*  Funkcja zwraca punkt symetryczny, względem prostej
    przechodzącej przez p1, p2 *)
let odbicie p1 p2 (x, y) =
    let a, b, c = rownanie p1 p2 in
    if b > -. eps && b < eps then (x +. 2. *. (-.c /. a -. x), y) else
    let a = (-1.) *. a /. b and c = (-1.) *. c /. b in
    let li = 1. -. do_kwadratu a and mian = 1. +. do_kwadratu a in
    (li *. x +. 2. *. a *. (y -. c)) /. mian,
    (2. *. a *. x -. li *. (y -. c)) /. mian +. c                               

(*  Funkcja zwraca poskładaną kartkę wzdłuż prostej wyznaczonej
    przez dane punkty *)
let zloz p1 p2 k = 
    let k1 = function p3 ->
        let a = strona p1 p2 p3 in
        if a < 0. -. eps then 0 else
        if a > 0. +. eps then k p3 + k (odbicie p1 p2 p3) else k p3
        in k1

(*  Funckja zwraca wielokrotnie poskładaną kartkę *)
let skladaj l k = 
    let l1,l2 = split l in fold_right2 zloz (rev l1) (rev l2) k 
