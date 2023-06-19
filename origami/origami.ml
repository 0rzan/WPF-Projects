(*code review: Maciej Bielik*)

open List
type point = float * float
type prosta = point * point * bool

let epsilon = 0.00000001

(*licze rownanie prestej przechodzacej przez dwa punkty*)
let rownanie_prostej (x1, y1) (x2, y2) =
	if x1 = x2 then (x1, x1, true) else
	((y1 -. y2) /. (x1 -. x2), (y1 -. ((y1 -. y2) /. (x1 -. x2)) *. x1), false)
	
let wektor (x1, y1) (x2, y2) = (x2 -. x1, y2 -. y1)
		
let iloczynw (x1, y1) (x2, y2) = (x1 *. y2) -. (y1 *. x2)

(* licze po ktorej stronie wektora p1 p2 znajduje jest p3*)
let strona p1 p2 p3 =
	let pom = iloczynw (wektor p1 p2) (wektor p1 p3) in
	if (-1.) *. epsilon < pom && pom < epsilon then 0
	else if pom < 0. then -1
	else 1

(*licze wspolrzedne punktu po odbiciu wzgledem prostej y=ax+b*)
let odbicie (xp, yp) (a, b, p) =
	if p = true then
		(xp +. 2. *. (a -. xp), yp) else 
	if (-1.) *. epsilon < a && a < epsilon then
		(xp, yp +. 2. *. (b -. yp)) else
	let bpom = yp +. (xp /. a) in	
	let xpom = (bpom -. b) /. (a +. (1. /. a)) in
	let ypom = a *. xpom +. b in
	(2. *. xpom -. xp, 2. *. ypom -. yp) 
	
type kartka = point -> int

let czy_nalezy_prostokat (x1, y1) (x2, y2) (x, y) =	
	if x1 -. epsilon <= x && y1 -. epsilon <= y && x2 +. epsilon >= x && y2 +. epsilon >= y then true else false

let czy_nalezy_kolko (x1, y1) r (x, y) =
	if (x -. x1) *. (x -. x1) +. (y -. y1) *. (y -. y1) <= (r +. epsilon) *. (r +. epsilon) then true else false
	
let prostokat (p1:point) (p2:point) : kartka =
	function (p:point) -> if czy_nalezy_prostokat p1 p2 p then 1 else 0

let kolko (p1:point) (r:float) : kartka =
	function (p:point) -> if czy_nalezy_kolko p1 r p then 1 else 0

(*zwracam 0, wartosc punktu w kartce lub wartosc punktu w kartce + wartosc punktu po odbiciu w kartce na podstawie
tego odpowiednio, czy po zagieciu punkt nalezy do kartki, nalezy do krawedzi zagiecia lub nalezy do kartki po zagieciu*)
let zloz (p1:point) (p2:point) (k:kartka) : kartka =
	function (p:point) ->
		let l = rownanie_prostej p1 p2 in
		match strona p1 p2 p with
		-1 -> 0 |
		0 -> (k p) |
		1 -> (k p) + (k (odbicie p l))

(*skladaj to zlozenie funkcji zloz*)		
let skladaj (lista:((point * point) list)) (k:kartka) =
	fold_left (fun a (p1, p2) -> zloz p1 p2 a) k lista









