(* code review: Jan Wangrat *)
(* deklaruje kolejke (lewy syn, prawy syn, wartosc, wysokosc) *)
type 'a queue =
	| Koniec 
	| Node of 'a queue * 'a queue * 'a * int

exception Empty

let empty = 
	Koniec
	
(* wysokosc wierzcholka *)	
let wys q = 
	match q with
	| Koniec -> 0
	| Node (_, _, _, h) -> h

(* obliczam wysokosc wierzcholka na podstawie synow i zwracam kolejke z ustawieniem lewicowym synow*)
let check q =
	match q with
	| Koniec -> Koniec
	| Node (lewy, prawy, v, h) ->
		let wl = wys lewy in
		let wp = wys prawy in
		if wl > wp then
		Node (lewy, prawy, v, wl+1)
		else
		Node (prawy, lewy, v, wp+1)

(* rekurencyjnie lacze q1 i q2 i w kolejnych przejsciach sprawdzam lewicowosc *)		
let rec join q1 q2 =
	match q1, q2 with
	| Koniec, q -> q
	| q, Koniec -> q
	| Node (l1, p1, w1, h1), Node (l2, p2, w2, h2) ->
		if w1 > w2 then
			join q2 q1
		else
			check (Node (l1, (join p1 q2), w1, h1))

let add e q =
	join (Node(Koniec, Koniec, e, 1)) q

let delete_min q =
	match q with
	| Koniec -> raise Empty
	| Node (prawy, lewy, v, h) ->
		(v, (join prawy lewy))

let is_empty q =
	match q with
	| Koniec -> true
	| Node(_, _, _, _) -> false
