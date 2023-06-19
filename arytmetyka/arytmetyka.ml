(* Arytmetyka *)

(* dla a < b para (a, b) reprezentuje <a, b>, a para (b, a) reprezentuje (-inf, a> u <b, inf) *)
type wartosc = 
	{left : float; right : float};;

let wartosc_od_do x y =
	{left = x; right = y};;
	
let abs x =
	if x >= 0. then
		x
	else
		-.x;;
		
let max a b =
	if classify_float a = FP_nan && classify_float b = FP_nan then
		nan
	else if classify_float a = FP_nan then
		b
	else if classify_float b = FP_nan then
		a
	else if a > b then
		a
	else 
		b;;

let min a b =
	if classify_float a = FP_nan && classify_float b = FP_nan then
		nan
	else if classify_float a = FP_nan then
		b
	else if classify_float b = FP_nan then
		a
	else if a < b then
		a
	else 
		b;;

let mini a b c d = 
	let f = min c d 
	and e = min a b
	in min e f;; 
	
let maxi a b c d = 
	let f = max c d 
	and e = max a b
	in max e f;;

let wartosc_dokladnosc x p =
	wartosc_od_do (x -. (abs x) *. (p /. 100.)) (x +. (abs x) *. (p /. 100.));;
	
let wartosc_dokladna x = 
	wartosc_od_do (x) (x);;
	
let in_wartosc x y =
	if x.left <= x.right then
		x.left <= y && y <= x.right
	else
		y <= x.right || x.left <= y;;
		
let min_wartosc x =
	if classify_float x.left = FP_nan then
		nan
	else if x.left <= x.right then
		x.left
	else
		neg_infinity;;
		
let max_wartosc x =
	if classify_float x.left = FP_nan then
		nan
	else if x.left <= x.right then
		x.right
	else
		infinity;;
		
let sr_wartosc x =
	if x.left = neg_infinity && x.right = infinity then
		nan
	else if x.left = neg_infinity then
		neg_infinity
	else if x.right = infinity then
		infinity
	else if x.left <= x.right then
		(x.left +. x.right) /. 2.0
	else
		nan;;
		
(* dla a = (x, y) oraz b = (z, t) *)
let plus a b =
	if classify_float a.left = FP_nan || classify_float b.left = FP_nan then
		wartosc_od_do nan nan 
	(* przypadek <x, y> i <z, t> *)
	else if a.left <= a.right && b.left <= b.right then
		wartosc_od_do (a.left +. b.left) (a.right +. b.right)
	(* przypadek (-inf, x> u <y, inf) i <z, t> lub <x, y> i (-inf, z> u <t, inf)*)
	else if (a.left > a.right && b.left <= b.right) || (a.left <= a.right && b.left > b.right) then
	(* jezeli pokrywa cale R *)
		if (a.right +. b.right >= a.left +. b.left) then
			wartosc_od_do (neg_infinity) (infinity)
	(* w.p.p *)
		else
			wartosc_od_do (a.left +. b.left) (a.right +. b.right)
	(* przypadek (-inf, x> u <y, inf) i (-inf, z> u <t, inf)*)
	else
		wartosc_od_do (neg_infinity) (infinity);;
		
let el_przeciwny x =
	wartosc_od_do (-.x.right) (-.x.left);;

(* odejmowanie poprzez dodawanie elementu odwrotnego do przedzialu s *)
let minus a b =
	plus a (el_przeciwny b);;
	
let rec razy a b =
	if classify_float a.left = FP_nan || classify_float b.left = FP_nan then
		wartosc_od_do nan nan
	else if (a.left = 0. && a.right = 0.) || (b.left = 0. && b.right = 0.) then
		wartosc_od_do 0. 0.
	(* przypadek <x, y> i <z, t> *)
	else if a.left <= a.right && b.left <= b.right then
	(* wynikiem bedzie <p, q> wiec minimalizuje p i maksymalizuje q *)
		wartosc_od_do (mini (a.left *. b.left) (a.left *. b.right) (a.right *. b.left) (a.right *. b.right))
		(maxi (a.left *. b.left) (a.left *. b.right) (a.right *. b.left) (a.right *. b.right))
	(* przypadek <x, y> i (-inf, z> u <t, inf)*)
	else if a.left <= a.right && b.left > b.right then 
		(* przypadek dla <x, y> jest nieujemny *)
		if a.left >= 0. then
			(* przypadek <x, y> i (-inf, (0 <= z)> u <(0 < t), inf) *)
			if b.right >= 0. then
				if (b.right = 0. && a.right = infinity) then
					wartosc_od_do (b.left *. a.left) (0.)
				else if ((b.left *. a.left) > (b.right *. a.right)) then
					wartosc_od_do (b.left *. a.left) (b.right *. a.right)
				else
					wartosc_od_do (neg_infinity) (infinity)
			(* przypadek <x, y> i (-inf, (0 > z)> u <(0 < t), inf) *)
			else if b.right < 0. && b.left > 0. then
				if (b.left *. a.left) > (b.right *. a.left) then
					wartosc_od_do (b.left *. a.left) (b.right *. a.left)
				else
					wartosc_od_do (neg_infinity) (infinity)
			(* przypadek <x, y> i (-inf, (0 > z)> u <(0 >= t), inf) *)
			else
				if (b.left = 0. && a.right = infinity) then
					wartosc_od_do (0.) (b.right *. a.left)
				else if ((b.left *. a.right) > (b.right *. a.left)) then
					wartosc_od_do (b.left *. a.right) (b.right *. a.left)
				else
					wartosc_od_do (neg_infinity) (infinity)
		(* przypadek dla <x, y> jest niedodatni *)
		else if a.right <= 0. then
			(* przypadek <x, y> i (-inf, (0 <= z)> u <(0 < t), inf) *)
			if b.right >= 0. then
				if (b.right = 0. && a.left = neg_infinity) then
					wartosc_od_do (0.) (b.left *. a.right)
				else if ((b.right *. a.left) > (b.left *. a.right)) then
					wartosc_od_do (b.right *. a.left) (b.left *. a.right)
				else
					wartosc_od_do (neg_infinity) (infinity)
			(* przypadek <x, y> i (-inf, (0 > z)> u <(0 < t), inf) *)
			else if b.right < 0. && b.left > 0. then
				if (b.right *. a.right) > (b.left *. a.right) then
					wartosc_od_do (b.right *. a.right) (b.left *. a.right)
				else
					wartosc_od_do (neg_infinity) (infinity)
			(* przypadek <x, y> i (-inf, (0 > z)> u <(0 >= t), inf) *)
			else
				if (b.left = 0. && a.left = neg_infinity) then
					wartosc_od_do (b.right *. a.right) (0.)
				else if ((b.right *. a.right) > (b.left *. a.left)) then
					wartosc_od_do (b.right *. a.right) (b.left *. a.left)
				else
					wartosc_od_do (neg_infinity) (infinity)
		(* przypadek dla <(x < 0), (y > 0)> *)
		else
			wartosc_od_do (neg_infinity) (infinity)
	(* przypadek <x, y> i (-inf, z> u <t, inf)*)
	else if a.left > a.right && b.left <= b.right then
		razy b a
	(* przypadek (-inf, (0 > x)> u < (0 < t), inf) i (-inf, (0 > z)> u <(0 < t), inf)*)
	else if a.left > 0. && a.right < 0. && b.left > 0. && b.right < 0. then
		wartosc_od_do (min (a.right *. b.right) (a.left *. b.left)) (max (a.right *. b.left) (a.left *. b.right))
	(* w.p.p dla (-inf, x> u <y, inf) i (-inf, z> u <t, inf) *)
	else
		wartosc_od_do (neg_infinity) (infinity);;

let el_odwrotny x = 
	if x.left = 0. && x.right = 0. then
		wartosc_od_do (nan) (nan)
	else if x.left = neg_infinity && x.right = infinity then
		wartosc_od_do (neg_infinity) (infinity)
	else if x.right = 0. then
		wartosc_od_do neg_infinity (1. /. x.left)
	else if x.left = 0. then
		wartosc_od_do (1. /. x.right) infinity
	else
		wartosc_od_do (1. /. x.right) (1. /. x.left);;

(* dzielenie to mnozenie przez element odwrotny *)	
let podzielic a b =
	razy a (el_odwrotny b);;
		
		
		
	
		
			
	
	
