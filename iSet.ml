(*Autor:          Wojciech Paupa          *)
(*Code review:    Jakub Zarzycki          *)


(*definicja z pseta dostosowana na potrzeby zadania*)
(*ostatnia liczba to liczba elementow w drzewie*)
type t =
  | Empty
  | Node of  t * (int*int) * t * int * int

(*plus dodaje liczby. stara sie nie wywalac przy max_intach.
podobnie minus*)
(*funkcja plus wywolywana jest tylko, gdy co najmniej jedna
z dodawanych liczb jest wieksza od zera*)
(*funkcja minus wywolywana jest tylko z a>b*)
let plus a b =
  if ((b>0) && (a+b<a)) || ((a>0) && (a+b<b)) then max_int else a+b
let minus a b =
  if a-b<0 then max_int else a-b

(*na wszelki*)
let odejmij_jeden x = if x>min_int then x-1 else min_int;;
let dodaj_jeden x = if x<max_int then x+1 else max_int

(*komparator mowi, ze przedzialy sa rowne, kiedy nie sa rozlaczne;
  w przeciwnym wypadku je porownoje standardowo*)
let cmp (a,b) (c,d) = if b<(odejmij_jeden c) then -1 else if a>(dodaj_jeden d) then 1 else 0 

(*gotowe - przepisane z pseta*)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(*zwraca liczbe elementow w drzewie*)
let elementy = function
  | Node (_, _, _, _, n) -> n
  | Empty -> 0

(*gotowe - przepisane z pseta*)
let make l (ka,kb) r = Node (l, (ka,kb), r, max (height l) (height r) + 1, plus (plus (elementy r)  (elementy l))  (dodaj_jeden (minus kb ka)))

(*gotowe - przepisane z pseta*)
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
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1, plus (plus (elementy r)  (elementy l))  (dodaj_jeden (minus (snd k) (fst k))))

(*gotowe - przepisane z pseta*)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(*gotowe - przepisane z pseta*)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt"

(*gotowe - przepisane z pseta*)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

(*gotowe - przepisane z pseta*)
let empty = Empty

(*gotowe - przepisane z pseta*)
let is_empty x = (x = Empty)

(*gotowe - przepisane z pseta z drobna modyfikacja*)
(*dziala tylko dla rozlacznych i niesasiadujacych
przedzialow*)
(*po tej operacji drzewo jest zbalansowane na mocy indukcji,
tak samo jak przy psecie. jesli c=0, nie zmieniamy balansu drzewa*)
let rec add_pset x = function
  | Node (l, k, r, h, _) ->
      let c = cmp x k in
      if c = 0 then 
        let ka = min (fst x) (fst k)
        and kb = max (snd x) (snd k) in
        Node (l, (ka, kb), r, h, plus (plus (elementy r)  (elementy l))  (dodaj_jeden (minus kb ka)))
      else if c < 0 then
        let nl = add_pset x l in
        bal nl k r
      else
        let nr = add_pset x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, dodaj_jeden (minus (snd x) (fst x)))


(*gotowe - przepisane z pseta*)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_pset v r
  | (_, Empty) -> add_pset v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(*zlacza rozbalansowane drzewa*)
(*dziala tak samo jak merge,
ale nie zaklada, ze drzewa roznia sie
co do wysokosci o max 2*)
let kmerge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      join t1 k (remove_min_elt t2)

(*dziala tylko wtedy, gdy x jest
rozlaczne z kazdym z przedzialow
z seta, do ktorego dodajemy.*)
(*ta funkcja dziala jak add_pset,
ale laczy dwa przedzialy,
gdy sasiaduja ze soba i wywoluje znowu
add_pset, zeby to dodal*)
(*drzewo zbalansowane po jej wykonaniu
z tego samego powodu, co w add_pset*)
let rec add_one x = function
  | Node (l, k, r, h, _) ->
      let c = cmp x k in
      if c = 0 then 
      	if (snd x) + 1 = fst k then
      	kmerge (add_pset (min (fst x) (fst k), max (snd x) (snd k)) l) r
        else if (fst x) - 1 = snd k then
        kmerge l (add_pset (min (fst x) (fst k), max (snd x) (snd k)) r) 
        else Node (l, x, r, h, plus (plus (elementy r)  (elementy l))  (plus (minus (snd k) (fst k)) 1))
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, dodaj_jeden (minus (snd x) (fst x)))




(*sprawdza, czy a zawiera sie w przedziale, czy jest mniejsze,
czy wieksze.*)
let comp a (f,s) = if (a<f) then -1 else if (s<a) then 1 else 0

(*balansuje drzewo*)
let balans = function
  | Empty -> Empty
  | Node(l,k,r,_,_) -> join l k r

(*przepisane z pseta z mala modyfikacja:
jak znajdziemy x, to przedzial, w ktorym byl,
rozdzielamy na mniejsze i dodajemy do pozostalego
drzewa*)
(*drzewo jest zbalansowane po wykonaniu operacji,
bo wywolujemy na koniec balans*)
let split x set =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
        let c = comp x v in
        if c = 0 then 
        	let p1 = if (fst v)< x then add_one (fst v,(odejmij_jeden x)) l else l  
        	and p2 = if x<(snd v) then add_one (dodaj_jeden x,snd v) r else r in
        	(p1, true, p2)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in let (l,k,r) = loop x set
  in (balans l, k, balans r)

(*pomocnicze procedury*)
let first  (a,b,c) = a
let second (a,b,c) = b
let third  (a,b,c) = c

(*bierzemy wszystkie elementy mniejsze od a, wszystkie wieksze od b
i je dodajemy do siebie. mergowane drzewa beda rozlaczne.*)
let remove (a,b) set = kmerge (first (split a set)) (third (split b set))

(*tak zwane "niech ktos inny sie tym martwi"*)
let add x set  = add_one x (remove x set)

(*gotowe - przepisane z pseta z drobna modyfikacja*)
let mem x set =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = comp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop set

(*gotowe - przepisane z pseta*)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set

(*gotowe - przepisane z pseta*)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc set

(*gotowe - przepisane z pseta*)
let elements set = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set

(*jesli ten element wystepuje
w secie, trzeba go dodac*)
let below n set = 
  let s = split n set in
  if (second s)
    then dodaj_jeden (elementy (first s))
  else elementy (first s)









(* Copyright Artur "mrowqa" Jamro 2015 *)
let zle = ref 0
let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Zly wynik testu %d!\n" id;
        incr zle
    end;;





let s = empty;;
test 11 (is_empty s) true;;
test 12 (is_empty (add (1, 1) s)) false;;


(* niestety musimy zalozyc poprawnosc mem... *)

let s = add (10, 12) empty;;
test 21 (mem 9 s) false;;
test 22 (mem 10 s) true;;
test 23 (mem 12 s) true;;
test 24 (mem 13 s) false;;

let s = add (4, 7) s;;
test 25 (mem 8 s) false;;
test 26 (mem 11 s) true;;
test 27 (mem 5 s) true;;
test 28 (mem 3 s) false;;


let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
test 31 (mem 10 (remove (10, 10) s)) false;;
test 32 (is_empty (remove (1, 20) s)) true;;
test 33 (mem 7 (remove (8, 15) s)) true;;

let s = add (-1, 1) (add (3, 7) (add (10, 12) (add (15, 18)
        (add (-15, -13) empty))));;
let s = remove (-10, 12) s;;
test 34 (is_empty s) false;;
test 35 (mem 5 s) false;;
test 36 (mem (-10) s) false;;
test 37 (mem (-15) s) true;;
test 38 (mem 17 s) true;;


test 41 (elements (add (4, 5) (add (7, 8) empty)) = [(4, 5); (7, 8)]) true;;
test 42 (elements (add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty))))
        = [(1, 1); (4, 9); (11, 14)]) true;;


let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
test 51 (below 2 s = 0) true;;
test 52 (below 3 s = 1) true;;
test 53 (below 10 s = 5) true;;
test 54 (below 15 s = 6) true;;
test 55 (below 100 s = 11) true;;
let s = add (1, max_int) (add (-1, 0) empty);;
test 56 (below max_int s = max_int) true;;
let s = add (-min_int, max_int) empty;;
test 57 (below max_int s = max_int) true;;
test 58 (below min_int s = 1) true;;


let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
let l, pres, r = split 9 s;;
test 61 (mem 9 l) false;;
test 62 (mem 9 r) false;;
test 63 (mem 8 l) true;;
test 64 (mem 10 r) true;;
test 65 pres true;;
test 66 (mem 7 l) false;;
test 67 (mem 4 l) true;;
test 68 (mem 11 r) false;;
test 69 (mem 16 r) true;;


let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let a = ref [];;
let foo x = a := x::!a; ();;
test 71 (iter foo s; !a = [(11, 14); (4, 9); (1, 1)]) true;;


let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let foo x a = x::a;;
test 81 (fold foo s [] = [(11, 14); (4, 9); (1, 1)]) true;;


let _ =
    if !zle = 0 then
        ()
    else
        Printf.printf "\nZlych odpowiedzi: %d.\n" !zle
;;





let a = add (0, 5) empty;;
let a = add (7, 8) a;;
let a = add (-3, -3) a;;
let a = add (10, 13) a;;
assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);;
assert(below 8 a = 9);;
let b = add (6, 6) a;;
let b = remove (6, 6) b;;
let b = add (-100, -5) b;;
let b = add (-4, 6) b;;
assert(elements b = [(-100, 8); (10, 13)]);;
assert(below 10 b = 110);;
let c = remove (2, 10) a;;
assert(elements c = [(-3, -3); (0, 1); (11, 13)]);;
assert(below 12 c = 5);;

let good = ref 0 and bad = ref 0

let check nr warunek wartosc =
  if warunek = wartosc then
    begin
      (* Printf.printf "OK - TEST nr %d \n" nr; *)
      incr good
    end
  else
    begin
      Printf.printf "Fail: %d\n" nr;
      assert (false);
    end;;



let liczba a = List.length (elements a)

(* Testy na add i remove *)

let a = empty
let a = add (17, 20) a
let a = add (5, 8) a
let a = add (1, 2) a
let a = add (10, 12) a
let a = add (28, 35) a
let a = add (22, 23) a
let a = add (40, 43) a
let a = add (37, 37) a;;

check 1 (is_empty a) false;;
check 2 (mem 29 a) true;;
check 3 (mem 21 a) false;;
check 4 (mem 38 a) false;;
check 5 (mem 37 a) true;;
check 6 (below 8 a = below 9 a) true;;
check 7 (below 29 a) 17;;
check 8 (liczba a) 8;;

let a = add (37, 42) a;;

check 9 (liczba a) 7;;
check 10 (mem 37 a) true;;
check 11 (mem 38 a) true;;
check 12 (mem 39 a) true;;
check 13 (mem 40 a) true;;
check 14 (mem 41 a) true;;
check 15 (mem 42 a) true;;
check 16 (mem 44 a) false;;
check 17 (below 38 a = below 39 a) false;;

let tmp = remove (8, 22) a;;
let tmp = add (8, 22) tmp;;

check 18 (elements tmp = elements a) false;;

(* Testy na split *)

let (l, exists, p) = split 9 a;;

check 19 exists false;;
check 20 (liczba l) 2;;
check 21 (liczba p) 5;;
check 22 (mem 10 l) false;;
check 23 (mem 9 l) false;;
check 24 (mem 8 l) true;;
check 25 (mem 1 l) true;;
check 26 (mem 9 p) false;;
check 27 (mem 10 p) true;;
check 28 (mem 17 p) true;;
check 29 (mem 29 p) true;;
check 30 (mem 24 p) false;;
check 31 (mem 38 p) true;;
check 32 ((elements l @ elements p) = elements a) true;;

let (l, exists, p) = split 21 a;;

check 33 exists false;;
check 34 ((elements l @ elements p) = elements a) true;;

let (l, exists, p) = split 15 a;;
check 35 exists false;;
check 36 ((elements l @ elements p) = elements a) true;;


let b = empty
let b = add (5, 10) b
let b = add (40, 50) b
let b = add (20, 25) b
let b = add (12, 14) b
let b = add (17, 18) b
let b = add (52, 60) b
let b = add (62, 80) b
let b = add (83, 100) b;;

check 37 (mem 41 b) true;;
check 38 (mem 11 b) false;;

let d = empty;;
let (l, ex, p) = split 0 d;;

check 39 (is_empty l) true;;
check 40 (is_empty p) true;;

let d = add (17, 30) d;;
let d = add (1, 3) d;;
let d = add (10, 10) d;;
let d = remove (11, 11) d;;
let d = add (12, 14) d;;
let d = add (32, 35) d;;
let d = add (38, 40) d;;

check 41 (below 36 d = below 37 d) true;;

let d = add (36, 37) d;;

check 42 (below 36 d = below 37 d) false;;

let d = remove (37, 37) d;;
check 43 (below 36 d = below 37 d) true;;

let d = remove (20, 21) d;;

check 44 (elements d) [(1, 3); (10, 10); (12, 14); (17, 19); (22, 30); (32, 36); (38, 40)];;

let (l, ex, p) = split 15 d;;
check 144 (elements l) [(1, 3); (10, 10); (12, 14)];;
check 145 (elements p) [(17, 19); (22, 30); (32, 36); (38, 40)];;

check 45 ((elements l @ elements p) = elements d) true;;
check 46 (liczba l, liczba p) (3, 4);;

check 47 (mem 13 l) true;;
check 48 (mem 14 l) true;;
check 49 ex false;;

let (l, ex, p) = split 25 d;;

check 50 ex true;;
check 51 (elements l) [(1, 3); (10, 10); (12, 14); (17, 19); (22, 24)];;
check 52 (elements p) [(26, 30); (32, 36); (38, 40)];;




(* Tests to iSet.ml *)

let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    assert (false);
  end



let a = empty;;
let a = add (2, 5) a;;
let a = add (7, 10) a;;
let a = add (12, 20) a;;
let a = add (0, 0) a;;

test 1 (mem 1 a = false);;
test 2 (mem 2 a = true);;
test 3 (mem 9 a = true);;
test 4 (mem 21 a = false);;

let elem = elements a;;

test 5 (elem = [(0,0);(2,5);(7,10);(12,20)]);;
test 6 (below 6 a == 5);;
test 7 (below 10 a == 9);;
test 8 (below 19 a == 17);;

let (l,_,r) = split 15 a;;

test 9 (elements l = [(0,0);(2,5);(7,10);(12,14)]);;
test 10 (elements r = [(16,20)]);;

let (l,_,r) = split 8 a;;

test 11 (elements l = [(0,0);(2,5);(7,7);]);;
test 12 (elements r = [(9,10);(12,20)]);;


let a = add (6, 6) a;;
let b = add (11, 11) a;;

test 13 (elements a = [(0,0);(2,10);(12,20)]);;
test 14 (elements b = [(0,0);(2,20)]);;

let b = empty;;
let b = add (-10, 5) b;;
let b = add (10, 34) b;;
test 15 (elements b  = [(-10,5);(10,34)]);;

let b = add (22, 40) b;;
test 16 (elements b  = [(-10, 5);(10, 40)]);;

let b = add (41, 45) b;;
test 17 (elements b  = [(-10, 5);(10, 45)]);;

let b = add (80, 102) b;;
let b = add (130, 220) b;;
test 18 (elements b  = [(-10, 5);(10, 45);(80,102);(130,220)]);;

let b = add (45, 140) b;;
test 19 (elements b  = [(-10, 5);(10, 220)]);;


let c = empty;;
let c = add (4, max_int) c;;
let c = add (min_int, 0) c;;

test 20 (mem 4 c = true);;
test 21 (mem 0 c = true);;
test 22 (mem 20 c = true);;
test 23 (mem 2 c = false);;
test 24 (elements c = [(min_int, 0);(4, max_int)]);;
test 25 (below 0 c = max_int);;
test 26 (below max_int c = max_int);;


let d = empty;;
let d = add (min_int, max_int) d;;

test 27 (below 0 c = max_int);;
test 28 (below (-2) c = max_int);;
test 29 (below min_int c = 1);;
test 30 (below (min_int+1) c = 2);;