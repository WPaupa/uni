(*definicja z pseta dostosowana na potrzeby zadania*)
type t =
  | Empty
  | Node of  t * (int*int) * t * int

(*komparator mowi, ze przedzialy sa rowne, kiedy nie sa rozlaczne;
  w przeciwnym wypadku je porownoje standardowo*)
let cmp (a,b) (c,d) = if b<c-1 then -1 else if a>d+1 then 1 else 0 

(*gotowe - przepisane z pseta*)
let height = function
  | Node (_, _, _, h) -> h
  | Empty -> 0

(*gotowe - przepisane z pseta*)
let make l k r = Node (l, k, r, max (height l) (height r) + 1)

(*gotowe - przepisane z pseta*)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1)

(*gotowe - przepisane z pseta*)
let rec min_elt = function
  | Node (Empty, k, _, _) -> k
  | Node (l, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(*gotowe - przepisane z pseta*)
let rec remove_min_elt = function
  | Node (Empty, _, r, _) -> r
  | Node (l, k, r, _) -> bal (remove_min_elt l) k r
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
let is_empty x = Empty

(*gotowe - przepisane z pseta z drobna modyfikacja*)
(*dziala tylko dla rozlacznych i niesasiadujacych
przedzialow*)
let rec add_pset x = function
  | Node (l, k, r, h) ->
      let c = cmp x k in
      if c = 0 then Node (l, (min (fst x) (fst k), max (snd x) (snd k)), r, h)
      else if c < 0 then
        let nl = add_pset x l in
        bal nl k r
      else
        let nr = add_pset x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1)


(*dziala tylko wtedy, gdy x jest
rozlaczne z kazdym z przedzialow
z seta, do ktorego dodajemy.*)
(*ta funkcja dziala jak add_pset,
ale laczy dwa przedzialy,
gdy sasiaduja ze soba i wywoluje znowu
add_pset, zeby to dodal*)
let rec add_one x = function
  | Node (l, k, r, h) ->
      let c = cmp x k in
      if c = 0 then 
      	if (snd x) + 1 = fst k then
      	merge (add_pset (min (fst x) (fst k), max (snd x) (snd k)) l) r
        else if (fst x) - 1 = snd k then
        merge l (add_pset (min (fst x) (fst k), max (snd x) (snd k)) r) 
        else Node (l, x, r, h)
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1)


(*gotowe - przepisane z pseta*)
let rec join l v r =
  match (l, r) with
  |  (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(*sprawdza, czy a zawiera sie w przedziale, czy jest mniejsze,
czy wieksze.*)
let comp a (f,s) = if (a<f) then -1 else if (s<a) then 1 else 0

(*przepisane z pseta z mala modyfikacja:
jak znajdziemy x, to przedzial, w ktorym byl,
rozdzielamy na mniejsze i dodajemy do pozostalego
drzewa*)
let split x set =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _) ->
        let c = comp x v in
        if c = 0 then 
        	let p1 = if (fst v<=x-1) then add_one (fst v,x-1) l else l  
        	and p2 = if (x+1<=snd v) then add_one (x+1,snd v) r else r in
        	(p1, true, p2)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in loop x set

(*pomocnicze procedury*)
let first  (a,b,c) = a
let second (a,b,c) = b
let third  (a,b,c) = c

(*bierzemy wszystkie elementy mniejsze od a, wszystkie wieksze od b
i je dodajemy do siebie. mergowane drzewa beda rozlaczne.*)
let remove (a,b) set = merge (first (split a set)) (third (split b set))

(*tak zwane "niech ktos inny sie tym martwi"*)
let add x set  = add_one x (remove x set)

(*gotowe - przepisane z pseta z drobna modyfikacja*)
let mem x set =
  let rec loop = function
    | Node (l, k, r, _) ->
        let c = comp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop set

(*gotowe - przepisane z pseta*)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _) -> loop l; f k; loop r in
  loop set

(*gotowe - przepisane z pseta*)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _) ->
          loop (f k (loop acc l)) r in
  loop acc set

(*gotowe - przepisane z pseta*)
let elements set = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _) -> loop (k :: loop acc r) l in
  loop [] set

(*safeplus dodaje liczbe elementow w przedziale (c,d) do liczby a.
stara sie nie wywalac przy max_intach.*)
let safeplus a (c,d) =
	let b = if d-c<0 then max_int else d-c+1 in
	if ((a=max_int) || b=max_int) || (a+b<0) then max_int else a+b

let below n set = 
	let k = split n set in
	if (second k) then List.fold_left safeplus 1 (elements (first k))
	else List.fold_left safeplus 0 (elements (first k)) 