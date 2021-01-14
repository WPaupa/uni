(*********************************************)
(*AUTOR KODU: Wojciech Paupa                 *)
(*CODE REVIEW: Maria Nazarczuk               *)
(*ZŁOŻONOŚĆ OBLICZENIOWA:                    *)
(*złożoność dfs * złożoność operacji na mapie*)
(*            O((v+e)log e)                  *)
(*ZŁOŻONOŚĆ PAMIĘCIOWA:                      *)
(*           O(e)                            *)
(*********************************************)
open PMap;;

exception Cykliczne;;

(*wersja PMap.find, ktora nie wywala wyjatkow*)
let find_def key map default = try find key map with Not_found -> default

let topol g =
  (*konstrukcja mapy sasiedztwa*)
  let m = 
	  List.fold_left 
	  (fun m (k,v) -> 
	  	(*jesli dodalismy juz jakichs
	  	sasiadow do danego wierzcholka,
	    doklejamy kolejnych*)
	  	add k (v@(find_def k m [])) m )
	  empty g in
  (*mapa odw trzyma wszystkie wierzcholki
  calkowicie rozwazone. tak naprawde to set
  zaimplementowany za pomoca mapy unitow*)
  let odw = ref empty
  (*lista !wyn trzyma rozwazone
  wierzcholki posortowane topologicznie*)
  and wyn = ref ([]) in
  (*procedura visit jest dfsem.
  mapa tempodw trzyma wszystkie
  wierzcholki, w ktorych bylo
  juz obecne przejscie dfsa
  (dfs bedzie wywolywany
  kilka osobnych razy).*)
  let rec visit n tempodw = 
      (*jesli jestesmy w juz rozwazonym
      wierzcholku, to z niego wychodzimy
      (czyli zwracamy unit) *)
    if mem n !odw then ()
    else
        (*jesli bylismy juz w tym wierzcholku
        przy tym przejsciu dfsem, to istnieje
        sciezka z wierzcholka do samego siebie 
        i graf jest cykliczny.*)
      if mem n tempodw then raise Cykliczne
      else
      begin
        (*przechodzimy po wszystkich sasiadach
        danego wierzcholka rekurencyjnie, po
        drodze dodajemy wierzcholek do mapy 
        tempodw (wierzcholkow rozwazanych w tym przejsciu)*)
		List.iter (fun x -> visit x (add n () tempodw)) (find_def n m []);
        (*dodajemy wierzcholek do rozwazonych*)
        odw := add n () !odw;
        (*skoro bylismy juz we wszystkich wierzcholkach,
        do ktorych da sie dojsc z tego, to wszystkie je juz wrzucilismy
        na liste wyn, czyli mozemy wrzucic tez ten.*)
        wyn := n::(!wyn) 
      end in
  (*po kolei wywolujemy dfsa w kazdym nierozwazonym wierzcholku*)
  iter (fun k v -> if mem k (!odw) then () else visit k empty) m;
  !wyn;;


(*TESTY*)
exception WA;;

let debug = false;;

(* True if the given order is a correct topological order, *)
(* false otherwise. Checks all edges.                      *)
let test graph order =
  let hashtbl = Hashtbl.create (List.length order)
  in
  List.iteri (fun i x -> Hashtbl.add hashtbl x i) order;
  let check_one (v, l) =
    List.iter (fun u ->
      if (Hashtbl.find hashtbl v) > (Hashtbl.find hashtbl u)
      then raise WA;) l
  in
  try (List.iter check_one graph; true)
  with WA -> false

(* Tests if Topol raises Cykliczne for the given graph *)
let test_cyclic g =
  try let _ = topol g in false
  with Cykliczne -> true

;;
      
if debug then print_endline "Acyclic correctness tests...";;
      
let g = [
  ("1", ["2"; "3"]);
  ("3", ["2"]);
  ("4", ["3"; "2"])
];;

assert(test g (topol g));;

let g = [
  ("first", ["second"; "fourth"; "eighth"]);
  ("second", ["fourth"; "eighth"]);
  ("third", ["fourth"; "fifth"; "sixth"]);
  ("fourth", ["eighth"]);
  ("fifth", ["sixth"; "seventh"]);
  ("sixth", ["eighth"; "first"]);
  ("seventh", ["eighth"]);
];;

assert(test g (topol g));;

let g = [
  (1, [2; 3]);
  (2, [4]);
  (3, [4]);
  (4, [5; 6]);
  (5, [7]);
  (6, [7]);
];;

assert(test g (topol g));;

let g = [
  (1, [7; 2]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;

assert(test g (topol g));;

let g = [
  (1, [2; 4; 8]);
  (2, [16; 32]);
  (4, [64; 128]);
  (8, [256; 512]);
  (16, [1024]);
  (32, [2048]);
  (64, [4096]);
  (128, [8192]);
  (256, [16384]);
  (512, [32768]);
];;

assert(test g (topol g));;

let g = [
  ("Lorem", ["sit"]);
  ("ipsum", ["sit"; "amet"]);
  ("dolor", ["amet"; "elit"]);
  ("sit", ["consectetur"; "adipiscing"; "elit"]);
];;

assert(test g (topol g));;

let g = [];;

assert(test g (topol g));;

let g = [
  ("through", ["the"; "gates"; "of"; "hell"]);
  ("hell", ["as"; "we"; "make"; "our"; "way"; "to"; "heaven"]);
  ("PRIMO", ["VICTORIA"]);
];;

assert(test g (topol g));;

let g = [
  ("one", ["three"]);
  ("one", ["two"]);
  ("two", []);
  ("two", []);
  ("two", ["three"]);
];;

assert(test g (topol g));;

if debug then print_endline "OK";;

if debug then print_endline "Cyclic correctness tests...";;

let g = [
  (10.001, [10.002]);
  (10.002, [10.001])
];;

assert(test_cyclic g);;

let g = [
  (1, [7; 2; 3]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (2, [3]);
  (3, [4; 5; 3]);
];;

assert(test_cyclic g);;

let g = [
  ("pole", ["pole"; "lyse"; "pole"])
];;

assert(test_cyclic g);;

let g = [
  ("tu", ["tudu"]);
  ("tudu", ["tudu"; "tudu"; "tudu"])
];;

assert(test_cyclic g);;

if debug then print_endline "OK";;
