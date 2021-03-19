open Queue

exception Ex of int;;

(*Algorytm Euklidesa*)
let rec gcd a b =
        if b = 0 then a else gcd b (a mod b);;

(*Liczy NWD elementow tablicy, ktora ma
co najmniej jeden element*)
let __gcd l = Array.fold_left gcd (l.(0)) l;;

let przelej ar tab = 
  let n = Array.length ar in
  (*zwracamy od razu dla pustej
  tablicy, zeby nie wywalalo sie nwd*)
  if n=0 then raise (Ex 0);
  (*sprawdzamy, czy kazdy element 
  do otrzymania dzieli sie przez nwd pojemnosci*)
  let g = __gcd ar in
  (*jesli wszystkie elementy tablicy pojemnosci
  sa zerami, to nwd tez bedzie: specjalny przypadek*)
  if g <> 0 then
  if not (Array.for_all (fun k -> k mod g = 0) tab) then raise (Ex (-1));
  (*sprawdzamy, czy w tablicy do otrzymania jest co najmniej jedna
  pusta lub pelna szklanka*)
  let check = ref true in
  for i = 0 to (n-1)
  do
  	if (tab.(i)=0) || (ar.(i)=tab.(i)) then check := false;
  done;
  (*jesli nie, to jaki bylby ostatni ruch?*)
  if !check then raise (Ex (-1));
  (*stany to hashmapa unitow, czyli w pewnym
  sensie set zaimplementowany za pomoca hashmapy.*)
  (*sluzy nam do sprawdzania, czy wierzcholek jest
  odwiedzony.*)
  let stany = Hashtbl.create 100000
  (*q to kolejka wierzcholkow do rozpatrzenia w bfsie
  wraz z ich odleglosciami od zrodla*)
  and q = create ()
  (*pierwszy stan do rozpatrzenia*)
  and pustostan = Array.make n 0 in
  add (pustostan,0) q;
  (*BFS*)
  while not (is_empty q)
  do
    let (st,dist) = take q in
    (*sprawdzamy, czy mozemy
    przedwczesnie skonczyc bfsa,
    bo otrzymalismy wynik*)
    let dupy = ref false in 
    for i = 0 to (n-1) do
      if (tab.(i)<>st.(i)) then dupy := true;
    done;
    if not (!dupy) then raise (Ex dist);
    (*dodatkowe sprawdzenie, czy rozwazamy
    nieodwiedzony jeszcze wierzcholek,
    tak na wszelki*)
    if not (Hashtbl.mem stany st) then
      begin 
        (*dodajemy wierzcholek do odwiedzonych*)
        Hashtbl.add stany (Array.copy st) ();
        (*rozpatrujemy wszystkie kubki i*)
        for i = 0 to (n-1)
        do
          let p = st.(i) in
          (*co gdybysmy wylali cala wode
          z kubka i do zlewu*)
          st.(i) <- 0;
          if not (Hashtbl.mem stany st)
          then add (Array.copy st,dist+1) q;
          (*co gdybysmy zapelnili z kranu kubek i*)
          st.(i) <- ar.(i);
          if not (Hashtbl.mem stany st) 
          then add (Array.copy st,dist+1) q;
          (*dla pewnosci zmieniamy wode w kubku i
          znow na taka, jaka byla*)
          st.(i) <- p;
          (*co gdybysmy przelali wode z kubka i
          	do pewnego kubka j (j niekoniecznie rozne od i)*)
          for j = 0 to (n-1)
          do
            let r = st.(j) in
            (*jesli w kubku i jest wiecej wody
            niz zmiesci sie w kubku j, to przelewamy
            tylko tyle, ile sie zmiesci. w przeciwnym
            wypadku przelewamy calosc.*)
            let przelane = min (ar.(j) - r) p in
            st.(i) <- p - przelane;
            (*uwaga implementacyjna: tutaj jest
            po prawej st.(j) zamiast r, zeby
            st.(i) sie nie zmienilo sumarycznie
            dla i = j*)
            st.(j) <- st.(j) + przelane;
            if not (Hashtbl.mem stany st) 
            then add (Array.copy st,dist+1) q; 
            st.(i) <- p;
            st.(j) <- r;
          done
        done;
      end 
  done; 
  (*jesli nie znalezlismy stanu, ktory szukalismy,
  to zwracamy -1*)
  (-1)

(*przechwytywanie wyjatku*)
let przelewanka ar = 
	try przelej 
		(Array.map (fun (x,_)-> x) ar) 
		(Array.map (fun (_,y)-> y) ar) 
	with Ex(i) -> i