open Queue

let przelewanka ar = 
	let n = Array.length ar in
	(*tworzymy tablice pojemnosci, ale indeksowana od 1 a nie od 0*)
	let tab = Array.init (n+1) (fun n -> if n=0 then 1 else fst (ar.(n-1))) in
	(*tworzymy tablice iloczynow prefiksowych, iloczyny.(n) to iloczyn
	wszystkich pojemnosci kubkow*)
	let iloczyny = Array.make (n+1) 1 in
	for i = 1 to n
	do
		iloczyny.(i) <- iloczyny.(i-1) * (tab.(i)+1)
	done;
	(*==operacje na hashu==*)

	(*zwraca ilosc wody z kubka na pozycji pos w danym stanie*)
	let ($$) stan pos =
	 (stan mod iloczyny.(pos))/(iloczyny.(pos-1)) in

	(*zwraca stan z iloscia wody z kubka na pozycji pos
	zmieniona na zmiana*)
	let (>>) (stan,pos) zmiana =
		stan + (iloczyny.(pos-1)) * (zmiana - (stan $$ pos)) in
	(*tablica odwiedzonych w BFSie*)
	let stany = Array.make (iloczyny.(n)) (-1) 
	(*kolejka trzymajaca stan do odwiedzenia
	i odleglosc w ruchach od stanu pustego*)
	and q = create () in
	add (0,0) q;
	(*BFS*)
	while not (is_empty q)
	do
		let (st,dist) = take q in
		(*jesli wierzcholek nie byl rozwazony,
		to go rozwazamy*)
		if stany.(st) = -1 then
		begin
		 	stany.(st) <- dist;
		 	for i = 1 to n
		 	do
		 		(*co jesli chcemy wylac wode z kubka*)
		 		if stany.((st,i) >> 0) = -1
		 		then add (((st,i) >> 0),dist+1) q;
		 		(*co jesli chcemy zapelnic kubek*)
		 		if stany.((st,i) >> tab.(i)) = -1
		 		then add (((st,i) >> tab.(i)),dist+1) q;
		 		(*co jesli chcemy przelac wode z kubka do innego*)
		 		for j = 1 to n
		 		do
		 			(*przelewamy tyle wody, ile sie zmiesci w kubku j*)
		 			let przelane = min (tab.(j) - (st $$ j)) (st $$ i) in
		 			(*stan po wylaniu wody z kubka i*)
		 			let wylane = (st,i) >> ((st $$ i) - przelane) in
		 			(*i dolaniu go do kubka j*)
		 			let nowystan = (wylane,j) >> ((wylane $$ j) + przelane) in

		 			if stany.(nowystan) = -1
		 			then add (nowystan,dist+1) q;
		 		done
		 	done;
		end 
	done;
	(*wyluskujemy wynik z tablicy odwiedzonych*)
	let x = ref 0 in
	for i = 0 to n-1
	do
		x := (!x) + iloczyny.(i) * (snd ar.(i))
	done; stany.(!x)