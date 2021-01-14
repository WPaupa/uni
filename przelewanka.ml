open Queue

let przelewanka ar = 
	let n = Array.length ar in
	let tab = Array.init (n+1) (fun n -> if n=0 then 1 else fst (ar.(n-1))) in
	let iloczyny = Array.make (n+1) 1 in
	for i = 1 to n
	do
		iloczyny.(i) <- iloczyny.(i-1) * (tab.(i)+1)
	done;
	let ($$) stan pos =
	 (stan mod iloczyny.(pos))/(iloczyny.(pos-1)) in
	let (>>) (stan,pos) zmiana =
		stan + (iloczyny.(pos-1)) * (zmiana - (stan $$ pos)) in
	let stany = Array.make (iloczyny.(n)) (-1) 
	and q = create () in
	add (0,0) q;
	while not (is_empty q)
	do
		let (st,dist) = take q in
		if stany.(st) = -1 then
		begin
		 	stany.(st) <- dist;
		 	for i = 1 to n
		 	do
		 		if stany.((st,i) >> 0) = -1
		 		then add (((st,i) >> 0),dist+1) q;
		 		if stany.((st,i) >> tab.(i)) = -1
		 		then add (((st,i) >> tab.(i)),dist+1) q;
		 		for j = 1 to n
		 		do
		 			let przelane = min (tab.(j) - (st $$ j)) (st $$ i) in
		 			let wylane = (st,i) >> ((st $$ i) - przelane) in
		 			let nowystan = (wylane,j) >> ((wylane $$ j) + przelane) in
		 			if stany.(nowystan) = -1
		 			then add (nowystan,dist+1) q;
		 		done
		 	done;
		end 
	done;
	let x = ref 0 in
	for i = 0 to n-1
	do
		x := (!x) + iloczyny.(i) * (snd ar.(i))
	done; stany.(!x)