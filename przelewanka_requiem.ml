open Queue

exception Ex of int;;

let rec gcd a b =
        if b = 0 then a else gcd b (a mod b);;

let __gcd l = Array.fold_left gcd (l.(0)) l;;

let przelej ar tab = 
  let n = Array.length ar in
  if n=0 then raise (Ex 0);
  let g = __gcd ar in
  if g <> 0 then
  if not (Array.for_all (fun k -> k mod g = 0) tab) then raise (Ex (-1));
  let check = ref true in
  for i = 0 to (n-1)
  do
  	if (tab.(i)=0) || (ar.(i)=tab.(i)) then check := false;
  done;
  if !check then raise (Ex (-1));
  let stany = Hashtbl.create 100000
  and q = create ()
  and pustostan = Array.make n 0 in
  add (pustostan,0) q;
  while not (is_empty q)
  do
    let (st,dist) = take q in
    let dupy = ref false in 
    for i = 0 to (n-1) do
      if (tab.(i)<>st.(i)) then dupy := true;
    done;
    if not (!dupy) then raise (Ex dist);
    if not (Hashtbl.mem stany st) then
      begin 
        Hashtbl.add stany (Array.copy st) ();
        for i = 0 to (n-1)
        do
          let p = st.(i) in
          st.(i) <- 0;
          if not (Hashtbl.mem stany st)
          then add (Array.copy st,dist+1) q;
          st.(i) <- ar.(i);
          if not (Hashtbl.mem stany st) 
          then add (Array.copy st,dist+1) q;
          st.(i) <- p;
          for j = 0 to (n-1)
          do
            let r = st.(j) in
            let przelane = min (ar.(j) - r) p in
            st.(i) <- p - przelane;
            st.(j) <- st.(j) + przelane;
            if not (Hashtbl.mem stany st) 
            then add (Array.copy st,dist+1) q; 
            st.(i) <- p;
            st.(j) <- r;
          done
        done;
      end 
  done; (-1)

let przelewanka ar = try przelej (Array.map (fun (x,_)-> x) ar) (Array.map (fun (_,y) -> y) ar) with Ex(i) -> i