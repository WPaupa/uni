open PMap;;
exception Cykliczne;;
let topol g =
  let m = List.fold_left (fun m (k,v) -> try add k (v@(find k m)) m with Not_found -> add k v m) empty g in
  let marx = ref (create compare)
  and wyn = ref ([]) in
  let rec visit n tempmarx =
    try 
      find n !marx
    with Not_found ->
      try
        find n tempmarx; raise Cykliczne
      with Not_found ->
        (try
           List.iter (fun x -> visit x (add n () tempmarx)) (find n m)
         with Not_found -> ());
        marx := add n () !marx;
        wyn := n::(!wyn) in
  iter (fun k v -> try find k (!marx) with Not_found -> visit(k) empty) m;
  !wyn;;