exception WA;;

(* Returns true if ALL values from q are taken out in the order given in l *)
let test q l =
    try
      let (b, nq) = List.fold_left (fun a x -> 
        let (e, nq) = delete_min (snd a)
        in 
        if(compare x e != 0) then raise WA 
        else (true, nq)) 
                                   (true, q) l
      in
      b && (is_empty nq)
    with WA -> false
;;

let q1 = empty and q2 = empty;;
let q1 = add 38 q1;;
assert ( fst (delete_min q1) = 38);;
let q1 = add 19 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 38 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 37 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 55 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 97 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 65 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 85 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 50 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 12 q1;;
assert ( fst (delete_min q1) = 12);;

let q2 = add 53 q2;;
assert ( fst (delete_min q2) = 53);;
let q2 = add 0 q2;;
assert ( fst (delete_min q2) = 0);;
let q2 = add 42 q2;;
assert ( fst (delete_min q2) = 0);;
let q2 = add 81 q2;;
assert ( fst (delete_min q2) = 0);;
let q2 = add 37 q2;;
assert ( fst (delete_min q2) = 0);;
let q2 = add 21 q2;;
assert ( fst (delete_min q2) = 0);;
let q2 = add 45 q2;;
assert ( fst (delete_min q2) = 0);;
let q2 = add 85 q2;;
assert ( fst (delete_min q2) = 0);;
let q2 = add 97 q2;;
assert ( fst (delete_min q2) = 0);;
let q2 = add 80 q2;;
assert ( fst (delete_min q2) = 0);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [0;12;19;21;37;37;38;38;42;45;50;53;55;65;80;81;85;85;97;97]);;

