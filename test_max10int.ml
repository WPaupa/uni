type 'a queue =
  | Null
  | Node of 'a queue * 'a queue * 'a * int;; 

let sval a =
  match a with
  | Null -> 0
  | Node(_,_,_,v) -> v;;

let rec join a b =
  match a,b with
  | Null,b->b
  | a,Null->a
  | Node(al,ar,akey,asval), Node(bl,br,bkey,bsval) ->
      if akey <= bkey then
        let r = join ar b 
        in if al = Null then
          Node(r,al,akey,1)
        else if (sval r) <= (sval al)
        then Node(al,r,akey,1+(sval r)) else Node(r,al,akey,1+(sval al))
      else
        let r = join br a 
        in if bl = Null then
          Node(r,bl,bkey,1)
        else if (sval r) <= (sval bl)
        then Node(bl,r,bkey,1+(sval r)) else Node(r,bl,bkey,1+(sval bl));;

let l n = Node(Null,Null,n,1);;

let empty = Null;;
let add a q = join q (l a);;

exception Empty;;
let delete_min = function
  | Null -> raise Empty
  | Node(l,r,key,_) -> (key, join l r);;

let is_empty  = function
  | Null -> true
  | Node(_,_,_,_) -> false;;

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

let q1 = empty and q2 = empty;;
let q1 = add 76 q1;;
assert ( fst (delete_min q1) = 76);;
let q1 = add 91 q1;;
assert ( fst (delete_min q1) = 76);;
let q1 = add 55 q1;;
assert ( fst (delete_min q1) = 55);;
let q1 = add 6 q1;;
assert ( fst (delete_min q1) = 6);;
let q1 = add 57 q1;;
assert ( fst (delete_min q1) = 6);;
let q1 = add 23 q1;;
assert ( fst (delete_min q1) = 6);;
let q1 = add 81 q1;;
assert ( fst (delete_min q1) = 6);;
let q1 = add 40 q1;;
assert ( fst (delete_min q1) = 6);;
let q1 = add 25 q1;;
assert ( fst (delete_min q1) = 6);;
let q1 = add 78 q1;;
assert ( fst (delete_min q1) = 6);;

let q2 = add 46 q2;;
assert ( fst (delete_min q2) = 46);;
let q2 = add 90 q2;;
assert ( fst (delete_min q2) = 46);;
let q2 = add 40 q2;;
assert ( fst (delete_min q2) = 40);;
let q2 = add 87 q2;;
assert ( fst (delete_min q2) = 40);;
let q2 = add 7 q2;;
assert ( fst (delete_min q2) = 7);;
let q2 = add 37 q2;;
assert ( fst (delete_min q2) = 7);;
let q2 = add 11 q2;;
assert ( fst (delete_min q2) = 7);;
let q2 = add 17 q2;;
assert ( fst (delete_min q2) = 7);;
let q2 = add 56 q2;;
assert ( fst (delete_min q2) = 7);;
let q2 = add 67 q2;;
assert ( fst (delete_min q2) = 7);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [6;7;11;17;23;25;37;40;40;46;55;56;57;67;76;78;81;87;90;91]);;

let q1 = empty and q2 = empty;;
let q1 = add 33 q1;;
assert ( fst (delete_min q1) = 33);;
let q1 = add 78 q1;;
assert ( fst (delete_min q1) = 33);;
let q1 = add 23 q1;;
assert ( fst (delete_min q1) = 23);;
let q1 = add 87 q1;;
assert ( fst (delete_min q1) = 23);;
let q1 = add 97 q1;;
assert ( fst (delete_min q1) = 23);;
let q1 = add 84 q1;;
assert ( fst (delete_min q1) = 23);;
let q1 = add 12 q1;;
assert ( fst (delete_min q1) = 12);;
let q1 = add 11 q1;;
assert ( fst (delete_min q1) = 11);;
let q1 = add 78 q1;;
assert ( fst (delete_min q1) = 11);;
let q1 = add 66 q1;;
assert ( fst (delete_min q1) = 11);;

let q2 = add 29 q2;;
assert ( fst (delete_min q2) = 29);;
let q2 = add 4 q2;;
assert ( fst (delete_min q2) = 4);;
let q2 = add 79 q2;;
assert ( fst (delete_min q2) = 4);;
let q2 = add 5 q2;;
assert ( fst (delete_min q2) = 4);;
let q2 = add 88 q2;;
assert ( fst (delete_min q2) = 4);;
let q2 = add 49 q2;;
assert ( fst (delete_min q2) = 4);;
let q2 = add 29 q2;;
assert ( fst (delete_min q2) = 4);;
let q2 = add 76 q2;;
assert ( fst (delete_min q2) = 4);;
let q2 = add 31 q2;;
assert ( fst (delete_min q2) = 4);;
let q2 = add 64 q2;;
assert ( fst (delete_min q2) = 4);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [4;5;11;12;23;29;29;31;33;49;64;66;76;78;78;79;84;87;88;97]);;

let q1 = empty and q2 = empty;;
let q1 = add 14 q1;;
assert ( fst (delete_min q1) = 14);;
let q1 = add 36 q1;;
assert ( fst (delete_min q1) = 14);;
let q1 = add 28 q1;;
assert ( fst (delete_min q1) = 14);;
let q1 = add 2 q1;;
assert ( fst (delete_min q1) = 2);;
let q1 = add 52 q1;;
assert ( fst (delete_min q1) = 2);;
let q1 = add 4 q1;;
assert ( fst (delete_min q1) = 2);;
let q1 = add 37 q1;;
assert ( fst (delete_min q1) = 2);;
let q1 = add 56 q1;;
assert ( fst (delete_min q1) = 2);;
let q1 = add 98 q1;;
assert ( fst (delete_min q1) = 2);;
let q1 = add 72 q1;;
assert ( fst (delete_min q1) = 2);;

let q2 = add 97 q2;;
assert ( fst (delete_min q2) = 97);;
let q2 = add 13 q2;;
assert ( fst (delete_min q2) = 13);;
let q2 = add 83 q2;;
assert ( fst (delete_min q2) = 13);;
let q2 = add 3 q2;;
assert ( fst (delete_min q2) = 3);;
let q2 = add 60 q2;;
assert ( fst (delete_min q2) = 3);;
let q2 = add 42 q2;;
assert ( fst (delete_min q2) = 3);;
let q2 = add 47 q2;;
assert ( fst (delete_min q2) = 3);;
let q2 = add 75 q2;;
assert ( fst (delete_min q2) = 3);;
let q2 = add 71 q2;;
assert ( fst (delete_min q2) = 3);;
let q2 = add 4 q2;;
assert ( fst (delete_min q2) = 3);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [2;3;4;4;13;14;28;36;37;42;47;52;56;60;71;72;75;83;97;98]);;

let q1 = empty and q2 = empty;;
let q1 = add 73 q1;;
assert ( fst (delete_min q1) = 73);;
let q1 = add 52 q1;;
assert ( fst (delete_min q1) = 52);;
let q1 = add 19 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 4 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 39 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 86 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 4 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 37 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 23 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 35 q1;;
assert ( fst (delete_min q1) = 4);;

let q2 = add 33 q2;;
assert ( fst (delete_min q2) = 33);;
let q2 = add 93 q2;;
assert ( fst (delete_min q2) = 33);;
let q2 = add 20 q2;;
assert ( fst (delete_min q2) = 20);;
let q2 = add 74 q2;;
assert ( fst (delete_min q2) = 20);;
let q2 = add 83 q2;;
assert ( fst (delete_min q2) = 20);;
let q2 = add 61 q2;;
assert ( fst (delete_min q2) = 20);;
let q2 = add 24 q2;;
assert ( fst (delete_min q2) = 20);;
let q2 = add 65 q2;;
assert ( fst (delete_min q2) = 20);;
let q2 = add 69 q2;;
assert ( fst (delete_min q2) = 20);;
let q2 = add 30 q2;;
assert ( fst (delete_min q2) = 20);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [4;4;19;20;23;24;30;33;35;37;39;52;61;65;69;73;74;83;86;93]);;

let q1 = empty and q2 = empty;;
let q1 = add 67 q1;;
assert ( fst (delete_min q1) = 67);;
let q1 = add 36 q1;;
assert ( fst (delete_min q1) = 36);;
let q1 = add 49 q1;;
assert ( fst (delete_min q1) = 36);;
let q1 = add 36 q1;;
assert ( fst (delete_min q1) = 36);;
let q1 = add 19 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 27 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 0 q1;;
assert ( fst (delete_min q1) = 0);;
let q1 = add 23 q1;;
assert ( fst (delete_min q1) = 0);;
let q1 = add 22 q1;;
assert ( fst (delete_min q1) = 0);;
let q1 = add 74 q1;;
assert ( fst (delete_min q1) = 0);;

let q2 = add 11 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 62 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 65 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 91 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 19 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 47 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 50 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 20 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 34 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 68 q2;;
assert ( fst (delete_min q2) = 11);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [0;11;19;19;20;22;23;27;34;36;36;47;49;50;62;65;67;68;74;91]);;

let q1 = empty and q2 = empty;;
let q1 = add 24 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 77 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 46 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 31 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 58 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 72 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 30 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 34 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 81 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 35 q1;;
assert ( fst (delete_min q1) = 24);;

let q2 = add 67 q2;;
assert ( fst (delete_min q2) = 67);;
let q2 = add 60 q2;;
assert ( fst (delete_min q2) = 60);;
let q2 = add 14 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 42 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 76 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 27 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 23 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 94 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 68 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 44 q2;;
assert ( fst (delete_min q2) = 14);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [14;23;24;27;30;31;34;35;42;44;46;58;60;67;68;72;76;77;81;94]);;

let q1 = empty and q2 = empty;;
let q1 = add 24 q1;;
assert ( fst (delete_min q1) = 24);;
let q1 = add 21 q1;;
assert ( fst (delete_min q1) = 21);;
let q1 = add 7 q1;;
assert ( fst (delete_min q1) = 7);;
let q1 = add 96 q1;;
assert ( fst (delete_min q1) = 7);;
let q1 = add 26 q1;;
assert ( fst (delete_min q1) = 7);;
let q1 = add 63 q1;;
assert ( fst (delete_min q1) = 7);;
let q1 = add 40 q1;;
assert ( fst (delete_min q1) = 7);;
let q1 = add 62 q1;;
assert ( fst (delete_min q1) = 7);;
let q1 = add 47 q1;;
assert ( fst (delete_min q1) = 7);;
let q1 = add 80 q1;;
assert ( fst (delete_min q1) = 7);;

let q2 = add 47 q2;;
assert ( fst (delete_min q2) = 47);;
let q2 = add 28 q2;;
assert ( fst (delete_min q2) = 28);;
let q2 = add 13 q2;;
assert ( fst (delete_min q2) = 13);;
let q2 = add 83 q2;;
assert ( fst (delete_min q2) = 13);;
let q2 = add 59 q2;;
assert ( fst (delete_min q2) = 13);;
let q2 = add 43 q2;;
assert ( fst (delete_min q2) = 13);;
let q2 = add 91 q2;;
assert ( fst (delete_min q2) = 13);;
let q2 = add 94 q2;;
assert ( fst (delete_min q2) = 13);;
let q2 = add 61 q2;;
assert ( fst (delete_min q2) = 13);;
let q2 = add 33 q2;;
assert ( fst (delete_min q2) = 13);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [7;13;21;24;26;28;33;40;43;47;47;59;61;62;63;80;83;91;94;96]);;

let q1 = empty and q2 = empty;;
let q1 = add 72 q1;;
assert ( fst (delete_min q1) = 72);;
let q1 = add 51 q1;;
assert ( fst (delete_min q1) = 51);;
let q1 = add 54 q1;;
assert ( fst (delete_min q1) = 51);;
let q1 = add 70 q1;;
assert ( fst (delete_min q1) = 51);;
let q1 = add 19 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 74 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 21 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 22 q1;;
assert ( fst (delete_min q1) = 19);;
let q1 = add 10 q1;;
assert ( fst (delete_min q1) = 10);;
let q1 = add 52 q1;;
assert ( fst (delete_min q1) = 10);;

let q2 = add 48 q2;;
assert ( fst (delete_min q2) = 48);;
let q2 = add 56 q2;;
assert ( fst (delete_min q2) = 48);;
let q2 = add 11 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 75 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 36 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 19 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 88 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 55 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 47 q2;;
assert ( fst (delete_min q2) = 11);;
let q2 = add 40 q2;;
assert ( fst (delete_min q2) = 11);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [10;11;19;19;21;22;36;40;47;48;51;52;54;55;56;70;72;74;75;88]);;

let q1 = empty and q2 = empty;;
let q1 = add 54 q1;;
assert ( fst (delete_min q1) = 54);;
let q1 = add 93 q1;;
assert ( fst (delete_min q1) = 54);;
let q1 = add 55 q1;;
assert ( fst (delete_min q1) = 54);;
let q1 = add 41 q1;;
assert ( fst (delete_min q1) = 41);;
let q1 = add 4 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 67 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 16 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 56 q1;;
assert ( fst (delete_min q1) = 4);;
let q1 = add 1 q1;;
assert ( fst (delete_min q1) = 1);;
let q1 = add 27 q1;;
assert ( fst (delete_min q1) = 1);;

let q2 = add 14 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 51 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 83 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 79 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 74 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 46 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 14 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 99 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 65 q2;;
assert ( fst (delete_min q2) = 14);;
let q2 = add 37 q2;;
assert ( fst (delete_min q2) = 14);;
let qx = join q1 q2;; let qy = join q2 q1;; assert(qx=qy);;
assert(test qx [1;4;14;14;16;27;37;41;46;51;54;55;56;65;67;74;79;83;93;99]);;

