let head l n =
  let rec aux l n a = 
    if n = 0 then a
    else match l with
      | [] -> a
      | h::b -> aux b (n-1) (h::a)
  in let rec rev l b = 
       match l with
       | [] -> b
       | h::a -> rev a (h::b)
  in rev (aux l n []) [];;


let rec tail l n = 
  let rec aux l n a = 
    if n = 0 then a
    else match l with
      | [] -> a
      | h::b -> aux b (n-1) (h::a)
  in let rec rev l b = 
       match l with
       | [] -> b
       | h::a -> rev a (h::b)
  in aux (rev l []) n [];;

let tails l =
  let rec aux l acc =
    match l with
    | [] -> l::acc
    | h::t -> l::(aux t acc)
  in aux l [];;

tails [1;2;3;4;5];;

let rec f l = 
  let rec aux a b ak = if a=0 then ak else aux (a-1) b (b::ak)
  in match l with
  | [] -> []
  | (a,b)::h -> aux a b (f h);;

let shuffle m n =
  let rec aux m n l =
    match (m,n) with
    | ([],[]) -> l
    | ([],h::a) -> aux [] a (h::l)
    | (h::a,[]) -> aux a [] (h::l)
    | (h::a,i::b) -> aux a b (i :: (h :: l))
  in let rec rev l b = 
       match l with
       | [] -> b
       | h::a -> rev a (h::b)
  in rev (aux m n []) [];;

open List;; 

let cmp x y = if x>y then 4 else 2;;
merge cmp [1;2;4;4] [3;4;5;6];;

let difc a b =
  let rec dcount a acc =
    match a with
    | [] -> acc
    | h::[] -> acc+1
    | h::(l::t) -> if l=h then dcount (l::t) (acc) else dcount (l::t) (acc+1)
  in let cmp x y = if x>y then 1 else -1
  in dcount (merge cmp a b) 0;;

difc [1;2;4;4] [2;3;4;5;5;6;7];;

let zsumuj a : (int list) =
  let rec aux a acc =
    match a with
    | [] -> acc
    | h::t -> match acc with
      |[] -> aux t [h]
      |j::u -> aux t ((j+h)::acc)
  in aux a [];;

zsumuj [1;2;3;4;5];;

let lider a =
  let rec aux a lid wyst =
    match a with
    | [] -> lid
    | h::t -> if lid = h
        then aux t lid (wyst+1)
        else if wyst>1
        then aux t lid (wyst-1)
        else aux t h 1
  in aux a (-1) (-1);;

let mieszaj a : (int list)=
  let rec dopisz a acc = match a with
    | []-> acc
    | h::t -> dopisz (rev t) (acc@[h])
  in dopisz a [];;

mieszaj [1;2;3;4;5];;

let rec rev l =
  let rec aux l b =
    match l with
    | [] -> b
    | h::a -> aux a (h::b)
  in aux l [];;

let podziel l =
  let rec aux l acc acc2 i suma=
    match l with 
    |[]->acc2 
    |h::t ->
        let acc = h::acc in
        if i*(i+1)/2 =  suma+ h 
        then aux t [] ((rev acc)::acc2) (i+1) (suma+h)
        else aux t acc acc2 (i+1) (suma +h)
  in rev(aux l [] [] 1 0);;

podziel [2;3;1;6;5;4;7;9;10;11;8];;  
let trojki a = 
  let rec jed l m a acc =
    match a with
    | [] -> acc
    | h::t -> if l + m > h 
        then jed l m t ((l,m,h)::acc) 
        else acc
  in let rec dwa l a acc =
       match a with
       | [] -> acc
       | h::t -> dwa l t ((jed l h t []) @ acc)
  in let rec trz a acc =
       match a with
       | [] -> acc
       | h::t -> trz t ((dwa h t []) @ acc) 
  in trz a [];;

trojki [1;2;3;4;5;6];;
open List;;
let malo a  = 
  let rec aux k ans =
    if length k < 2 then ans
    else 
      let var = abs((hd k) + (hd (tl k)))
      in aux (tl k) (min ans var)
    
  in let cmp x y = if abs x > abs y then 1 else -1
  in let s = fast_sort cmp a
  in aux s (abs ((hd s) + (hd (tl s))));;

let malo a = 
  let rec aux b r ans =
    if (length b) + (length r) < length a + 2
    then ans
    else 
      let new_ans = 
        min ans (abs ((hd b) + (hd r)))
      in if abs ((hd b) + (hd r))
            > abs ((hd (tl b)) + (hd r))
      then aux (tl b) r (min ans new_ans)
      else aux b (tl r) (min ans new_ans)
  in aux a (rev a) (abs ((hd a) + (hd (tl a))));;
malo [-42; -12; -8;-1; -1; 5; 15; 60];;