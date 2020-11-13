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