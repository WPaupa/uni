(*DRZEWA LEWICOWE - WPF ZAD 2*)
(*AUTOR KODU - WOJCIECH PAUPA*)
(*CODE REVIEW - KACPER HARASIMOWICZ*)
(*pierwszy element Node to lewe poddrzewo, drugi to prawe, trzeci to klucz, czwarty to prawa wysokość*)
type 'a queue =
  | Null
  | Node of 'a queue * 'a queue * 'a * int;; 

(*zwraca prawą wysokość, tylko null się liczy jako liść*)
let sval a =
  match a with
  | Null -> 0
  | Node(_,_,_,v) -> v;;

let rec join a b =
  match a,b with
  | Null,b->b
  | a,Null->a
  | Node(al,ar,akey,asval), Node(bl,br,bkey,bsval) ->
      (*jeśli klucz a>klucz b, to wykonujemy ten sam algorytm, ale z zamienionymi a i b*)
      if akey <= bkey then
        (*złączamy prawe poddrzewo z drzewem do złączenia rekurencyjnie*)
        let r = join ar b 
        (*zamieniamy kolejność lewego z prawym tak, żeby spełniony był warunek lewicowości*)
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

let empty = Null;;
let add a q = join q (Node(Null,Null,a,1));;

exception Empty;;
let delete_min = function
  | Null -> raise Empty
  | Node(l,r,key,_) -> (key, join l r);;

let is_empty  = function
  | Null -> true
  | Node(_,_,_,_) -> false;;