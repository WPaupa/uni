(*AUTOR KODU: WOJCIECH PAUPA*)
(*CODE REVIEW: MACIEJ RACZUK*)
(*ZADANIE ARYTMETYKA, WPF 2020*)

(*kazda liczba z zadania jest:
zbiorem pustym, przedzialem lub dopelnieniem przedzialu*)
type wartosc = 
  | Przedzial of float*float 
  | Dopelnienie of float*float
  | Pusty;;

(*w stalych Przedzial i Dopelnienie wazna jest czystosc:
druga liczba musi byc wieksza od pierwszej.
dopelnienie sprawdza ten warunek wprost,
natomiast przedzial nie jest wywolywany
z x>y*)

(*chcemy, zeby przedzial zawieral w sobie:
0. jesli ma tylko wartosci dodatnie i -0. jesli ma tylko wartosci ujemne*)
let przedzial(x,y) =
  if (x=0.) && (y=0.) then Przedzial(-0.,0.) 
  else if x=0. then Przedzial(0.,y)
  else if y=0. then Przedzial(x,-0.)
  else Przedzial(x,y);;

(*dopelnienie(x,y) zwraca sume przedzialow od -inf do x oraz od x do inf*)
let dopelnienie(x,y) =
  if (x>=y) then przedzial(neg_infinity,infinity) else 
  if x=0. then Dopelnienie(0.,y)
  else if y=0. then Dopelnienie(x,-0.)
  else Dopelnienie(x,y);;

let wartosc_od_do x y = przedzial(x,y);;
(*nie chcemy wywolywac przedzial z x>y*)
let wartosc_dokladnosc x p = if (x>=0.) then wartosc_od_do
      (x -. ((p *. x) /. 100.)) 
      (x +. ((p *. x) /. 100.))
  else wartosc_od_do
      (x +. ((p *. x) /. 100.)) 
      (x -. ((p *. x) /. 100.))
let wartosc_dokladna x = przedzial(x,x);;

(*zeby nie trzeba bylo korzystac
z modulu Float*)
let is_nan x = (compare x nan) = 0;;

(*alternatywne definicje mina i maxa,
bo funkcje min i max z ocamla
psuja sie przy nanach*)

let mux x y =
  if is_nan x then y
  else if is_nan y then x
  else max x y;;

let mun x y =
  if is_nan x then y
  else if is_nan y then x
  else min x y;; 

(*przedzialy sa domykane,
wiec wszedzie <= zamiast <*)
let in_wartosc x y = 
  match x with
  | Przedzial(a,b) -> (a<=y) && (y<=b) 
  | Dopelnienie(a,b) -> (y<=a) || (b<=y)
  | Pusty -> false;;

let min_wartosc x =
  match x with
  | Przedzial(a,b) -> a
  | Dopelnienie(a,b) -> neg_infinity
  (* co zrobic z pustym zbiorem?*)
  | Pusty -> nan;;

let max_wartosc x =
  match x with
  | Przedzial(a,b) -> b
  | Dopelnienie(a,b) -> infinity
  | Pusty -> nan;;

let sr_wartosc x =
  match x with
  | Przedzial(a,b) ->
      (*match dla estetyki, sprawdza
       mozliwe nieskonczone wartosci a i b*)
      (match a,b with
       | t,u when (t = neg_infinity) && (u=infinity) -> nan
       | t,_ when t = neg_infinity -> neg_infinity
       | _,t when t = infinity -> infinity
       | a,b -> (a+.b)/.2.
      )
  | Dopelnienie(a,b) -> nan 
  | Pusty -> nan;; 

(*przy dodawaniu przedzialu do przedzialu,
minimalna wartosc bedzie suma minimalnych wartosci
i podobnie z max. Przy dodawaniu przedzialu
do dopelnienia, najmniejsza nieosiagalna
wartosc bedzie suma najmniejszej nieosiagalnej
wartosci dopelnienia i najwiekszej wartosci przedzialu;
podobnie z najwieksza nieosiagalna.
Przy dodawaniu dopelnienia do dopelnienia,
mozemy wygenerowac dowolna liczbe jako
-duza_liczba + (duza_liczba+x) *)
let plus a b =
  match a with
  | Przedzial(xa,ya) -> 
      (match b with
       | Przedzial(xb,yb) -> przedzial(xa+.xb,ya+.yb)
       | Dopelnienie(xb,yb) -> dopelnienie(ya+.xb,xa+.yb) 
       | Pusty -> Pusty
      )
  | Dopelnienie(xa,ya) ->
      (match b with 
       | Przedzial(xb,yb) -> dopelnienie(yb+.xa,xb+.ya)
       | Dopelnienie(xb,yb) -> przedzial(neg_infinity,infinity)
       | Pusty -> Pusty
      )
  | Pusty -> Pusty;; 


(*przedzial-przedzial i dopelnienie-dopelnienie
podobnie jak przy dodawaniu. najmniejsza nieosiagalna
liczba w dopelnienie-przedzial bedzie roznica
najmniejszej nieosiagalnej liczby dopelnienia
i najwiekszej liczby przedzialu. Odwrotnie
dla najwiekszej nieosiagalnej liczby i vice versa
dla przedzial-dopelnienie.*)
let minus a b =
  match a with
  | Przedzial(xa,ya) ->
      (match b with 
       | Przedzial(xb,yb) -> przedzial(xa-.yb,ya-.xb)
       | Dopelnienie(xb,yb) -> dopelnienie(ya-.yb,xa-.xb)
       | Pusty -> Pusty
      )
  | Dopelnienie(xa,ya) ->
      (match b with 
       | Przedzial(xb,yb) -> dopelnienie(xa-.xb,ya-.yb)
       | Dopelnienie(xb,yb) -> przedzial(neg_infinity,infinity)
       | Pusty -> Pusty
      )
  | Pusty -> Pusty;; 

let razy a b =
  match a with
  | Przedzial(xa,ya) ->
      (match b with
       | Przedzial(xb,yb) ->
           (*definiujemy wszystkie iloczyny xa,xb z ya,yb,
             zeby latwiej bylo o tym myslec. jesli mnozymy
             0.,0. przez nieskonczony przedzial,
             to wynik jest 0.,0.; ale komputer zwroci nany.*)
           let il1 = xa*.xb in
           let il2 = ya*.xb in
           let il3 = xa*.yb in
           let il4 = ya*.yb in
           if (is_nan (mun (mun il1 il2) (mun il3 il4)))
           || (is_nan (mux (mux il1 il2) (mux il3 il4))) 
           then przedzial(0.,0.)
           else
             (*mozliwe wartosci w przedzial*przedzial
               to wszystkie od najmniejszego iloczynu
               do najwiekszego.*)
             przedzial(
               mun (mun il1 il2) (mun il3 il4),
               mux (mux il1 il2) (mux il3 il4)
             )
       | Dopelnienie(xb,yb) -> 
           let il1 = xa*.xb in
           let il2 = ya*.xb in
           let il3 = xa*.yb in
           let il4 = ya*.yb in
             (*zakomentowana funkcja robi to samo
             i może jest bardziej przejrzysta,
             ale nowe rozwiazanie jest zgrabiejsze*)
           (*match xa,ya with
            | c,d when (c <0.) && (d >0.) -> przedzial(neg_infinity,infinity)
            | c,d when (c>=0.) && (d >0.) -> dopelnienie(mux il1 il2,mun il3 il4)
            | c,d when (c <0.) && (d<=0.) -> dopelnienie(mux il3 il4,mun il1 il2)
            | _,_ -> przedzial(0.,0.)
           *)
            if (xa<0.) && (ya>0.) then przedzial(neg_infinity,infinity) else
           if (xa=0.) && (ya=0.) then przedzial(0.,0.) else
           (*tutaj i w pozniejszych miejscach w kodzie
             wewnetrzna funkcja mun i mux tak naprawde sprawdza
             tylko znaki liczb. Zeby lepiej to zilustrowac,
             zostawilem tego matcha na gorze.*)
           dopelnienie(mux (mun il1 il3) (mun il2 il4),
                       mun (mux il1 il3) (mux il2 il4)) 
       | Pusty -> Pusty
      )
  | Dopelnienie(xa,ya) ->
      (match b with
       | Przedzial(xb,yb) ->
           (* a*b=b*a, przekopiowalem kod*)
           let il1 = xb*.xa in
           let il2 = yb*.xa in
           let il3 = xb*.ya in
           let il4 = yb*.ya in
           (*match xb,yb with
            | c,d when (c <0.) && (d >0.) -> przedzial(neg_infinity,infinity)
            | c,d when (c>=0.) && (d >0.) -> dopelnienie(mux il1 il2,mun il3 il4)
            | c,d when (c <0.) && (d<=0.) -> dopelnienie(mux il3 il4,mun il1 il2)
            | _,_ -> przedzial(0.,0.)
           *)
           if (xb<0.) && (yb>0.) then przedzial(neg_infinity,infinity) else
           if (xb=0.) && (yb=0.) then przedzial(0.,0.) else
           dopelnienie(mux (mun il1 il3) (mun il2 il4),
                       mun (mux il1 il3) (mux il2 il4)) 
       | Dopelnienie(xb,yb) ->
           if ( (xa>0.) || (xb>0.) ) || ( (ya<0.) || (yb<0.) )
           then przedzial(neg_infinity,infinity)
           else dopelnienie(mux (xa*.yb) (xb*.ya), mun (xa*.xb) (ya*.yb))
       | Pusty -> Pusty
      )
  | Pusty -> Pusty;;

let podzielic a b =
  match b with
  | Przedzial(xb,yb) when (xb =0.) && (yb =0.) -> Pusty 
  | Przedzial(xb,yb) when (xb<=0.) && (yb<=0.) ->
      (match a with
       | Przedzial(xa,ya) -> przedzial
                               (mun (ya/.xb) (ya/.yb), mux (xa/.xb) (xa/.yb))
       | Dopelnienie(xa,ya) -> dopelnienie 
                                 (mux (ya/.xb) (ya/.yb), mun (xa/.xb) (xa/.yb))
       | Pusty -> Pusty
      ) 
  | Przedzial(xb,yb) when (xb <0.) && (yb >0.) ->
      (match a with 
       | Przedzial(xa,ya) -> if (xa=0.) && (ya=0.) then przedzial(0.,0.)
	   else if (xa*.ya<0.) then przedzial(neg_infinity,infinity)
           else dopelnienie(mux (mun (ya/.xb) (ya/.yb)) (mun (xa/.xb) (xa/.yb)),
                            mun (mux (ya/.xb) (ya/.yb)) (mux (xa/.xb) (xa/.yb)))
       | Dopelnienie(xa,ya) -> 
           dopelnienie(mux (xa/.yb) (ya/.xb), mun (xa/.xb) (ya/.yb))
             (*jesli xa>0 lub ya<0, to funkcja  dopelnienie zwroci
                                 przedzial(neg_infinity,infinity*) 
       (* jesli xa=ya=0, to funkcja dopelnienie zwroci przedzial pelny*)  
       | Pusty -> Pusty
      )
  | Przedzial(xb,yb) when (xb>=0.) && (yb>=0.) ->
      (match a with
       | Przedzial(xa,ya) -> przedzial 
                               (mun (xa/.yb) (xa/.xb), mux (ya/.xb) (ya/.yb))
       | Dopelnienie(xa,ya) -> dopelnienie
                                 (mux (xa/.yb) (xa/.xb), mun (ya/.xb) (ya/.yb))
       | Pusty -> Pusty
      )
  | Dopelnienie(xb,yb) when (xb<=0.) && (yb>=0.) -> 
      (match a with
       | Przedzial(xa,ya) -> przedzial
                               (mun (mun (xa/.yb) (xa/.xb)) (mun (ya/.yb) (ya/.xb)),
                                mux (mux (xa/.yb) (xa/.xb)) (mux (ya/.yb) (ya/.xb)))
       | Dopelnienie(xa,ya) -> przedzial(neg_infinity,infinity)
       | Pusty -> Pusty
      )
  | Dopelnienie(xb,yb) when (xb >0.) && (yb >0.) -> 
      (match a with
       | Przedzial(xa,ya) -> if (xa*.ya)<0. then przedzial(neg_infinity,infinity)
           else if (xa>0.) then dopelnienie(
               mux (xa/.yb) (ya/.yb), mun (xa/.xb) (ya/.xb))
           else dopelnienie (
               mux (xa/.xb) (ya/.xb), mun (xa/.yb) (ya/.yb))
       | Dopelnienie(xa,ya) -> przedzial(neg_infinity,infinity)
       | Pusty -> Pusty
      )
  | Dopelnienie(xb,yb) when (xb <0.) && (yb <0.) ->
      (match a with
       | Przedzial(xa,ya) -> if (xa*.ya)<0. then przedzial(neg_infinity,infinity)
           else if (xa<0.) then dopelnienie(
               mux (xa/.xb) (ya/.xb), mun (xa/.yb) (ya/.yb))
           else dopelnienie (
               mux (xa/.yb) (ya/.yb), mun (xa/.xb) (ya/.xb))
       | Dopelnienie(xa,ya) -> przedzial(neg_infinity,infinity)
       | Pusty -> Pusty
      )
  (*zeby pozbyc sie warnow *)
  | Przedzial(_,_) -> Pusty
  | Dopelnienie(_,_) -> Pusty
  | Pusty -> Pusty;; 

(*TESTY *)
let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, 2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s;;                        (* (-inf, 0) *)

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert (is_nan (sr_wartosc c) );
assert (not (in_wartosc c 0.));
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert (is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, is_nan (sr_wartosc p)) = (neg_infinity, infinity, true));
assert ((min_wartosc q, max_wartosc q, is_nan (sr_wartosc q)) = (neg_infinity, infinity, true));
assert ((min_wartosc r, max_wartosc r, is_nan (sr_wartosc r)) = (neg_infinity, infinity, true));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));;

let a = wartosc_od_do neg_infinity infinity
let c = plus a a
let d = razy a a
let e = podzielic a a
let f = minus a a;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));
assert ((min_wartosc e, max_wartosc e, is_nan (sr_wartosc e)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do 0. infinity
let b = wartosc_dokladna 0.
let c = podzielic a b
let d = podzielic b b;;
assert ((is_nan(min_wartosc c), is_nan(max_wartosc c), is_nan (sr_wartosc c)) = (true, true, true));
assert ((is_nan(min_wartosc d), is_nan(max_wartosc d), is_nan (sr_wartosc d)) = (true, true, true));;

let a = wartosc_od_do (-10.) 10.
let b = wartosc_od_do (-1.) 1000.
let c = podzielic a b;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do (-1.0) 1.0
let b = wartosc_dokladna 1.0
let c = podzielic b a
let d = wartosc_dokladna 3.0
let e = plus c d      (* (-inf, 2> U <4 inf) *)
let f = podzielic b e (* (-inf, 1/4> U <1/2, inf) *)
let g = podzielic d a (* (-inf, -3> U <3, inf) *)
let h = podzielic g f (* (-inf, inf *)
let i = plus f g;;    (* (-inf, inf) *)

assert ((in_wartosc f 0.25, in_wartosc f 0.26, in_wartosc f 0.49, in_wartosc f 0.50)=(true, false, false, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.) = (neg_infinity, infinity, true, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.3) = (neg_infinity, infinity, true, true));;

let jed = wartosc_dokladna 1.
let zero = wartosc_dokladna 0.;;
assert ((sr_wartosc zero, max_wartosc zero, min_wartosc zero) = (0.,0.,0.));;

let a = wartosc_od_do 0. 1. (* <0,1> *)
let b = podzielic a a       (* <0, inf)*)
let c = razy b zero;;       (* <0,0> *)
assert ((sr_wartosc c, max_wartosc c, min_wartosc c) = (0.,0.,0.));;

let a = podzielic jed zero;; (* nan *)
assert (is_nan (min_wartosc a));
assert (is_nan (max_wartosc a));
assert (is_nan (sr_wartosc a));;

let a = wartosc_dokladnosc 1. 110.;; (* <-0.1, 2.1> *)
assert (in_wartosc a (-.0.1));
assert (in_wartosc a (2.1));;

let a = wartosc_od_do (-.3.) 0.  (* <-3.0, 0.0> *)
let b = wartosc_od_do 0. 1.      (* <-0.0, 1.0> *)
let c = podzielic a b;;          (* (-inf, 0> *)
assert (max_wartosc c = 0.);
assert (min_wartosc c = neg_infinity);
assert (sr_wartosc c = neg_infinity);;

let a = wartosc_od_do 1. 4.     (* <1.0, 4.0> *)
let b = wartosc_od_do (-.2.) 3. (* <-2.0, 3.0> *)
let c = podzielic a b           (* (-inf, -1/2> U <1/3, inf) *)
let d = podzielic c b           (* (-inf, -1/6> U <1/9, inf) *)
let e = plus d jed              (* (-inf, 5/6> U <10/9, inf) *)
let f = sr_wartosc (podzielic jed (wartosc_dokladna 9.));; (* 1/9 *)
assert (is_nan (sr_wartosc d));
assert (in_wartosc d 0.12);
assert (not (in_wartosc d 0.));
assert (not (in_wartosc d (-0.125)));
assert (in_wartosc d f);
assert (not (in_wartosc e 1.));;

(* uwaga, ten test moze sie zawiesic przy pewnych implementacjach! *)
let a = wartosc_od_do (-2.) 3.
let b = wartosc_od_do 2. 3.
let c = podzielic b a

let rec iteruj f n acc = match n with
    | 0 -> acc
    | n when n > 0 -> iteruj f (n-1) (f acc acc)
    | _ -> acc

let x = iteruj razy 10 c;;
assert (not (in_wartosc x 0.));;

