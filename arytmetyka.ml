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
           (match xa,ya with
            | c,d when (c <0.) && (d >0.) -> przedzial(neg_infinity,infinity)
            | c,d when (c>=0.) && (d >0.) -> dopelnienie(mux il1 il2,mun il3 il4)
            | c,d when (c <0.) && (d<=0.) -> dopelnienie(mux il3 il4,mun il1 il2)
            | _,_ -> przedzial(0.,0.)
           )
       | Pusty -> Pusty
      )
  | Dopelnienie(xa,ya) ->
      (match b with
       | Przedzial(xb,yb) ->
           let il1 = xb*.xa in
           let il2 = yb*.xa in
           let il3 = xb*.ya in
           let il4 = yb*.ya in
           (match xb,yb with
            | c,d when (c <0.) && (d >0.) -> przedzial(neg_infinity,infinity)
            | c,d when (c>=0.) && (d >0.) -> dopelnienie(mux il1 il2,mun il3 il4)
            | c,d when (c <0.) && (d<=0.) -> dopelnienie(mux il3 il4,mun il1 il2)
            | _,_ -> przedzial(0.,0.)
           )
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
