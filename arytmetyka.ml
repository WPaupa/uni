type wartosc = 
  | Przedzial of float*float 
  | Dopelnienie of float*float
  | Pusty;;

let przedzial(x,y) =
  if (x=0.) && (y=0.) then Przedzial(-0.,0.) 
  else if x=0. then Przedzial(0.,y)
  else if y=0. then Przedzial(x,-0.)
  else Przedzial(x,y);;

let dopelnienie(x,y) =
  if (x>=y) then przedzial(neg_infinity,infinity) else 
  if x=0. then Dopelnienie(0.,y)
  else if y=0. then Dopelnienie(x,-0.)
  else Dopelnienie(x,y);;

let wartosc_od_do x y = przedzial(x,y);;
let wartosc_dokladnosc x p = wartosc_od_do
    (x -. ((p *. x) /. 100.)) 
    (x +. ((p *. x) /. 100.));;
let wartosc_dokladna x = przedzial(x,x);;

let mux x y =
  if Float.is_nan x then y
  else if Float.is_nan y then x
  else max x y;;

let mun x y =
  if Float.is_nan x then y
  else if Float.is_nan y then x
  else min x y;; 


let in_wartosc x y = 
  match x with
  | Przedzial(a,b) -> (a<=y) && (y<=b) 
  | Dopelnienie(a,b) -> (y<a) || (b<y)
  | Pusty -> false;;

let min_wartosc x =
  match x with
  | Przedzial(a,b) -> a
  | Dopelnienie(a,b) -> neg_infinity
  | Pusty -> infinity;;

let max_wartosc x =
  match x with
  | Przedzial(a,b) -> b
  | Dopelnienie(a,b) -> infinity
  | Pusty -> neg_infinity;;

let sr_wartosc x =
  match x with
  | Przedzial(a,b) ->
      (match a,b with
       | t,u when (t = neg_infinity) && (u=infinity) -> nan
       | t,_ when t = neg_infinity -> neg_infinity
       | _,t when t = infinity -> infinity
       | a,b -> (a+.b)/.2.
      )
  | Dopelnienie(a,b) -> nan 
  | Pusty -> nan;; 

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

let minus a b =
  match a with
  | Przedzial(xa,ya) ->
      (match b with 
       | Przedzial(xb,yb) -> przedzial(xa-.yb,ya-.xb)
       | Dopelnienie(xb,yb) -> dopelnienie(xa-.xb,ya-.yb)
       | Pusty -> Pusty
      )
  | Dopelnienie(xa,ya) ->
      (match b with 
       | Przedzial(xb,yb) -> dopelnienie(xb-.xa,yb-.ya)
       | Dopelnienie(xb,yb) -> dopelnienie((ya-.yb), (xa-.xb))
       |Pusty -> Pusty
      )
  | Pusty -> Pusty;; 

let razy a b =
  match a with
  | Przedzial(xa,ya) ->
      (match b with
       | Przedzial(xb,yb) ->
           let il1 = xa*.xb in
           let il2 = ya*.xb in
           let il3 = xa*.yb in
           let il4 = ya*.yb in
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
                                 (mun (ya/.xb) (ya/.yb), mux (xa/.xb) (xa/.yb))
       | Pusty -> Pusty
      ) 
  | Przedzial(xb,yb) when (xb <0.) && (yb >0.) ->
      (match a with 
       | Przedzial(xa,ya) -> if (xa=0.) && (ya=0.) then przedzial(0.,0.)
           else dopelnienie(mux (mun (ya/.xb) (ya/.yb)) (mun (xa/.xb) (xa/.yb)),
                            mun (mux (ya/.xb) (ya/.yb)) (mux (xa/.xb) (xa/.yb)))
       | Dopelnienie(xa,ya) -> 
           dopelnienie(mux (xa/.yb) (ya/.xb), mun (xa/.xb) (ya/.yb))
             (*jeśli xa>0 lub ya<0, to funkcja  dopełnienie zwróci
                                 przedzial(neg_infinity,infinity*) 
       (* jeśli xa=ya=0, to funkcja dopelnienie zwróci przedział pełny*)  
       | Pusty -> Pusty
      )
  | Przedzial(xb,yb) when (xb>=0.) && (yb>=0.) ->
      (match a with
       | Przedzial(xa,ya) -> przedzial 
                               (mun (xa/.yb) (xa/.xb), mux (ya/.xb) (ya/.yb))
       | Dopelnienie(xa,ya) -> dopelnienie
                                 (mun (ya/.xb) (ya/.yb), mux (xa/.xb) (xa/.yb))
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
  (*żeby pozbyć się warnów *)
  | Przedzial(_,_) -> Pusty
  | Dopelnienie(_,_) -> Pusty
  | Pusty -> Pusty;; 