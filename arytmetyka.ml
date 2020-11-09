type wartosc = 
  | Przedzial of float*float 
  | Dopelnienie of float*float
  | Pusty;;


let wartosc_od_do x y = Przedzial(x,y);;
let wartosc_dokladnosc x p = wartosc_od_do
    (x -. ((p *. x) /. 100.)) 
    (x +. ((p *. x) /. 100.));;
let wartosc_dokladna x = Przedzial(x,x);;


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
      (match (a,b) with
       | t when t = (neg_infinity,infinity) -> nan
       | (t,_) when t = neg_infinity -> neg_infinity
       | (_,t) when t = infinity -> infinity
       | (a,b) -> (a+.b)/.2.
      )
  | Dopelnienie(a,b) -> nan 
  | Pusty -> nan;; 

let plus a b =
  match a with
  | Przedzial(xa,ya) -> 
      (match b with
       | Przedzial(xb,yb) -> Przedzial(xa+.xb,ya+.yb)
       | Dopelnienie(xb,yb) -> if (ya+.xb)<(xa+.yb)
           then Dopelnienie(ya+.xb,xa+.yb)
           else Przedzial(neg_infinity,infinity)
       | Pusty -> Pusty
      )
  | Dopelnienie(xa,ya) ->
      (match b with 
       | Przedzial(xb,yb) -> if (yb+.xa)<(xb+.ya)
           then Dopelnienie(yb+.xa,xb+.ya)
           else Przedzial(neg_infinity,infinity)
       | Dopelnienie(xb,yb) -> Dopelnienie((xa+.xb),min (xa+.yb) (ya+.xb))
       | Pusty -> Pusty
      )
  | Pusty -> Pusty;; 

let minus a b =
  match a with
  | Przedzial(xa,ya) ->
      (match b with 
       | Przedzial(xb,yb) -> Przedzial(xa-.yb,ya-.xb)
       | Dopelnienie(xb,yb) -> Dopelnienie(xa-.xb,ya-.yb)
       | Pusty -> Pusty
      )
  | Dopelnienie(xa,ya) ->
      (match b with 
       | Przedzial(xb,yb) -> Dopelnienie(xb-.xa,yb-.ya)
       | Dopelnienie(xb,yb) -> if (ya-.yb)<(xa-.xb) then Dopelnienie
               ((ya-.yb), (xa-.xb))
           else Przedzial(neg_infinity,infinity)
       |Pusty -> Pusty
      )
  | Pusty -> Pusty;;