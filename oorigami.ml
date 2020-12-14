type point = float * float;;
type kartka = point -> int;; 

let eps = 0.0000001;;
let (<=) x y = if (x)<=(y+.eps) then true else false;; 
let (=.) x y = if (x <= y) && (y <= x) then true else false;;

let prostokat (x1,y1) (x2,y2) (x,y) = 
  if ((x1<=x) && (x<=x2)) && ((y1<=y) && (y<=y2)) then 1 else 0;;

let kolko (xs,ys) r (x,y) =
  if ((x-.xs)*.(x-.xs)) +. ((y-.ys)*.(y-.ys)) <= r *. r then 1 else 0 ;;

let cross (ux,uy) (vx,vy) = (uy*.vx)-.(ux*.vy);;
let dot (ux,uy) (vx,vy) = (ux*.vx)+.(uy*.vy);; 
let mult alfa (ux,uy) = (alfa*.ux,alfa*.uy);;
let len (ux,uy) = sqrt((ux*.ux)+.(uy*.uy));; 
let vec (x1,y1) (x2,y2) = (x2-.x1,y2-.y1);;
let vers p1 p2 = mult (1./.(len (vec p1 p2))) (vec p1 p2) ;;
let plus (ux,uy) (vx,vy) = ((ux+.vx),(uy+.vy));;


let rec zloz p1 p2 f p = 
  if cross (vec p1 p) (vec p p2)=.0.
  then f p 
  else if cross (vec p1 p) (vec p p2)<0.
  then 0
  else 
    let n = vers p1 p2 in
    let v = plus (vec p p1) (mult (-2.*.(dot (vec p p1) n)) n)
    in (f (plus p1 v))+(f p);; 

let skladaj lst f = List.fold_left (fun f (p1,p2) -> zloz p1 p2 f) f lst;; 