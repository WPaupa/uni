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



let centr = (0., 0.);;
let info = true;;

let mx_float = sqrt(max_float);;

if info then if info then print_endline "Correctness tests for zloz function:";;

if info then print_string "Rectangle #1 ";;

let a = prostokat centr (10., 10.);;

assert(a centr = 1);;
assert(a (5., 5.) = 1);;
assert(a (10., 10.) = 1);;
assert(a (10., 0.) = 1);;
assert(a (0., 10.) = 1);;
assert(a (10.1, 0.) = 0);;
assert(a (0., 10.1) = 0);;
assert(a (10.1, 10.1) = 0);;

let a = zloz (5., 0.) (5., 377.) a;;

assert(a centr = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;

let a = zloz (5., 0.) (5., 1.) a;;

assert(a centr = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;

if info then print_endline "OK";;

if info then print_string "Rectangle #2 ";;

let b = zloz (-7., -7.) (300., 300.) a;;

assert(b centr = 2);;
assert(b (0., 5.) = 3);;
assert(b (2.5, 2.5) = 2);;
assert(b (1., 2.) = 4);;
assert(b (2.5, 5.) = 3);;
assert(b (2.5, 6.) = 2);;
assert(b (2.5, 2.) = 0);;
assert(b (5., 5.) = 1);;
assert(b (5., 0.) = 0);;
assert(b (4., 2.) = 0);;
assert(b (7., 9.) = 0);;
assert(b (7., 2.) = 0);;
assert(b (5., 2.5) = 0);;
assert(b (10., 0.) = 0);;
assert(b (10., 10.) = 0);;
assert(b (10., 2.5) = 0);;

if info then print_endline "OK";;

if info then print_string "Rectangle #3 ";;

let c = zloz (-6., -6.) (-6.1, -6.1) a;;

assert(c centr = 2);;
assert(c (0., 5.) = 0);;
assert(c (2.5, 2.5) = 2);;
assert(c (1., 2.) = 0);;
assert(c (2.5, 5.) = 0);;
assert(c (2.5, 6.) = 0);;
assert(c (2.5, 2.) = 4);;
assert(c (5., 5.) = 1);;
assert(c (5., 0.) = 3);;
assert(c (4., 2.) = 4);;
assert(c (7., 9.) = 0);;
assert(c (7., 2.) = 2);;
assert(c (7., 3.8) = 2);;
assert(c (5., 2.5) = 3);;
assert(c (10., 0.) = 2);;
assert(c (10., 10.) = 0);;
assert(c (10., 2.5) = 2);;

if info then print_endline "OK";;

if info then print_string "Rectangle #4 ";;

let d = zloz (9., 5.) (4., 2.) c;;

assert(d centr = 0);;
assert(d (2.9, 1.9) = 0);;
assert(d (5., 5.) = 0);;
assert(d (7., 1.) = 2);;
assert(d (7.1, 1.45) = 2);;
assert(d (7.1, 1.5) = 4);;
assert(d (7., 3.) = 4);;
assert(d (7., 3.8) = 2);;
assert(d (7., 3.81) = 0);;
assert(d (5., 0.) = 3);;
assert(d (5., 0.5) = 3);;
assert(d (5., 1.) = 7);;
assert(d (5., 2.) = 7);;
assert(d (5., 3.) = 0);;
assert(d (5., 5.) = 0);;
assert(d (9., 5.) = 1);;
assert(d (4., 0.) = 4);;
assert(d (3., 0.) = 4);;
assert(d (2., 0.) = 8);;
assert(d (1., 0.) = 8);;
assert(d (0., 0.) = 0);;
assert(d (0.8, -0.2) = 4);;
assert(d (10., 3.) = 2);;
assert(d (4., 1.) = 8);;

if info then print_endline "OK";;

if info then print_string "Circle #1 ";;

let a = kolko (3., 3.) 7.;;

assert(a centr = 1);;
assert(a (3., 3.) = 1);;
assert(a (8., 7.5) = 1);;
assert(a (10., 3.) = 1);;
assert(a (3., 10.) = 1);;
assert(a (-4., 3.) = 1);;
assert(a (3., -4.) = 1);;
assert(a (10.1, 3.) = 0);;
assert(a (10., 3.1) = 0);;
assert(a (-4.1, 3.) = 0);;
assert(a (-3.9, 3.) = 1);;

let a = zloz (5., -10.) (5., 100.) a;;

assert(a centr = 1);;
assert(a (0.67, 0.) = 1);;
assert(a (0.68, 0.) = 2);;
assert(a (0.69, 0.69) = 2);;
assert(a (1., 0.) = 2);;
assert(a (2., 2.) = 2);;
assert(a (3., 0.) = 2);;
assert(a (5., 0.) = 1);;
assert(a (5.1, 0.) = 0);;
assert(a (3., 3.) = 2);;
assert(a (3., 10.) = 1);;
assert(a (-1., -1.) = 1);;
assert(a (7., 7.) = 0);;
assert(a (10., 0.) = 0);;

let a = zloz (5., 0.) (5., 0.01) a;;

assert(a centr = 1);;
assert(a (0.67, 0.) = 1);;
assert(a (0.68, 0.) = 2);;
assert(a (0.69, 0.69) = 2);;
assert(a (1., 0.) = 2);;
assert(a (2., 2.) = 2);;
assert(a (3., 0.) = 2);;
assert(a (5., 0.) = 1);;
assert(a (5.1, 0.) = 0);;
assert(a (3., 3.) = 2);;
assert(a (3., 10.) = 1);;
assert(a (-1., -1.) = 1);;
assert(a (7., 7.) = 0);;
assert(a (10., 0.) = 0);;

if info then print_endline "OK";;

if info then print_string "Circle #2 ";;

let a = zloz (1., 0.) (1., -1.) a;;

assert(a centr = 0);;
assert(a (1., 0.) = 2);;
assert(a (1.1, 0.) = 4);;
assert(a (5., 3.) = 2);;
assert(a (3., 3.) = 3);;
assert(a (7., 2.) = 0);;
assert(a (6., 3.) = 1);;
assert(a (6., 2.9) = 0);;
assert(a (6.1, 3.) = 0);;

if info then print_endline "OK";;

if info then print_string "Circle #3 ";;

let a = zloz (5., 10.) (1., 0.) a;;

assert(a centr = 0);;
assert(a (1., 0.) = 2);;
assert(a (2., 0.) = 3);;
assert(a (5., 0.) = 2);;
assert(a (6., 0.) = 0);;
assert(a (2., 2.) = 7);;
assert(a (3., 3.) = 7);;
assert(a (5., 5.) = 5);;
assert(a (6., 6.) = 2);;
assert(a (8., 8.) = 0);;
assert(a (4., 3.) = 3);;
assert(a (5., 3.) = 2);;
assert(a (6., 3.) = 1);;
assert(a (7., 3.) = 0);;
assert(a (1., -1.) = 1);;
assert(a (1., -3.) = 1);;
assert(a (1., -4.) = 0);;
assert(a (3., -1.) = 3);;
assert(a (3., -2.) = 3);;
assert(a (3., -3.) = 1);;
assert(a (3., -4.) = 1);;
assert(a (3., -5.) = 0);;

if info then print_endline "OK";;

if info then print_string "Circle #4 ";;

let a = zloz (1., 0.) (5., 10.) a;;

assert(a centr = 3);;
assert(a (1., 0.) = 2);;
assert(a (2., 0.) = 0);;
assert(a (5., 0.) = 0);;
assert(a (6., 0.) = 0);;
assert(a (2., 2.) = 0);;
assert(a (3., 3.) = 0);;
assert(a (5., 5.) = 0);;
assert(a (6., 6.) = 0);;
assert(a (8., 8.) = 0);;
assert(a (4., 3.) = 0);;
assert(a (5., 3.) = 0);;
assert(a (6., 3.) = 0);;
assert(a (7., 3.) = 0);;
assert(a (1., -1.) = 0);;
assert(a (1., -3.) = 0);;
assert(a (1., -4.) = 0);;
assert(a (3., -1.) = 0);;
assert(a (3., -2.) = 0);;
assert(a (3., -3.) = 0);;
assert(a (3., -4.) = 0);;
assert(a (3., -5.) = 0);;
assert(a (0., 4.) = 3);;
assert(a (0., 5.) = 1);;
assert(a (0., 6.) = 1);;
assert(a (0., 7.) = 0);;
assert(a (0., -1.) = 2);;
assert(a (0., -2.) = 0);;
assert(a (2., 3.) = 7);;
assert(a (1., 3.) = 5);;
assert(a (0., 3.) = 3);;
assert(a (-1., 3.) = 3);;
assert(a (-2., 3.) = 1);;
assert(a (-3., 3.) = 0);;
assert(a (1., 5.) = 5);;
assert(a (2., 5.) = 6);;
assert(a (3., 5.) = 3);;
assert(a (4., 5.) = 0);;
assert(a (3., 6.) = 6);;
assert(a (3., 4.) = 0);;
assert(a (3., 7.) = 6);;
assert(a (3., 8.) = 3);;
assert(a (3., 9.) = 1);;
assert(a (3., 10.) = 1);;
assert(a (3., 10.1) = 0);;

if info then print_endline "OK";;


if info then print_endline "Correctness tests for skladaj function:";;

if info then print_string "Rectangle ";;

let l = [((5., 0.), (5., 377.)); ((5., 0.), (5., 1.));
         ((-6., -6.), (-6.1, -6.1)); ((9., 5.), (4., 2.))];;

let a = prostokat centr (10., 10.);;

let a = skladaj l a;;

assert(a centr = 0);;
assert(a (2.9, 1.9) = 0);;
assert(a (5., 5.) = 0);;
assert(a (7., 1.) = 2);;
assert(a (7.1, 1.45) = 2);;
assert(a (7.1, 1.5) = 4);;
assert(a (7., 3.) = 4);;
assert(a (7., 3.8) = 2);;
assert(a (7., 3.81) = 0);;
assert(a (5., 0.) = 3);;
assert(a (5., 0.5) = 3);;
assert(a (5., 1.) = 7);;
assert(a (5., 2.) = 7);;
assert(a (5., 3.) = 0);;
assert(a (5., 5.) = 0);;
assert(a (9., 5.) = 1);;
assert(a (4., 0.) = 4);;
assert(a (3., 0.) = 4);;
assert(a (2., 0.) = 8);;
assert(a (1., 0.) = 8);;
assert(a (0., 0.) = 0);;
assert(a (0.8, -0.2) = 4);;
assert(a (10., 3.) = 2);;
assert(a (4., 1.) = 8);;

if info then print_endline "OK";;

if info then print_string "Circle ";;

let l = [((5., -10.), (5., 100.)); ((5., 0.), (5., 0.01));
         ((1., 0.), (1., -1.)); ((5., 10.), (1., 0.));
         ((1., 0.), (5., 10.))];;

let a = kolko (3., 3.) 7.;;

let a = skladaj l a;;

assert(a centr = 3);;
assert(a (1., 0.) = 2);;
assert(a (2., 0.) = 0);;
assert(a (5., 0.) = 0);;
assert(a (6., 0.) = 0);;
assert(a (2., 2.) = 0);;
assert(a (3., 3.) = 0);;
assert(a (5., 5.) = 0);;
assert(a (6., 6.) = 0);;
assert(a (8., 8.) = 0);;
assert(a (4., 3.) = 0);;
assert(a (5., 3.) = 0);;
assert(a (6., 3.) = 0);;
assert(a (7., 3.) = 0);;
assert(a (1., -1.) = 0);;
assert(a (1., -3.) = 0);;
assert(a (1., -4.) = 0);;
assert(a (3., -1.) = 0);;
assert(a (3., -2.) = 0);;
assert(a (3., -3.) = 0);;
assert(a (3., -4.) = 0);;
assert(a (3., -5.) = 0);;
assert(a (0., 4.) = 3);;
assert(a (0., 5.) = 1);;
assert(a (0., 6.) = 1);;
assert(a (0., 7.) = 0);;
assert(a (0., -1.) = 2);;
assert(a (0., -2.) = 0);;
assert(a (2., 3.) = 7);;
assert(a (1., 3.) = 5);;
assert(a (0., 3.) = 3);;
assert(a (-1., 3.) = 3);;
assert(a (-2., 3.) = 1);;
assert(a (-3., 3.) = 0);;
assert(a (1., 5.) = 5);;
assert(a (2., 5.) = 6);;
assert(a (3., 5.) = 3);;
assert(a (4., 5.) = 0);;
assert(a (3., 6.) = 6);;
assert(a (3., 4.) = 0);;
assert(a (3., 7.) = 6);;
assert(a (3., 8.) = 3);;
assert(a (3., 9.) = 1);;
assert(a (3., 10.) = 1);;
assert(a (3., 10.1) = 0);;

if info then print_endline "OK";;