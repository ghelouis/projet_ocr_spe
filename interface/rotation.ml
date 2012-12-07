(*PI*)
let pi = 4.0 *. atan 1.0;;

let deg_to_rad a = (pi *. a) /. 180.;;

(* Rotation Angle a : float*)
let rotation (x,y) (cx,cy) a =
  let a1 = deg_to_rad a in
  let x1 = (float_of_int(x)-.cx) *. cos a1 +. (float_of_int(y)-.cy) *. sin (a1) +. cx 
  and y1 = (float_of_int(y)-.cy) *. cos a1 +. (cx-.(float_of_int(x))) *. sin (a1) +. cy in
  (int_of_float(x1),int_of_float(y1));;
