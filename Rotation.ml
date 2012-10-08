(*Module Detection_Rotation*)

(* Soit n la ligne de l'histogramme*)

(* point de rotation =(largeur/2, n) *)


let pi = 4.0 *. atan 1.0;;

let deg_to_rad a = (pi *. a) /. 180.;;

(* Rotation Angle a : float*)
let rotation(x,y) a =
  let a1 = deg_to_rad a in
  let x1 = float_of_int(x) *. cos a1 -. float_of_int(y) *. sin a1 
  and y1 = float_of_int(y) *. cos a1 +. float_of_int(x) *. sin a1 in
  (int_of_float(x1),int_of_float(y1));;

(*Translation*)	
let translation(x,y) (tx,ty)=
  let x1 = x - tx and y1 = y - ty in
  (x1,y1);;		

(*Translation => Origin => Rotation => Translation*)
(* origin =  et largeur /2 vers la droite*)

let rot_trans (x,y) (x0,y0) a =
  let (newx,newy) = translation(x,y) (x0,y0) in
  let (newx,newy) = rotation(newx,newy) a in
  let (newx,newy) = translation(newx,newy) (-x0,-y0) in
  (newx,newy);;

(*Translation de n vers le bas*)

let rot_foreach_angle h w a =
  let middle = w/2
  and weight = ref w 
  and count = ref 0 
  and i = ref (-10.) in
  while !i < 10.0 do 
    i := !i +. 0.1;
(*count := sdl rot_translation *)
    weight := !weight + 1;
  done;
!count;;
  
(*Pour chaque point récupération I => histogramme*) 


let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h);;
(*Tourner une image*)
(*let rot_picture picture =*)
