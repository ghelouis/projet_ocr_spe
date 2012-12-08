let pow a = a*a;;

let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
(Sdlvideo.surface_info img).Sdlvideo.h) 

(*Somme des niveaux gris sur une ligne d'angle a*)

let rot_line (w,h) (x0,y0) ang img =
  let count = ref 0 in 
  let j = int_of_float(y0) in 
  for i = 0 to w-1 do
    let (x1,y1) = Rotation.rotation (i,j) (x0,y0) ang in
    if(x1>=0 && x1<w && y1>=0 && y1<h) then 
      let (r,g,b) = Sdlvideo.get_pixel_color img x1 y1 in
        if((r,g,b) = (0,0,0)) then 
          count := 1 + !count;
  done;
!count;;

(* Algo de PostL *) 

let premium_page (w,h) (x0,y0) img a =
  let premium = ref 0 in
  for n = 1 to h-2 do
    premium := !premium + 
pow((rot_line (w,h) (x0,float_of_int(n)) a img)-
(rot_line (w,h) (x0,float_of_int(n+1)) a img));
  done;  
  !premium;;

(*TRI*)
let rec max tab (refp,refa) =  
  match tab with 
  |[] -> refa
  |(p,a)::l when p > refp -> max l (p,a)
  |(p,a)::l -> max l (refp,refa);; 

let skew img =
  (*let tab = Array.create 302 (0,0) in*)
  let tab = ref [] in
  let (w,h) = get_dims img in  
  let (x0,y0) = (float_of_int(w)/.2.,float_of_int(h)/.2.) in
  let premium = ref 0 in
  let angle = ref (-15.) in 
  (*let i = ref 0 in*)
  while ( !angle <= 15. ) do
      begin
      premium := premium_page (w,h) (x0,y0) img !angle;
      (*tab.(i) <- (!premium,!angle);*)
      tab := (!premium,!angle)::!tab;
      angle := !angle +. 0.1;
      (*i := 1+!i;*)
      end 
  done;
  max !tab (0,0.)
