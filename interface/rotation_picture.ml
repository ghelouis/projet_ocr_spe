(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)  

let all_white img (w,h) =
  for j = 0 to h-1 do
    for i = 0 to w-1 do
      Sdlvideo.put_pixel_color img i j (255,255,255)
    done
  done;;

(*Rotation image*)
let rot_picture img newimg a =
  let (w,h) = get_dims img in
  let (cx,cy) = (float_of_int(w)/.2.,float_of_int(h)/.2.) in
  (*let newimg = Sdlvideo.create_RGB_surface_format img [] w h in *)
  for j = 0 to h-1 do
    for i = 0 to w-1 do
       let color = Sdlvideo.get_pixel_color img i j in
       let (x,y) = Rotation.rotation (i,j) (cx,cy) a in 
       if(x>=0 && x<w && y>=0 && y<h) then  
         if( color <> (255,255,255)) then    
           Sdlvideo.put_pixel_color newimg x y color
    done;
  done

(*Rotation Image NEW GEN*)
(*let rot_picture_bi img newimg a =
  let (w,h) = get_dims img in
  let (cx,cy) = (float_of_int(w)/.2.,float_of_int(h)/.2.) in*)

