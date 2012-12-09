(*binarization*)

let binarize_pixel(r,g,b) = match r with
  |r when r > 150 -> (255,255,255)
  |_ -> (0,0,0) ;;


let binarize_image img_begin img_end width height = 
  for y=0 to height -1 do
    for x=0 to width - 1 do
      let (a,b,c)=binarize_pixel(Sdlvideo.get_pixel_color img_begin x y) in
      Sdlvideo.put_pixel_color img_end x y (a,b,c)
    done ;
  done

















