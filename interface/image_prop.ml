
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info
  img).Sdlvideo.h) ;;

let get_height img = 
        (Sdlvideo.surface_info img).Sdlvideo.h;;
let get_width img = 
        (Sdlvideo.surface_info img).Sdlvideo.w;;

let new_image_ img width height= 
	Sdlvideo.create_RGB_surface_format img [] width height;;

