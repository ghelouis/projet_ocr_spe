(*dimensions d'une image*)

let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info
  img).Sdlvideo.h) ;;


(* init de SDL *)       
let sdl_init () = 
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

  (* attendre une touche *)
(*let rec wait_key () =
  let e = Sdlevent.wait_event () in 
  match e with 
  Sdlevent.KEYDOWN _ -> ()
        | _ -> wait_key ()
*)
        (* show img dst affiche la surface de destinations (normalement a
        l'ecran *)

let show img dst =
  let d = Sdlvideo.display_format img in
  Sdlvideo.blit_surface d dst ();
  Sdlvideo.flip dst


let level (r,g,b) = (0.3*. (float) r +. 0.59*. (float) g +. 0.11*. (float) b)/.255.

let color2grey (r,g,b)=
  (int_of_float((level(r,g,b))*.255.),
int_of_float((level(r,g,b))*.255.),
int_of_float ((level(r,g,b))*.255.))

let image2grey src dst width height =
  for j = 0 to (height-1) do
    for i = 0 to (width-1) do  
      let (x,y,z) = Sdlvideo.get_pixel_color src i j in
      let (a,b,c) = color2grey(x,y,z) in
       Sdlvideo.put_pixel_color dst i j (a,b,c)
    done;
  done

(* applique la transformation désirée à l'image *)
let apply transfo img_ref s () =
  let img = Sdlloader.load_image !img_ref in
  let (w,h) = get_dims img in
  let transformed_img = Sdlvideo.create_RGB_surface_format img [] w h in
  transfo img transformed_img w h;
  Sdlvideo.save_BMP transformed_img s;
  img_ref := s;
  Interface.update_img s

(* applique la rotation *)
let apply_rot img_ref s () =
  let img = Sdlloader.load_image !img_ref in
  let (w,h) = get_dims img in
  let rotated_img = Sdlvideo.create_RGB_surface_format img [] w h in
  let angle = Detection_rotation2.skew img in
  Rotation_picture.all_white rotated_img (w,h);
  Rotation_picture.rot_picture img rotated_img (-.angle);
  Sdlvideo.save_BMP rotated_img s;
  img_ref := s;
  Interface.update_img s

(* applique toutes les fonctions *)
let apply_all img_ref () =
  begin
    let img = Sdlloader.load_image !img_ref in
    let (w,h) = get_dims img in
    let grey_img = Sdlvideo.create_RGB_surface_format img [] w h in
    let binarized_img = Sdlvideo.create_RGB_surface_format img [] w h in
    let rotated_img = Sdlvideo.create_RGB_surface_format img [] w h in
    image2grey img grey_img w h;
    Binarization.binarize_image grey_img binarized_img w h;
    let angle = Detection_rotation2.skew img in
    Rotation_picture.all_white rotated_img (w,h);
    Rotation_picture.rot_picture binarized_img rotated_img (-.angle);
    let s = "final.bmp" in
    Sdlvideo.save_BMP rotated_img s;
    img_ref := s;
    Interface.update_img s
  end
  
(* Toolbar - c'est d'ici qu'on peut appeler nos fonctions *)
let toolbar = GButton.toolbar
  ~orientation:`HORIZONTAL
  ~style:`ICONS
  ~packing:(Interface.vbox#pack ~expand:false) ()

let data = [`G; `B; `R; `SEG; `S; `A]

(* association des fonctions aux boutons *)
let _ =
  let current_img = ref (Interface.get_img ()) in
  let packing = toolbar#insert in
  List.iter (function
    | `S -> ignore (GButton.separator_tool_item ~packing ())
    | `G -> ignore (let btn = GButton.tool_button 
      ~label:"Grayscale" 
      ~packing () 
      in btn#connect#clicked 
      ~callback:(apply image2grey current_img "grey.bmp"))
    | `B -> ignore (let btn = GButton.tool_button 
      ~label:"Binarize" 
      ~packing () 
      in btn#connect#clicked 
      ~callback:(apply Binarization.binarize_image current_img "binarized.bmp"))
    | `R -> ignore (let btn = GButton.tool_button 
      ~label:"Rotate" 
      ~packing () 
      in btn#connect#clicked 
      ~callback:(apply_rot current_img "rotated.bmp"))
    | `SEG -> ignore (let btn = GButton.tool_button 
      ~label:"RLSA" 
      ~packing () 
      in btn#connect#clicked 
      ~callback:(apply Segmentation.rlsa current_img "rlsa.bmp"))
    | `A -> ignore (let btn = GButton.tool_button 
      ~label:"Apply all" 
      ~packing () 
      in btn#connect#clicked 
      ~callback:(apply_all current_img))
    | _ -> ()
  ) data 

let main () =
  begin
    let _ = Interface.window#connect#destroy ~callback:GMain.quit in
    Interface.window#show ();
    GMain.main ();
    sdl_init ();
  end

let _ = main ()


