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
let rec wait_key () =
  let e = Sdlevent.wait_event () in 
  match e with 
  Sdlevent.KEYDOWN _ -> ()
        | _ -> wait_key ()

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

let image2grey src dst =
  let (width,height) = get_dims src in 
  for j = 0 to (height-1) do
    for i = 0 to (width-1) do  
      let (x,y,z) = Sdlvideo.get_pixel_color src i j in
      let (a,b,c) = color2grey(x,y,z) in
       Sdlvideo.put_pixel_color dst i j (a,b,c)
    done;
  done

  (*main*)
let main () =
  begin
    (* + nous voulons un argument*)
    if Array.length(Sys.argv) < 2 then
      failwith "Il manque le nom du fichier";
    (* initialisation de SDL *)
    sdl_init ();
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* on recupere les dimensions *)
    let (w,h) = get_dims img in
    (* on cree la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    (* on affiche l'image*)
    show img display ;
    let new_image=Sdlvideo.create_RGB_surface_format img [] w h in
    image2grey img  new_image;
    show img display;
      
    (* wait_key ();
      
    show new_image  display;*)
    (* on attend une touche*)
    wait_key ();
    let binarized_image = Sdlvideo.create_RGB_surface_format img [] w h in
    Binarization.binarize_image img binarized_image w h ;
    show binarized_image display;
    wait_key();
    Clear_image.clear_image binarized_image;
    show binarized_image display;
    wait_key ();
(*
    (*ROTATION*)
   (* let rotated_image = 
      Sdlvideo.create_RGB_surface_format binarized_image [] w h in*)
    let rotated_image2 = 
      Sdlvideo.create_RGB_surface_format binarized_image [] w h in

   (* Rotation_picture.all_white rotated_image (w,h);
    Rotation_picture.rot_picture binarized_image rotated_image  (-5.3);
      
    wait_key();
    show rotated_image display;
    wait_key();*)

    let angle = Detection_rotation2.skew binarized_image in
    wait_key();

    Rotation_picture.all_white rotated_image2 (w,h);
    Rotation_picture.rot_picture binarized_image rotated_image2 (-.angle);
    wait_key();
      
    show rotated_image2 display;
    wait_key(); 
    (*END OF ROTATION*)

    (* RLSA *) 
    let rlsa_img = Sdlvideo.create_RGB_surface_format img [] w h in
      Segmentation.rlsa rotated_image2 w h rlsa_img;
    show rlsa_img display;
    wait_key();
    (* END OF RLSA *)*)

    (*Detection de lignes*)
    let display2 = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    let lines = Types.list2tab(Extraction.create_lines binarized_image) in (*
      for i = 0 to (Array.length lines) - 1 do
        show lines.(i).Types.imgL display2;
        wait_key();
      done;*)

      (*mise en evidence des caracteres*)
    let show_char_img = 
      Sdlvideo.create_RGB_surface_format binarized_image [] w h in
    Extraction.show_all_char binarized_image lines;
    show binarized_image display;

    wait_key ();

    (*on quitte*)
    exit 0
  end

let _ = main ()



