(* segmentation.ml *)

(* Détermine les différentes régions de l'image 
 * en appliquant RLSA (Run Length Smoothing Algorithm). *)

(* applique f à tout les éléments blancs du tableau *)
let  map_white f tab =
  let out_tab = Array.create (Array.length tab) 0 in
    for i = 0 to Array.length tab - 1 do
      if tab.(i) != 0 then
        out_tab.(i) <- f tab i
      else
        out_tab.(i) <- tab.(i)
    done;
  out_tab

(* applique f à tout les éléments noirs du tableau *)
let  map_black f tab =
  let out_tab = Array.create (Array.length tab) 0 in
    for i = 0 to Array.length tab - 1 do
      if tab.(i) != 1 then
        out_tab.(i) <- f tab i
      else
        out_tab.(i) <- tab.(i)
    done;
  out_tab

(* applique rlsa à l'élément i d'un tableau tab avec un seuil s *)
let process s tab i =
  let s_neg = 0 - s in
    let rec adj_1s_right d = match d with
      | x when i + x > Array.length tab - 1 -> 0
      | x when x = s -> if tab.(i + s) = 1 then 1 else 0
      | x -> if tab.(i + x) = 1 then 1 + adj_1s_right (x + 1) else 0
    in
    let rec adj_1s_left d = match d with
      | x when i + x < 0 -> 0 
      | x when x = s_neg -> if tab.(i - s) = 1 then 1 else 0
      | x -> if tab.(i + x) = 1 then 1 + adj_1s_left (x - 1) else 0
    in
    if (adj_1s_left 0) + (adj_1s_right 0) > s + 1 then 1 else 0


(* pix_tab est un tableau de 0 et de 1 
 * qui représentent des pixels noirs et blancs *)
let rlsa_hori img w h dst =
  let pix_tab = Array.create w 0 in
    for y = 0 to h - 1 do
      (* remplissage du tableau à partir des pixels originaux *)
      for x = 0 to w - 1 do
        let (r,g,b) = Sdlvideo.get_pixel_color img x y in
          pix_tab.(x) <- (r / 255)
      done;
      (* applique un rlsa horizontal *)         
      let processed_tab = map_white (process 20) pix_tab in
      for x = 0 to w - 1 do 
        let c = processed_tab.(x) * 255 in
          Sdlvideo.put_pixel_color dst x y (c,c,c)
      done;
    done


let rlsa_verti img w h dst =
  let pix_tab = Array.create h 0 in
    for x = 0 to w - 1 do
      (* remplissage du tableau à partir des pixels originaux *)
      for y = 0 to h - 1 do
        let (r,g,b) = Sdlvideo.get_pixel_color img x y in
          pix_tab.(y) <- (r / 255)
      done;
      (* applique un rlsa vertical *)
      let processed_tab = map_white (process 20) pix_tab in
      for y = 0 to h - 1 do
        let c = processed_tab.(y) * 255 in
          Sdlvideo.put_pixel_color dst x y (c,c,c)
      done;
    done

(* efface les pixels noirs peu nombreux entourés de pixels blancs *)
let smooth tab i =
  let surrounding_white = ref 0 in
    for x = -80 to 80 do
      if i + x < Array.length tab - 1 && i + x > -1 then
        if tab.(i + x) = 1  && i != x then
          surrounding_white := !surrounding_white + 1
    done;
    if !surrounding_white > 80 then 1 else 0

(* lissage *)
let smoothing img w h dst =
  let pix_tab = Array.create w 0 in
    for y = 0 to h - 1 do
      (* remplissage du tableau à partir des pixels originaux *)
      for x = 0 to w - 1 do
        let (r,g,b) = Sdlvideo.get_pixel_color img x y in
          pix_tab.(x) <- (r / 255)
      done;
      (* on applique la fonction de lissage *)
      let processed_tab = map_black smooth pix_tab in
      for x = 0 to w - 1 do 
        let c = processed_tab.(x) * 255 in
          Sdlvideo.put_pixel_color dst x y (c,c,c)
      done;
    done

let rlsa img w h dst =
  let rlsa_hori_img = Sdlvideo.create_RGB_surface_format img [] w h in
  let rlsa_verti_img = Sdlvideo.create_RGB_surface_format img [] w h in
  begin
    (* rlsa horizontal *)    
    rlsa_hori img w h rlsa_hori_img;
    (* rlsa vertical sur l'image obtenue *)
    rlsa_verti rlsa_hori_img w h rlsa_verti_img;
    (* lissage léger sur l'image obtenue *)
    smoothing rlsa_verti_img w h dst;
  end

