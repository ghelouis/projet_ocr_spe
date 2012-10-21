(* RLSA *)

(* applique f à tout les éléments du tableau *)
let  map f tab =
  for i = 0 to Array.length tab - 1 do
      if tab.(i) != 0 then
              tab.(i) <- f tab i
  done;
  tab

(* applique rlsa à l'élément i d'un tableau tab
 * avec un seuil s *)
let process s tab i =
    let s_neg = 0 - s in
    let rec adj_1s_right d = match d with
        | x when i + x > Array.length tab - 1 -> 0
        | x when x = s -> if tab.(i + s) = 1 then 1 else 0
        | x -> if tab.(i + x) = 1 then
                    1 + adj_1s_right (x + 1)
                else
                    0
    in
    let rec adj_1s_left d = match d with
        | x when i + x < 0 -> 0 
        | x when x = s_neg -> if tab.(i - s) = 1 then 1 else 0
        | x -> if tab.(i + x) = 1 then
                    1 + adj_1s_left (x - 1)
                else
                    0
    in
        if (adj_1s_left 0) + (adj_1s_right 0) > s + 1 then
            1
        else
            0

(* pix_tab est un tableau de 0 et de 1
 * qui représentent des pixels blancs et noirs *)
let rlsa_hori img w h dst =
  let pix_tab = Array.create w 0 in
    for y = 0 to h - 1 do

        (* remplissage du tableau à partir des pixels originaux *)
        for x = 0 to w - 1 do
          let (r,g,b) = Sdlvideo.get_pixel_color img x y in
            pix_tab.(x) <- (r / 255)
        done;

        (* applique un rlsa horizontal *)         
        let processed_tab = map (process 20) pix_tab in
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
      let processed_tab = map (process 20) pix_tab in
      for y = 0 to h - 1 do
          let c = processed_tab.(y) * 255 in
            Sdlvideo.put_pixel_color dst x y (c,c,c)
      done;
    done

(* ET logique pour les valeurs 255 et 0 *)
let logical_and a b =
    if a = 255 && b = 255 then 
        255 
    else 
        0

(* fusionne 2 images en appliquant un ET logique *)
let fusion w h img img2 dst =
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            let (r,g,b) = Sdlvideo.get_pixel_color img x y in
                let (r2,g2,b2) = Sdlvideo.get_pixel_color img2 x y in
                    let c = logical_and r r2 in
                        Sdlvideo.put_pixel_color dst x y (c,c,c)
        done;
    done

(* pour faire des tests avec un tableau de 1 et de 0 *)
(*let rlsa_test tab =
    for x = 0 to Array.length tab - 1 do
        let processed_tab = map (process 4) tab in
            print_int processed_tab.(x)
    done*)


(* EN COURS
(* donne le nombre de pixels noirs dans 
 * une ligne de hauteur h d'une image img de largeur w *)
let nb_of_black_px_hori img w h =
    let black_px = ref 0 in
        for x = 0 to w - 1 do
            if (Sdlvideo.get_pixel_color img x h) = (0,0,0) then 
                black_px := !black_px + 1
        done

(* donne le nombre de pixels noirs dans une colonne *)
let nb_of_black_px_verti img w h =
    let black_px = ref 0 in
        for y = 0 to h - 1 do
            if (Sdlvideo.get_pixel_color img w y) = (0,0,0) then
                black_px := !black_px + 1
        done

(* retourne un tableau de quadruplets (x,y,w,h)
 * x,y : coordonnées du coin gauche du bloc
 * w : largeur du bloc
 * h : hauteur du bloc *)
let extract_blocks img w h = 
    let rows_tab = Array.create 0 h and columns_tab = Array.create 0 w in
        let min = 100 in
            (* rempli la liste des lignes *)
            for y = 0 to h - 1 do
                let nb_of_black_px_hori img w y = n in
                    if n > min then
                        rows_tab.(y) <- (y,n)
                    else
                        rows_tab.(y) <- (y,0)
            done;
            (* rempli la liste des colonnes *)
            for x = 0 to w - 1 do
                let nb_of_black_px_verti img h x = n in
                    if n > 2 * min then
                        columns_tab.(x) <- (x,n)
                    else
                        columns_tab.(x) <- 0
            done;

            let coo = ref (0,0) and block_list = [] in
            for x = 0 to w - 1 do
                if column_tab.(x) != 0 then
                    for y = 0 to h - 1 do
                        if rows_tab.(y) != 0 then
                            coo := (x,y);

                done;
            done
*)

