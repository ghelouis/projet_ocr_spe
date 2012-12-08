
(*Donne le nombre de pixels noirs dans une colonne de position x d'une image 
 * img*)
let c_darkness img nb =
  let black = ref 0 
  in for i = 0 to (Sdlvideo.surface_info img).Sdlvideo.h do
    if (Sdlvideo.get_pixel_color img nb i = (0,0,0))
    then black := !black + 1
  done;
  !black


  (*Prend une image en parametre, retourne la liste d'entiers correspondant au
   * nombre de pixels noirs par colonnes*)
let shadowY img =
  let rec shadowYrec img nb listResult =
    if nb < 0
    then listResult
    else (c_darkness img nb)::(shadowYrec img (nb-1) listResult)
  in shadowYrec img (Sdlvideo.surface_info img).Sdlvideo.w []


  (*Fait la moyenne float d'une liste d'int*)
let moyenne blackList =
  let rec mrec m length = function
    |[]    -> invalid_arg "Liste de noir vide"
    |e::[] -> (m *. length +.  (float)e) /. (length +. 1.)
    |e::l  -> mrec ((m *. length +. (float)e)/.(length +. 1.)) (length +. 1.) l
  in mrec 0. 0. blackList


  (*prend une image, et retourne une liste de chiffres binaires, chaque chiffre
   * correspond a une colonne, 1 si il y a du noir, 0 si c'est une ligne blanche*)

let binarize_col img =
  let blackList = shadowY img in
  let rec binRec m = function
    |[]   -> []
    |nb::l -> if ((float)nb > m /. 4.)
              then 0::(binRec m l)
              else 1::(binRec m l)
  in binRec (moyenne blackList) blackList


(*Cree un pixel*)

let create_char line start_x end_x =
  let img = line.Types.imgL in
  let ma _ = Array.init (Sdlvideo.surface_info img).Sdlvideo.h (fun _ -> 1) in
  let tab = Array.init (end_x - start_x) ma in
  let is_black x y = (0,0,0) = Sdlvideo.get_pixel_color img x y in
  for i = 0 to (Array.length tab) - 1 do
    for j = 0 to (Array.length tab.(0)) - 1 do
      if is_black i j then
        tab.(i).(j) <- 0;
    done;
  done;
  Types.newChar tab (line.Types.bLeft + start_x, line.Types.bSup)


  (*Prend l'image de la ligne, retourne une liste de caracteres 
   * grace a la liste binaire*)
let firstList_char line =
  let binList = binarize_col line.Types.imgL in
  let rec findNextChar listOfChar x = function
    |[]   -> listOfChar
    |1::l -> findNextChar listOfChar (x+1) l
    |0::l -> findEndOfChar listOfChar (x+1) x l
    |_ -> invalid_arg "firstListChar"
  and findEndOfChar listOfChar x start = function
    |[]   -> (create_char line start x)::listOfChar
    |0::l -> findEndOfChar listOfChar (x+1) start l
    |1::l -> findNextChar ((create_char line start x)::listOfChar) 
    (x+1) l
    |_ -> invalid_arg "firstListChar"
  in findNextChar [] 0 binList






(*Detection des lignes*)
(*let ligneDetec image = *)


(*Donne le nombre de pixels noirs dans une ligne de hauteur x d'une image img*)
let lineDarkness img nb =
  let black = ref 0 
  in for i = 0 to (Sdlvideo.surface_info img).Sdlvideo.w do
    if (Sdlvideo.get_pixel_color img i nb = (0,0,0))
    then black := !black + 1
  done;
  !black


  (*Prend une image en parametre, retourne la liste d'entiers correspondant au
   * nombre de pixels noirs par lignes*)
let shadowX img =
  let rec shadowXrec img nb listResult =
    if nb < 0
    then listResult
    else (lineDarkness img nb)::(shadowXrec img (nb-1) listResult)
  in shadowXrec img (Sdlvideo.surface_info img).Sdlvideo.h []


  (*prend une image, et retourne une liste de chiffres binaires, chaque chiffre
   * correspond a une ligne, 1 si il y a du noir, 0 si c'est une ligne blanche*)

let binarizeLines img =
  let blackList = shadowX img in
  let rec binRec m = function
    |[]   -> []
    |nb::l -> if ((float)nb > m /. 32.)
              then 0::(binRec m l)
              else 1::(binRec m l)
  in binRec (moyenne blackList) blackList

let invBin listBin =
  let rec inv l2 = function
    |[] -> l2
    |e::l1 -> inv (e::l2) l1
  in inv [] listBin

  (*Prend une liste de chiffres binaires et lui enleve les *)
let rec suppLostWhites = function
  |[] -> []
  |1::0::1::l -> 1::1::(suppLostWhites (1::l))
  |1::0::0::1::l -> 1::1::1::(suppLostWhites (1::l))
  |1::0::0::0::1::l -> 1::1::1::1::(suppLostWhites (1::l))
  |e::l -> e::(suppLostWhites l)


  (*Prend l'image, retourne une liste de lignes grace a la liste binaire*)
let firstListLines img =
  let binList = suppLostWhites (invBin(binarizeLines img)) and
  imgWidth = Image_prop.get_width img in
  let rec findNextLine listOfLines x = function
    |[]   -> listOfLines
    |1::l -> findNextLine listOfLines (x+1) l
    |0::l -> findEndOfLine listOfLines (x+1) x l
    |_ -> invalid_arg "firstListLines"
  and findEndOfLine listOfLines x start = function
    |[]   -> (Types.newline start (x-1) imgWidth img)::listOfLines
    |0::l -> findEndOfLine listOfLines (x+1) start l
    |1::l -> findNextLine ((Types.newline start x imgWidth img)::listOfLines) 
    (x+1) l
    |_ -> invalid_arg "firstListLines"
  in findNextLine [] 0 binList


  (*Calcul les largeurs de lignes et suprimme celles trop petites*)
let supressLittleLines linesList =
  let m = Types.moyenneL linesList in
  let rec sup_rec m = function
    |[] -> []
    |li::l -> if ((float)(Types.getH li)) *. 3. < m
              then sup_rec m l
              else li::(sup_rec m l)
  in sup_rec m linesList


  (*Cree la liste de lignes a partir de l'image de base, et remplis les images 
  correspondant a chaque ligne. Mais les bornes gauche et droite ne sont 
  pas detectee*)

let create_lines img =
  let lines = supressLittleLines (firstListLines img) in
  let rec set_all_img = function
    |[] -> ()
    |li::l -> Types.setImageL li img;
              li.Types.letters <- firstList_char li;
              set_all_img l
  in set_all_img lines;
  lines



let show_all_char img array_l =
  let rec make_frame img = function
    |[] -> ()
    |c::l -> 
      for x = 0 to (Array.length c.Types.tab) - 1 do
        Sdlvideo.put_pixel_color img (c.Types.pos_x + x) 
            c.Types.pos_y (255, 0, 255);
        Sdlvideo.put_pixel_color img (c.Types.pos_x + x)
            (c.Types.pos_y + Array.length c.Types.tab.(0)) (255, 0, 255)
      done;
      for y = 0 to (Array.length c.Types.tab.(0)) - 1 do
        Sdlvideo.put_pixel_color img c.Types.pos_x
            (c.Types.pos_y + y) (255, 0, 255);
        Sdlvideo.put_pixel_color img (c.Types.pos_x + Array.length c.Types.tab)
            (c.Types.pos_y + y) (255, 0, 255)
      done;
      make_frame img l in
  for i = 0 to (Array.length array_l) - 1 do
    make_frame img array_l.(i).Types.letters;
  done;
  (*img*)






  (*END*)
