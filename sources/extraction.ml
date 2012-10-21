
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
(* let nb = (Sdlvideo.surface_info img).Sdlvideo.h in*)
	let rec shadowXrec img nb listResult =
		if nb < 0
	    then listResult
      else (lineDarkness img nb)::(shadowXrec img (nb-1) listResult)
  in shadowXrec img (Sdlvideo.surface_info img).Sdlvideo.h []


        (*Fait la moyenne float d'une liste d'int*)
let moyenne blackList =
  let rec mrec m length = function
    |[]    -> invalid_arg "Liste de noir vide"
    |e::[] -> (m *. length +.  (float)e) /. (length +. 1.)
    |e::l  -> mrec ((m *. length +. (float)e)/.(length +. 1.)) (length +. 1.) l
  in mrec 0. 0. blackList


(*prend une image, et retourne une liste de chiffres binaires, chaque chiffre
 * correspond a une ligne, 1 si il y a du noir, 0 si c'est une ligne blanche*)

let binarizeLines img =
  let blackList = shadowX img in
  let rec binRec m = function
    |[]   -> []
    |nb::l -> if ((float)nb > m /. 8.)
    then 1::(binRec m l)
    else 0::(binRec m l)
  in binRec (moyenne blackList) blackList


(*Prend l'image, retourne une liste de lignes grace a la liste binaire*)
let firstListLines img =
	let binList = binarizeLines img and
			imgWidth = Image_prop.get_width img in
	let rec findNextLine listOfLines x = function
		|[]   -> listOfLines
		|0::l -> findNextLine listOfLines (x+1) l
		|1::l -> findEndOfLine listOfLines (x+1) x l
		|_ -> invalid_arg "firstListLines"
	and findEndOfLine listOfLines x start = function
		|[]   -> (Types.newline (x-1-start) (x-1) imgWidth img)::listOfLines
		|1::l -> findEndOfLine listOfLines (x+1) start l
		|0::l -> findNextLine ((Types.newline (x-start) x imgWidth img)::listOfLines) 
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
						set_all_img l
	in set_all_img lines;
	lines


(*Detecte les bornes gauches et droites de la ligne*)
 



        (*END*)
