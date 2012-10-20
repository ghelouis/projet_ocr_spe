
(*Detection des lignes*)
(*let ligneDetec image = *)


(*Donne le nombre de pixels noirs dans une ligne de hauteur x d'une image img*)
let lineDarkness img nb =
  let black = ref 0 
  in for i = 0 to (Sdlvideo.surface_info img).Sdlvideo.w do
	     if (Sdlvideo.get_pixel_color img nb i = (0,0,0))
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

let binarizeLignes img =
  let blackList = shadowX img in
  let rec binRec m = function
    |[]   -> []
    |nb::l -> if ((float)nb > m /. 8.)
    then 1::(binRec m l)
    else 0::(binRec m l)
  in binRec (moyenne blackList) blackList





        (*END*)
