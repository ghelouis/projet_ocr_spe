
(*Detection des lignes*)
(*let ligneDetec image = *)


(*Donne le nombre de pixels noirs dans une ligne de hauteur x d'une image img*)
let lineDarkness img nb =
        let black = ref 0 
        in for i = 0 to (Sdlvideo.surface_info img).Sdlvideo.w do
                        if
                                (Sdlvideo.get_pixel_color img nb i = (0,0,0))
                        then 
                                black := !black + 1
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

        (*END*)
