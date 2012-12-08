(*let set_background img = 
  for i = 0 to img.width do 
   for j = 0 to img.height do 
           if (Sdlvideo.get_pixel_color img i j) ;;*)
let level (r,g,b) = 
        (0.3*. (float) r +. 0.59*. (float) g +. 0.11*. (float) b)/.255.;;


let color2grey (r,g,b)=
  (int_of_float((level(r,g,b))*.255.),
int_of_float((level(r,g,b))*.255.),
int_of_float ((level(r,g,b))*.255.));;

(*let n img = (Image_prop.get_height(img))*(Image_prop.get_width(img));;*)
 (*fonction qui va initialiser une partie du tableau*)

(*
       match histogram_array.(x,_) with
       |histogram_array.(x,_)  when histogram_array.(j,nb)
       = Sdlvideo.get_pixel_color img i j -> histogram_array.(j,nb+1)
       |histogram_array.(x,nb) -> (Sdlvideo.get_pixel_color img i j,nb)  
   (*on va trier les valeurs des couleurs de chaque case pour avoir un ordre croissant but : former histogramme -> seuil*)
     for y=0 to histogram_array.length-2 do
      if (histogram_array.(y) < histogram_array.(y+1)) then 
        ()histogram_array.(y)


       if Sdlvideo.get_pixel_color img O j = histogram_array.(j,_) then
         histogram_array.(j,nb) = histogram_array.(j,nb+1)
       else (*nouvelle intensité de couleur à rajouter dans histogramme*)
         histogram_array.(x,nb) = (Sdlvideo.get_pixel_color img i j,nb)
     *)

(*initialisation du tableau avec la couleur noire *)


(* ici l'IMAGE EST EN NIVEAU DE GRIS donc les 3 composantes sont = *)

(*let histogram_array img = Array.create
((Image_prop.get_height(img))*(Image_prop.get_width(img))) 0;;*)

 let histogram_calculus img (*histogram_array (n)*) = 
    (*parcours image*)
   let histogram_array = Array.create 255 0 in

    for i = 0 to (Image_prop.get_width(img)-1) do 
     for j = 0 to (Image_prop.get_height(img)-1) do
     (*niveau de gris = 0 -> mb de la case 0 augmente*)
       let (*niveau de gris*)(r,g,b) = (Sdlvideo.get_pixel_color img i j) in
       histogram_array.(r) <- histogram_array.(r)+1 ;
    (* histogram_array.(color2grey(Sdlvideo.get_pixel_color img i j)) <-
              histogram_array.level
      (color2grey(Sdlvideo.get_pixel_color img i j) + 1);*)
     done;
    done;
    histogram_array;;


(*let max histogram_calculus img(*histogram_array*) = 
        let max_array = ref 0 in
        for i = 0 to Array.length(histogram_array) - 1 do
             if (histogram_array.(i) > max_array) then 
                     max_array := i
        done;
        max_array ;;*)



(* on compare la variation encore entre cases i et i+1 et quand on trouve les
 * cases ou = max_variation -> on peut savoir quand fond/forme *)
(* calcul de la plus grande variation sur le schema : la fonction retournera
le niveau de gris i ou il va y avoir la pente elevee sur le prochain pixel*)

let comparison img =
  let new_histogram_array = (histogram_calculus img) in (*histogram_array = *)
  let break_point = ref 0 in (*seuil*)
  let max_variation = ref 0 in
    for i = 0 to Array.length(new_histogram_array) - 2 do
     let variation = ref (new_histogram_array.(i+1) /
        new_histogram_array.(i)) in  
           if !variation > !max_variation then 
             begin
                   max_variation := !variation;
                   variation := 0;
		               break_point := i ;
             end
    done;
        break_point;;

(* on peut maintenant distinguer le fond de la forme grace au seuil : 
        * le pixel au dessus du seuil deviendra blanc
        * le pixel au dessous du seuil deviendra noir *)
let break_point img = 
    for i = 0 to (Image_prop.get_width(img)-1) do 
     for j = 0 to (Image_prop.get_height(img)-1) do
         let (r1,g1,b1) = (comparison (img),comparison (img), comparison (img)) in
         let (r,g,b) = Sdlvideo.get_pixel_color img i j in
          if (r > !r1) then 
             (Sdlvideo.put_pixel_color img i j (255,255,255))
      else
             (Sdlvideo.put_pixel_color img i j (0,0,0))
     done
    done;;




      
