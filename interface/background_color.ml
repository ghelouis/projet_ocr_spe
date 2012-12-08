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

let n img = (Image_prop.get_height(img))*(Image_prop.get_width(img));;
(*fonction qui va initialiser une partie du tableau*)
(*initialisation du tableau avec la couleur noire *)

let step=0;; (*seuil*)
let histogram_calculus img  =
    (*parcours image*)
  let histogram_array n = Array.create  (n img ) 0 in 
    for i = 0 to (Image_prop.get_width(img)-1) do 
     for j = 0 to (Image_prop.get_height(img)-1) do
     (*niveau de gris = 0 -> mb de la case 0 augmente*)
      histogram_array.(color2grey(Sdlvideo.get_pixel_color img i j)) <- histogram_array.
      (color2grey(Sdlvideo.get_pixel_color img i j) + 1);
     done
    done;;
















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
  

    
         

