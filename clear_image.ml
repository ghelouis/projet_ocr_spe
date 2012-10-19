let average = (0,0,0) ;;

let clear_image img = 
    (*coin supérieur gauche de l'image*)
    if i = 0 && j = 0 then
      for p = i to i+1 do
         for n = j to j+1 do
             (*on met les couleurs des 3 pixels autour de ce coin dans une
               liste*) 
             average = average + Sdlvideo.get_pixel_color img n p
         done
      done

    else
        (*coin supérieur droit*)
        if i = img.width && j = 0 then
            for p = i downto i-1 do 
                for n = j to j+1 do
                   average = average + Sdlvideo.get_pixel_color img n p
                done
            done
        else
               (*coin inférieur gauche*)
            if i = 0 && j = img.height then 
                for p = i to i+1 do 
                    for n = j downto j-1 do 
                       average = average + Sdlvideo.get_pixel_color img n p 
            else
                (*coin inférieur droit*)
                if i = img.width && j = img.height then
                   for p = i downto i-1 do
                      for n = j downto j-1 do
                         average = average + Sdlvideo.get_pixel_color img n p 
                else
                       (*cas où pas de problème de bords ou de coins:*)
                            for j=1 to img.height-2 do
                               for i=1 to img.width-2 do
                                (* on met les couleurs des 8 pixels autour dans une matrice*)	
                                   for p=i-1 to i+1 do 
                                     for n=j-1 to j+1 do
                                      average = average + Sdlvideo.get_pixel_color img n p
                                     done
                                   done
                               (*on fait la la moyenne des valeurs des couleurs contenues dans average*)
	                           average = average/8 ;;
                               (*on met la couleur moyenne obtenue dans le pixel fautif*)
                               Sdlvideo.put_pixel_color img i j average ;;
                               done
                            done
                            img;;
  
