let division (x,y,z) = (x/9,y/9,z/9);;

let clear_image img =
        let average= ref(0,0,0) in

   for j=1 to (Image_prop.get_height(img)-2) do
     for i=1 to (Image_prop.get_width(img)-2) do
       small_clean_image_8(img) i j;
     done;
   done;
   
   (* top border*)
   if j = 0 then
          for i = 0 to (Image_prop.get_width(img) -1) do 
          done;

   (* down border*)

   if (j = Image_prop.get_height(img)-1) then
           for i=0 to (Image_prop.get_width(img)-1) do
           done;

    (* left border*)

   if i = 0 then
           for j=0 to (Image_prop.get_height(img)-1) do
           done;

   (* right border*)

   if (i = Image_prop.get_width(img)-1) then
           for j = 0 to (Image_prop.get_height(img)-1) do
           done;

 (*c'est un coin !*)

        if (i=0) && (j=0)
        then
                small_clean_image_top_L img i j;
          
          (*for p=i to (i+1) do
           for n=j to (j+1) do
             (*on met les couleurs des 3 pixels autour de ce coin dans une
               liste*) 
             average := !average + Sdlvideo.get_pixel_color img n p;
           done;
          done;*)

         else

           if (i=Image_prop.get_width(img)-1) && (j=0)
           then
             small_clean_image_top_R img i j;;
           (*coin supérieur droit*)
           (*if (i=Image_prop.get_width(img)-1) && (j=0) 
           then
             for p=i downto (i-1) do 
                for n=j to (j+1) do
                        average := !average + Sdlvideo.get_pixel_color img n p;
                done;
             done;*)
           else

              if (i=0) && (j=Image_prop.get_height(img)-1) 
              then 
                   small_clean_image_down_L img i j;;
                  (*coin inférieur gauche*)
                (*if (i=0) && (j=Image_prop.get_height(img)-1) 
                then 
                   for p=i to (i+1) do 
                    for n=j downto (j-1) do 
                            average := !average + Sdlvideo.get_pixel_color img n p ;
                    done;
                   done;*)
                else

                    if (i=Image_prop.get_width(img)-1) && (j =Image_prop.get_height(img)-1) 
                    then

                        small_clean_image_down_R img i j;;
                        (*coin inférieur droit*)
                  (*if (i=Image_prop.get_width(img)-1) && (j =
                        Image_prop.get_height(img)-1) 
                  then
                   for p=i downto (i-1) do
                      for n=j downto (j-1) do
                              average := !average + Sdlvideo.get_pixel_color img n p;
                      done;
                   done;*)
  
let small_clean_image_8 img i j = 
        (*cas où pas de problème de bords ou de coins:*)	
         for p=i-1 to (i+1) do 
            for n=j-1 to (j+1) do
              average := !average + Sdlvideo.get_pixel_color img n p;
            done;
         done;
         (*on fait la la moyenne des valeurs des couleurs contenues dans average*)
         (*on met la couleur moyenne obtenue dans le pixel fautif*)
        Sdlvideo.put_pixel_color img i j (division(!average));;
     

let small_clean_image_top_L img i j =
  (*coin supérieur gauche de l'image*)
          for p=i to (i+1) do
           for n=j to (j+1) do
             (*on met les couleurs des 3 pixels autour de ce coin dans une
               liste*) 
             average := !average + Sdlvideo.get_pixel_color img n p;
           done;
          done;;

let small_clean_image_top_R img i j = 
    (*coin supérieur droit*)
             for p=i downto (i-1) do 
                for n=j to (j+1) do
                        average := !average + Sdlvideo.get_pixel_color img n p;
                done;
             done;;

let small_clean_image_down_L img i j = 
     (*coin inférieur gauche*)
        for p=i to (i+1) do 
          for n=j downto (j-1) do 
            average := !average + Sdlvideo.get_pixel_color img n p ;
          done;
        done;;

let small_clean_image_down_R img =
   (*coin inférieur droit*)
      for p=i downto (i-1) do
         for n=j downto (j-1) do
           average := !average + Sdlvideo.get_pixel_color img n p;
         done;
      done;;
