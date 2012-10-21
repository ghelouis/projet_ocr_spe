let division_9 (x,y,z) = (x/9,y/9,z/9);; (*normal*)
let division_6 (x,y,z) = (x/6,y/6,z/6);; (*border*)
let division_4 (x,y,z) = (x/4,y/4,z/4);; (*corner*)
let add (x,y,z) (a,b,c) = (a+x, b+y, c+z);;

let left_border img i j = 
  let average = ref (0,0,0) in
      for p = i to (i+1) do 
   for n = j-1 to (j+1) do
    average := add !average (Sdlvideo.get_pixel_color img n p);
   done;
 done; 
 (division_6(!average));;


let right_border img i j = 
   let average = ref (0,0,0) in
       for p = i-1 to i do
    for n = j-1 to (j+1) do 
     average := add !average (Sdlvideo.get_pixel_color img n p);
    done;
 done;
 (division_6(!average));;



let down_border img i j=
     let average = ref (0,0,0) in
       for p = i-1 to (i+1) do 
       for n = j-1 to j  do 
         average := add !average (Sdlvideo.get_pixel_color img n p);
       done;
   done;
   (division_6(!average));;



let top_border img i j = 
   let average = ref (0,0,0) in
       for p = i-1 to (i+1) do
        for n = j to j+1 do 
          average := add !average (Sdlvideo.get_pixel_color img n p);
        done;
 done;
 (division_6(!average));;
             


let small_clean_image_8 img i j = 
        (*cas où pas de problème de bords ou de coins:*)	
           let average = ref (0,0,0) in
       for p=i-1 to (i+1) do 
            for n=j-1 to (j+1) do
              average := add !average (Sdlvideo.get_pixel_color img n p);
            done;
         done;
         (*on fait la la moyenne des valeurs des couleurs contenues dans average*)
         (*on met la couleur moyenne obtenue dans le pixel fautif*)
        (division_9(!average));;
     

let small_clean_image_top_L img i j =
  (*coin supérieur gauche de l'image*)
           let average = ref (0,0,0) in
        for p=i to (i+1) do
           for n=j to (j+1) do
             (*on met les couleurs des 3 pixels autour de ce coin dans une
               liste*) 
             average := add !average (Sdlvideo.get_pixel_color img n p);
           done;
          done;
           (division_4(!average));;
     

let small_clean_image_top_R img i j = 
    (*coin supérieur droit*)
     let average = ref (0,0,0) in
             for p=i downto (i-1) do 
                for n=j to (j+1) do
                   average := add !average (Sdlvideo.get_pixel_color img n p);
                done;
             done;
              (division_4(!average));;
     
let small_clean_image_down_L img i j = 
     (*coin inférieur gauche*)
        let average = ref (0,0,0) in
        for p=i to (i+1) do 
          for n=j downto (j-1) do 
            average := add !average (Sdlvideo.get_pixel_color img n p);
          done;
        done;
         (division_4(!average));;
     

let small_clean_image_down_R img i j =
   (*coin inférieur droit*)
  let average = ref (0,0,0) in
       
      for p=i downto (i-1) do
         for n=j downto (j-1) do
           average := add !average (Sdlvideo.get_pixel_color img n p);
        done;
      done;
       (division_4(!average));;
     

      
let clear_image img =
 let new_img = Image_prop.new_image_ img (Image_prop.get_width(img))
      (Image_prop.get_height(img)) in

          for j=1 to (Image_prop.get_height(img)-2) do
     for i=1 to (Image_prop.get_width(img)-2) do
       Sdlvideo.put_pixel_color new_img i j  (small_clean_image_8 img i j) ;
     done;
   done;
   
   (* top border*)
          for i = 1 to (Image_prop.get_width(img) -2) do 
             Sdlvideo.put_pixel_color new_img i 0 (top_border img i 0) ;
           done;
   (* down border*)

           for i=1 to (Image_prop.get_width(img)-2) do
             Sdlvideo.put_pixel_color new_img i (Image_prop.get_height(img)-1) 
             (down_border img i (Image_prop.get_height(img)-1));
           done;
     

    (* left border*)

           for j=1 to (Image_prop.get_height(img)-2) do
            Sdlvideo.put_pixel_color new_img 0 j (left_border img 0 j);
            done;
            
          

   (* right border*)

           for j = 1 to (Image_prop.get_height(img)-2) do
           Sdlvideo.put_pixel_color new_img (Image_prop.get_width(img)-1) j 
           (right_border img ((Image_prop.get_width(img))-1) j);
           done;

 (*c'est un coin !*)

               Sdlvideo.put_pixel_color new_img 0 0 (small_clean_image_top_L
               img 0 0);
               Sdlvideo.put_pixel_color new_img ((Image_prop.get_width(img))-1) 0
               ( small_clean_image_top_R img ((Image_prop.get_width(img))-1) 0);
           (*coin supérieur droit*)
           (*if (i=Image_prop.get_width(img)-1) && (j=0) 
           then
             for p=i downto (i-1) do 
                for n=j to (j+1) do
                        average := !average + Sdlvideo.get_pixel_color img n p;
                done;
             done;*)

                  Sdlvideo.put_pixel_color new_img 0
                 ((Image_prop.get_height(img))-1)
                  (small_clean_image_down_L img 0
                  ((Image_prop.get_height(img))-1));

                 (*coin inférieur gauche*)
                (*if (i=0) && (j=Image_prop.get_height(img)-1) 
                then 
                   for p=i to (i+1) do 
                    for n=j downto (j-1) do 
                  average := !average + Sdlvideo.get_pixel_color img n p ;
                    done;
                   done;*)
                        Sdlvideo.put_pixel_color new_img
                        ((Image_prop.get_width(img))-1)
                        ((Image_prop.get_height(img))-1) 
                        (small_clean_image_down_R img
                        ((Image_prop.get_width(img))-1)
                        ((Image_prop.get_height(img))-1));
                        (*coin inférieur droit*)
                  (*if (i=Image_prop.get_width(img)-1) && (j =
                        Image_prop.get_height(img)-1) 
                  then
                   for p=i downto (i-1) do
                      for n=j downto (j-1) do
                     average := !average + Sdlvideo.get_pixel_color img n p;
                      done;
                   done;*)






