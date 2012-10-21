type tChar = (*Une lettre*)
  {
    mutable letter: char; (*lettre correspondante*)
    imgC: Sdlvideo.surface; (*image de la lettre *)
  }

(*Innitialise une nouvelle lettre, a caractere par defaut*)
let newChar image = {letter='a'; imgC=image} 


type word = (*Un mot = liste de lettres*)
  {
    mutable letters: tChar list;
    (*coordonnees ?*)
  }


type line = (*une ligne est une liste de mots*)
  {
    mutable letters: tChar list;
    mutable bSup:int; (*borne superieure*)
    mutable bInf:int; (*borne inferieure*)
		mutable bLeft:int;
		mutable bRight:int;
		mutable imgL: Sdlvideo.surface;
  }

let newline sup inf imgWidth img = 
		{letters = []; 
		 bSup = sup; 
		 bInf = inf; 
		 bLeft = 0; 
		 bRight = imgWidth;
     imgL = Sdlvideo.create_RGB_surface_format img [] imgWidth (inf - sup); }

let getH line = line.bInf - line.bSup

let moyenneL linesList =
	let rec mrec m length = function
		|[] -> 0.
		|li::[] -> (m *. length +. (float)(getH li)) /. (length +. 1.)
		|li::l -> mrec ((m *. length +. (float)(getH li)) /. (length +. 1.))
									 (length +. 1.) l
	in mrec 0. 0. linesList

let setImageL line img =
	line.imgL <- Sdlvideo.create_RGB_surface_format 
										img [] (line.bRight-line.bLeft) (line.bInf-line.bSup);
		for i = 0 to (line.bRight-line.bLeft-1) do
			for j = 0 to (line.bInf-line.bSup-1) do
				let pix_color = 
								Sdlvideo.get_pixel_color img (line.bLeft+i) (line.bInf+j) in
					Sdlvideo.put_pixel_color line.imgL i j pix_color
			done;
		done;



  (*zone de texte : "paragraphe"*)
type textArea =
  {
    lines:line list;
    (*"cadre" du paragraphe*)
    mutable xmin: int;
    mutable xmax: int;
    mutable ymin: int;
    mutable ymax: int;
    (*image de la zone de texte*)
    mutable imgA: Sdlvideo.surface;
  }



let list2tab l =
	if l = [] then [||]
	else 
	let rec nb_elts = function
 		|[] -> 0
		|e::l -> 1 + nb_elts l in 
	let n = nb_elts l in
	let give_first = function 
		|e::_ -> e 
		|[] -> failwith "pb creation tableau" in
	let new_tab = Array.init n (fun _ -> give_first l) in
	let put_elt tab pos e = tab.(pos) <- e in
	let rec put_all tab pos = function
		|[] -> ()
		|e::l -> put_elt tab pos e;
						 put_all tab (pos+1) l
		in
	put_all new_tab 0 l;
	new_tab
