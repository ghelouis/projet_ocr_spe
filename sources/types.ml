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


type ligne = (*une ligne est une liste de mots*)
  {
    mutable letters: tChar list;
    mutable bSup:int; (*borne superieure*)
    mutable bInf:int; (*borne inferieure*)
		mutable bLeft:int;
		mutable bRight:int;
  }

let newligne sup inf imgWidth = {letters=[]; bSup=sup; bInf=inf; 
															   bLeft= 0; bRight=imgWidth; }

  (*zone de texte : "paragraphe"*)
type textArea =
  {
    lignes:ligne list;
    (*"cadre" du paragraphe*)
    mutable xmin: int;
    mutable xmax: int;
    mutable ymin: int;
    mutable ymax: int;
    (*image de la zone de texte*)
    mutable imgA: Sdlvideo.surface;
  }

