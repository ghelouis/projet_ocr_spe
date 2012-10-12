# TP sdl

OCAML=ocamlopt
OCAMLFLAGS = -I +sdl
OCAMLLD = bigarray.cmxa sdl.cmxa sdlloader.cmxa
FILES = binarization.ml ocr_main.ml
SORTIE = -o ocr_main

tpsdl:
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} ${SORTIE} ${FILES}


clean:: 
	rm -f *~ *.o *.cm? ocr_main

# FIN



