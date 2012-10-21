# TP sdl

OCAML=ocamlopt
OCAMLFLAGS = -I +sdl
OCAMLLD = bigarray.cmxa sdl.cmxa sdlloader.cmxa
FILES = types.ml image_prop.ml extraction.ml rotation.ml rotation_picture.ml detection_rotation.ml clear_image.ml binarization.ml ocr_main.ml
SORTIE = -o ocr_main

tpsdl:
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} ${SORTIE} ${FILES}


clean:: 
	rm -f *~ *.o *.cm? ocr_main

# FIN



