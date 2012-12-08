# TP sdl

OCAML=ocamlopt
OCAMLFLAGS = -I +sdl -I +lablgtk2
OCAMLLD = bigarray.cmxa sdl.cmxa sdlloader.cmxa lablgtk.cmxa lablgtkspell.cmxa
FILES = types.ml image_prop.ml extraction.ml rotation.ml rotation_picture.ml detection_rotation2.ml clear_image.ml binarization.ml segmentation.ml interface.ml ocr_main.ml
SORTIE = -o ocr_main

tpsdl:
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} ${SORTIE} ${FILES}


clean:: 
	rm -f *.bmp *~ *.o *.cm? ocr_main

# FIN



