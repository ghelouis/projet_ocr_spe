(* interface.ml - une interface graphique avec lablgtk *)

(* initialisation de GTK *)
let _ = GMain.init ()

(* fenêtre principale de l'application *)
let window = GWindow.window
  ~title:"Ocaml Recognition"
  ~position:`CENTER
  ~height:800
  ~width:1100 ()

(* hbox et vbox sont des conteneurs principaux, pour pouvoir insérer 
 * plusieurs widgets. En effet, les fenêtres (GtkWindow) ne peuvent 
 * contenir qu'un seul enfant. 
 * hbox : horizontal box
 * vbox: vertical box 
 * *)
let vbox = GPack.vbox 
  ~spacing:10
  ~border_width:10
  ~packing:window#add ()

let uprint msg () =
  print_endline msg;
  flush stdout

let hbox = GPack.hbox 
  ~spacing:10
  ~border_width:10
  ~packing:vbox#add ()

(* Insertion de barres de défilement. *)
let scroll = GBin.scrolled_window
  ~height:620
  ~hpolicy:`ALWAYS
  ~vpolicy:`ALWAYS
  ~packing:hbox#add ()
  
(* Zone de texte avec des barres de défilement. *)
let text =
  let scroll = GBin.scrolled_window
    ~hpolicy:`ALWAYS
    ~vpolicy:`ALWAYS
    ~shadow_type:`ETCHED_IN
    ~packing:hbox#add () in
  let txt = GText.view ~packing:scroll#add () in
  GtkSpell.attach
    ~lang:"en_FR" txt;
  txt#misc#modify_font_by_name "Monospace 10";
  txt 

(* fonction pour charger un fichier texte dans la zone de texte *)
(*let load file () =
  let ich = open_in file in
  let len = in_channel_length ich in
  let buf = Buffer.create len in
    Buffer.add_channel buf ich len;
    close_in ich;
    text#buffer#set_text (Buffer.contents buf)
*)

let set_text s () =
  text#buffer#set_text s

(* pour sauvegarder le texte dans un fichier *)
let save_aux file = 
  let och = open_out file in
  output_string och (text#buffer#get_text ());
  close_out och

(* popup qui demande à l'utilisateur le nom du fichier *)
let save file () =
  let title = "save as" in
  let uri =
    GToolbox.input_string ~title ~text:"extracted_text.txt" ~ok:"OK" "" in
  match uri with
  | None -> ()
  | Some uri -> save_aux uri


(* pour sauvegarder le texte dans un fichier *)(*
let save file () = 
  let dlg = GWindow.file_chooser_dialog
  ~action: `OPEN
  ~parent:window
  ~position:`CENTER_ON_PARENT
  ~destroy_with_parent:true () in
  dlg#add_button_stock `CANCEL `CANCEL;
  dlg#add_select_button_stock `SAVE `SAVE;
  if dlg#run () = `OPEN then
    Gaux.may uprint dlg#filename; dlg#misc#hide ()*)
  (*
  let och = open_out file in
  output_string och (text#buffer#get_text ());
  close_out och *)

(* Un conteneur spécialement conçu pour les boutons. *) 
let bbox = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~packing:(vbox#pack ~expand:false) ()

(*
(* GtkFileChooserDialog - Boîte de dialogue d'ouverture et d'enregistrement. *)
let action_button stock event action =
  let dlg = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~parent:window
    ~position:`CENTER_ON_PARENT
    ~destroy_with_parent:true () in
  dlg#add_button_stock `CANCEL `CANCEL;
  dlg#add_select_button_stock stock event;
  let btn = GButton.button ~stock ~packing:bbox#add () in
  let _ = GMisc.image ~stock ~packing:btn#set_image () in
  let _ = btn#connect#clicked (fun () ->
    if dlg#run () = `OPEN then Gaux.may action dlg#filename;
    dlg#misc#hide ())
  in
  btn

let open_button = action_button `OPEN `OPEN (Aux.load text)
let save_button = action_button `SAVE `SAVE (Aux.save text)
*)

(* on récupère l'image passée en argument *)
let get_img () = if Array.length(Sys.argv) < 2 then
    failwith "image missing"
  else
    Sys.argv.(1)

let current_img = ref (get_img ())

(* l'image affichée *)
let image = GMisc.image
  ~file: (get_img ())
  ~packing:scroll#add_with_viewport ()

(* met à jour l'image affichée *)
let update_img img = 
  image#set_file img;
  current_img := img

(* met à jour l'image depuis le bouton open_img_button *)
let set_img btn () = 
  Gaux.may image#set_file btn#filename;
  match btn#filename with
  | None -> ()
  | Some s -> current_img := s

(* bouton pour ouvrir une image *)
let open_img_button = 
  let btn = GFile.chooser_button
    ~title:"Choose an image"
    ~action:`OPEN
    ~packing:(bbox#pack ~expand:false) () in
  let _ = btn#connect#selection_changed ~callback:(set_img btn) in
  btn

(* GtkFontSelectionDialog - Sélection de la police. *)
let font_button =
  let dlg = GWindow.font_selection_dialog
    ~parent:window
    ~destroy_with_parent:true
    ~position:`CENTER_ON_PARENT () in
  let _ = dlg#ok_button#connect#clicked (fun () ->
    text#misc#modify_font_by_name dlg#selection#font_name)
  in
  let btn = GButton.button ~stock:`SELECT_FONT ~packing:bbox#add () in
  let _ = GMisc.image ~stock:`SELECT_FONT ~packing:btn#set_image () in
  let _ = btn#connect#clicked (fun () -> ignore (dlg#run ());
    dlg#misc#hide ())
  in
  btn

(* GtkColorSelectionDialog - Sélection de couleur de l'arrière-plan. *)
let color_picker =
  let dlg = GWindow.color_selection_dialog
    ~parent:window
    ~destroy_with_parent:true
    ~position:`CENTER_ON_PARENT () in
  let _ = dlg#ok_button#connect#clicked (fun () ->
    text#misc#modify_base [`NORMAL, `COLOR dlg#colorsel#color])
  in
  let btn = GButton.button ~label:"Background color" ~packing:bbox#add () in
  let _ = GMisc.image ~stock:`COLOR_PICKER ~packing:btn#set_image () in
  let _ =  btn#connect#clicked (fun () -> ignore (dlg#run ()); 
    dlg#misc#hide ()) 
  in
  btn

(* Boîte de dialogue "À propos..." *)
let about_button =
  let dlg = GWindow.about_dialog
    ~authors:["Amaterasu - Nono - Reynova - Guigui"]
    ~copyright:"Copyright © 2012-2013 Renaissance du Travail Totalitaire"
    ~version:"1.0"
    ~website:"http ://www.opticalcharacterrecognition.wordpress.com/"
    ~website_label:"Ocaml Recognition"
    ~position:`CENTER_ON_PARENT
    ~parent:window
    ~destroy_with_parent:true () in
  let btn = GButton.button ~stock:`ABOUT ~packing:bbox#add () in
  let _ = GMisc.image ~stock:`ABOUT ~packing:btn#set_image () in
  let _ = btn#connect#clicked (fun () -> ignore (dlg#run ()); dlg#misc#hide ())
  in
    btn

let quit_button =
  let btn = GButton.button
    ~stock:`QUIT
    ~packing:bbox#add ()
  in
  btn#connect#clicked ~callback:(GMain.Main.quit)

