(* interface.ml - une interface graphique avec lablgtk *)

(* module pour faciliter le chargement et la sauvegarde des fichiers *)
module Aux =
  struct
    let load (text : GText.view) file =
      let ich = open_in file in
      let len = in_channel_length ich in
      let buf = Buffer.create len in
      Buffer.add_channel buf ich len;
      close_in ich;
      text#buffer#set_text (Buffer.contents buf)

    let save (text : GText.view)
    file = let och = open_out file in
    output_string och (text#buffer#get_text ());
    close_out och
  end


(* initialisation de GTK *)
let _ = GMain.init ()

(* fenêtre principale de l'application *)
let window = GWindow.window
  ~title:"Ocaml Recognition"
  ~position:`CENTER
  ~height:500
  ~width:700 ()

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

let hbox = GPack.hbox 
  ~spacing:5
  ~border_width:5
  ~packing:vbox#add ()

(* Insertion de barres de défilement. *)
let scroll = GBin.scrolled_window
  ~height:200
  ~hpolicy:`ALWAYS
  ~vpolicy:`ALWAYS
  ~packing:hbox#add ()

let help_message () = print_endline "Cliquez sur \"Quitter\" pour quitter"

let image = GMisc.image
  ~file: "ocr-test.jpg"
  ~packing:scroll#add_with_viewport ()
  
(* Zone de texte avec des barres de défilement. *)
let text =
  let scroll = GBin.scrolled_window
    ~hpolicy:`ALWAYS
    ~vpolicy:`ALWAYS
    ~shadow_type:`ETCHED_IN
    ~packing:hbox#add () in
  let txt = GText.view ~packing:scroll#add () in
  txt#misc#modify_font_by_name "Monospace 10";
  txt 

(* Un conteneur spécialement conçu pour les boutons. *) 
let bbox = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~packing:(vbox#pack ~expand:false) ()


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

(* GtkColorSelectionDialog - Sélection de couleur. *)
let color_picker =
  let dlg = GWindow.color_selection_dialog
    ~parent:window
    ~destroy_with_parent:true
    ~position:`CENTER_ON_PARENT () in
  let _ = dlg#ok_button#connect#clicked (fun () ->
    text#misc#modify_base [`NORMAL, `COLOR dlg#colorsel#color])
  in
  let btn = GButton.button ~label:"Arrière-plan" ~packing:bbox#add () in
  let _ = GMisc.image ~stock:`COLOR_PICKER ~packing:btn#set_image () in
  let _ =  btn#connect#clicked (fun () -> ignore (dlg#run ()); 
    dlg#misc#hide ()) 
  in
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

(*
(* pour afficher le nom du fichier ouvert sur le bouton *)
let may_print btn () = Gaux.may print_endline btn#filename

(* bouton pour ouvrir un fichier *)
let open_button = 
  let btn = GFile.chooser_button
    ~action:`OPEN
    ~packing:(bbox#pack ~expand:false) () in
  let _ = btn#connect#selection_changed (may_print btn) in
  btn
*)

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

(*
let toolbar = GButton.toolbar
  ~orientation:`HORIZONTAL
  ~style:`ICONS
  ~packing:(vbox#pack ~expand:false) ()

let days =
  let menu = GMenu.menu () in
  List.iter (fun label -> ignore (GMenu.menu_item ~label ~packing:menu#add ()))
      ["Lundi"; "Mardi"; "Mercredi"; "Jeudi"; "Vendredi"];
  menu

let data = [`B `NEW; `B `OPEN; `B `SAVE; `S; `B `CUT; `B `COPY; `B `PASTE; `S;
`T "Bascule"; `S; `M days]

let _ =
  let packing = toolbar#insert in
  List.iter (function
    | `S -> ignore (GButton.separator_tool_item ~packing ())
    | `B stock -> ignore (GButton.tool_button ~stock ~packing ())
    | `T label -> ignore (GButton.toggle_tool_button ~label ~packing ())
    | `M menu -> ignore (GButton.menu_tool_button ~label:"Foo" ~menu ~packing ())
    | _ -> ()
  ) data 
  *)

let main () =
  begin
    let _ = window#connect#destroy ~callback:GMain.quit in
    window#show ();
    GMain.main ();
  end

let _ = main ()
