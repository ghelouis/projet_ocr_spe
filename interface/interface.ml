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

(* Menu *)
let _ =
  (* container for the menus *)
  let menu_bar = GMenu.menu_bar
    ~height:10
    ~packing:vbox#add ()
  in
  (* container for the menu items *)
  let file_menu = GMenu.menu () in
  (* menu items *)
  let item_open = GMenu.menu_item ~label:"Open" ~packing:file_menu#append () in
  let item_save = GMenu.menu_item ~label:"Save" ~packing:file_menu#append () in
  let item_quit = GMenu.menu_item ~label:"Quit" ~packing:file_menu#append () in
  let file_item = GMenu.menu_item ~label:"File" () in
  let _ = item_open#connect#activate ~callback:(uprint "Open") in
  let _ = item_save#connect#activate ~callback:(uprint "Save") in
  let _ = item_quit#connect#activate ~callback:GMain.Main.quit in
    file_item#set_submenu file_menu;
    menu_bar#append file_item

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

(* on récupère l'image passée en argument *)
let get_img () = if Array.length(Sys.argv) < 2 then
    failwith "image missing"
  else
    Sys.argv.(1)

let image = GMisc.image
  ~file: (get_img ())
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



(*
let dialog  =
  let dlg : [`DELETE_EVENT] = GWindow.file_chooser_dialog ~action:`SAVE () in
  dlg#add_button_stock `CANCEL `CANCEL;
  dlg#add_select_button_stock `SAVE `SAVE;
  dlg

let may_save () =
  if dialog#run () = `SAVE then Gaux.may print_endline dialog#filename;
  dialog#misc#hide ()

let save_button =
  let btn = GButton.button ~stock:`SAVE () in
  btn#connect#clicked may_save;
  btn
*)



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

let print_file btn () = Gaux.may print_endline btn#filename
let set_img btn () = Gaux.may image#set_file btn#filename

(* bouton pour ouvrir une image *)
let open_img_button = 
  let btn = GFile.chooser_button
    ~action:`OPEN
    ~packing:(bbox#pack ~expand:false) () in
  let _ = btn#connect#selection_changed ~callback:(set_img btn) in
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

let toolbar = GButton.toolbar
  ~orientation:`VERTICAL
  ~style:`ICONS
  ~packing:(hbox#pack ~expand:false) ()

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

(*
let main () =
  begin
    let _ = window#connect#destroy ~callback:GMain.quit in
    window#show ();
    GMain.main ();
  end

let _ = main ()
*)
