(* pas encore terminé ni débugué *)
(* normalement applique un rlsa horizontal *)


(* RLSA *)

(* applique f à toute la liste l de longueur len *)
let rec map f l len = match len with
    | 0 -> l
    | i -> f l i :: map f l (i - 1) 

(* renvoie le n-ième élément de la liste l *)
let rec nth n l = match l with
    | [] -> raise (Failure "nth")
    | x::l -> if n <= 0 then x else nth (n - 1) l

(* renvoie 1 si l'élément courant est un 1 ou s'il est 
 * encadré à une distance de 5 ou moins d'un 1 *)
let process l i =
(* renvoie si un 1 se trouve à la distance d 
 * de la position i dans la liste l *)
    let rec is_near_1 d l i = match d with
        | -1 -> false
        | x -> if i + x < 0 || i + x > List.length l - 1 then 
            false || is_near_1 (abs x - 1) l i 
            else
                (nth (i + x) l) = 1 || is_near_1 (abs x - 1) l i
    in
        if is_near_1 5 l i && is_near_1 (-5) l i then 1 else 0

(* cette fonction applique un rlsa à un liste de pixels *)
let rlsa pix_list =
    let len = List.length pix_list in
        for i = 0 to len do
            map process pix_list len
        done

(* pix_list est une liste de 0 et de 1
 * qui représentent des pixels blancs et noirs *)
let rlsa img w h dst =
    for y = 0 to h - 1 do
        let pix_list = [] in
            for x = 0 to w - 1 do
                let (r,g,b) = Sdlvideo.get_pixel_color img x y in
                    (r / 255)::pix_list
            done
            for x = 0 to w - 1 do
                let processed_list = map process pix_list w pix_list in
                    Sdlvideo.put_pixel_color dst x y (nth x processed_list)
            done
    done


