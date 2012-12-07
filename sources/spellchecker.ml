type 'a maybe =
    Just of 'a
  | Nothing

type trie = {
  value : char;
  childs : trie list
}

let empty_trie value = { value = value; childs = [] }

let find_child trie elem = 
  let rec helper = function
    [] -> Nothing
  | hd::_ when hd.value = elem -> Just hd
  | _::tl -> helper tl
  in
  helper trie.childs

let add_child trie child = { value = trie.value;
                             childs = child::trie.childs }


let map_childs f trie = { value = trie.value;
                         childs = List.map f trie.childs }

let rec add_word trie = function
    [] -> add_child trie (empty_trie ' ')
  | hd::tl ->
    match find_child trie hd with
        Just _ -> 
          let helper child =
            if child.value = hd then
              add_word child tl
            else
              child
          in
          map_childs helper trie
      | Nothing -> 
        let child = add_word (empty_trie hd) tl in
        add_child trie child

let list_from_string s = 
  Array.to_list (Array.init (String.length s) (String.get s))

let explode s = 
  let rec exp i l = 
    if( i<0) then l else exp(i-1) (s.[i]::l) in
  exp (String.length s-1) []

let rec learn trie = function
    [] -> trie
  | hd::tl -> learn (add_word trie (list_from_string hd)) tl

let rec spellcheck trie = function
    [] -> (match find_child trie ' ' with
        Nothing -> false
      | Just _ -> true)
  | hd::tl -> (match find_child trie hd with
        Nothing -> false
      | Just child -> spellcheck child tl)
  
let rec suggest trie dist word acc =
  if dist < 0 then
    []
  else
    let helper child =
      if child.value = ' ' then
        match word with
            [] -> [List.rev acc]
          | hd::tl -> if (List.length tl) < dist then [List.rev acc] else []
      else
        match word with
            [] -> suggest child (dist-1) [] (child.value::acc)
          | hd::tl when hd = child.value -> suggest child dist tl 
          (child.value::acc)
          | hd::tl -> ((suggest child (dist-1) tl (child.value::acc)) @(suggest child (dist-1) (hd::tl) (child.value::acc)))
    in
    List.concat (List.map helper trie.childs)

type check_result =
    Correct
  | Incorrect of char list list

let check trie word =
  let w = list_from_string word in
  if spellcheck trie w then
    Correct
  else
    Incorrect (suggest trie 1 w [])

let rec load_trie trie chan =
  try
    load_trie (add_word trie (list_from_string (input_line chan))) chan
  with End_of_file ->
    trie

let load_trie_from_file file =
  let chan = open_in file in
  let trie = load_trie (empty_trie ' ') chan in
  trie

let rec trie_size trie = match trie.childs with
    [] -> 0
  | childs -> List.fold_left (fun last child -> last + (trie_size child)) 1 childs
  
let implode l = 
  let res = String.create (List.length l) in 
  let rec imp i = function 
    |[] ->  res
    |c::l -> res.[i] <- c;
    imp (i+1) l in
  imp 0 l;;

let rec concat l new_list= 
  match l with 
  |[] -> new_list
  |a::l1 -> concat l1 ((implode a)::new_list);;  

let corrected word = 
  let trie = load_trie_from_file "Dico_en" in 
  match check trie word with 
  |Correct ->  word;
  |Incorrect l -> List.hd(concat l []);;

(*******************************************************************)
(*USE Spellchecker.corrected *MOT_A_CORRIGER*                      *)
(*******************************************************************)
