let valid_chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" 
;;

(* zipper dont les éléments sont de type 'a et l'élément courrant de type 'b *)

type ('a,'b) zipper =
  { before:  'a list (* liste des éléments précédents dans l'ordre inversé *)
    ; current: 'b
    ; after:   'a list (* liste des éléments précédents dans l'ordre *)
    ; pos:     int (* position courrante *)
    }
;;

type cursor = Cursor;;

type line   = (char, cursor) zipper;;

type buffer = (line, line) zipper;;

type action =
    | Up
    | Left
    | Down
    | Right
    | Char of char
    | Newline
    | Delete
    | Backspace
;;

let empty_line = { before = []; current = Cursor; after = []; pos = 0 };;

let empty_buf  = { before = []; current = empty_line; after = []; pos = 0 };;

(** NE RIEN MODIFIER AVANT CE POINT **)

let sx = 800 (* LARGEUR DE LA FENETRE GRAPHIQUE EN POINTS *)

let sy = 600  (* HAUTEUR DE LA FENETRE GRAPHIQUE EN POINTS *)

(** Partie 1 **)

(** 
 * Récupère l'élément courant d'un zipper
 * @param z Zipper
 * @return Élément courant
 *)

let get_current z = z.current

(** 
 * Récupère l'élément courant d'un zipper
 * @param z Zipper
 * @return Élément courant
 *)
let get_pos z = z.pos


let fold_zipper f g acc0 z =
  let acc1 = List.fold_left f acc0 (List.rev z.before) in
  let acc2 = g acc1 z.current in
  List.fold_left g acc2 z.after

(** 
 * Met à jour l'élément courant d'un zipper
 * @param f Fonction de transformation
 * @param z Zipper original
 * @return Nouveau zipper avec élément courant transformé
 *)
let update_with f z = { z with current = f z.current }

(**Partie 2**)

  (** 
 * Déplace le curseur vers la gauche dans une ligne
 * @param l La ligne à modifier
 * @return Nouvelle ligne avec curseur déplacé
 * @raises Failure si on est déjà en début de ligne
 *)
 
let line_move_left l =
  match l.before with
  | [] -> l (* Ne rien faire si début de ligne *)
  | hd::tl ->
      { before = tl;
        current = Cursor;
        after = hd :: l.after;
        pos = get_pos l - 1 }

(** 
 * Déplace le curseur vers la droite dans une ligne
 * @param l La ligne à modifier
 * @return Nouvelle ligne avec curseur déplacé
 * @raises Failure si on est déjà en fin de ligne
 *)
let line_move_right l =
  match l.after with
  | [] -> l (* Ne rien faire si fin de ligne *)
  | hd::tl ->
      { before = hd :: l.before;
        current = Cursor;
        after = tl;
        pos = get_pos l + 1 }

(** 
 * Déplace le curseur vers le haut dans le buffer
 * @param buf Le buffer à modifier
 * @return Nouveau buffer avec curseur déplacé
 * @raises Failure si on est déjà sur la première ligne
 *)

let buffer_move_up buf =
  match buf.before with
  | [] -> buf (* Ne rien faire si première ligne *)
  | hd::tl ->
      let new_current = hd in
      let new_after = get_current buf :: buf.after in
      { before = tl;
        current = new_current;
        after = new_after;
        pos = get_pos buf - 1 }

(** 
 * Déplace le curseur vers le bas dans le buffer
 * @param buf Le buffer à modifier
 * @return Nouveau buffer avec curseur déplacé
 * @raises Failure si on est déjà sur la dernière ligne
 *)
let buffer_move_down buf =
  match buf.after with
  | [] -> buf (* Ne rien faire si dernière ligne *)
  | hd::tl ->
      let new_current = hd in
      let new_before = get_current buf :: buf.before in
      { before = new_before;
        current = new_current;
        after = tl;
        pos = get_pos buf + 1 }


(* Déplacements horizontaux *)

(*
let move_left buf =
  { buf with current = line_move_left (get_current buf) }
   *)

let move_right buf =
  { buf with current = line_move_right (get_current buf) }


(* Déplacements verticaux *)
let move_up = buffer_move_up

let move_down = buffer_move_down

(* Insertion *)

(** 
 * Insère un caractère à la position courante
 * @param ch Caractère à insérer
 * @param buf Buffer actuel
 * @return Nouveau buffer avec caractère inséré
 *)
 let insert_char ch buf =
    let update_line line = {
      before = ch :: line.before;
      current = Cursor;
      after = line.after;
      pos = get_pos line + 1
    } in
    update_with update_line buf

(* Suppressions *)

(** 
 * Supprime le caractère courant
 * @param buf Buffer actuel
 * @return Nouveau buffer avec caractère supprimé
 *)
 
 let do_suppr buf =
  let current = get_current buf in
  match current.after with
  | [] -> buf
  | _::tl ->
      let update_line line = { line with after = tl } in
      update_with update_line buf

(** 
 * Supprime le caractère avant le curseur
 * @param buf Buffer actuel
 * @return Nouveau buffer avec caractère supprimé
 *)

 (*
let do_backspace buf =
  let current = get_current buf in
  match current.before with
  | [] -> buf
  | _::tl ->
      let update_line line = { 
        line with 
        before = tl;
        pos = line.pos - 1 
      } in
      update_with update_line buf
*)

(* Nouvelle ligne *)

(** 
 * Crée une nouvelle ligne dans le buffer
 * @param buf Buffer actuel
 * @return Nouveau buffer avec nouvelle ligne
 *)

(*
let create_newline buf =
  let cl = buf.current in
  let new_line = {
    before = [];
    current = Cursor;
    after = cl.after;
    pos = 0
  } in
  {
    before = buf.current :: buf.before;
    current = new_line;
    after = buf.after;
    pos = buf.pos + 1
  }
*)

(**Partie 3*)

(**
 * Déplace le curseur vers la gauche dans le buffer
 * @param buf Le buffer à modifier
 * @return Nouveau buffer avec curseur déplacé
 * @raises Failure si on est déjà en début de ligne
 *)

let move_left buf =
  let current_line = buf.current in
  if current_line.before = [] then (* Début de la ligne courante *)
    match buf.before with
    | [] -> buf (* Déjà sur la première ligne, rien à faire *)
    | prev_line :: rest_before -> (* Il y a une ligne au-dessus *)
        let new_current = {
          before = prev_line.before;
          current = Cursor;
          after = prev_line.after;
          pos = List.length prev_line.before;
        } in
        {
          before = rest_before;
          current = new_current;
          after = current_line :: buf.after;
          (* Utiliser get_pos *)
          pos = buf.pos - 1;
        }
  else
    { buf with current = line_move_left buf.current } (* Cas normal: on se déplace simplement à gauche *)

(**
 * Déplace le curseur vers la droite dans le buffer
 * @param buf Le buffer à modifier
 * @return Nouveau buffer avec curseur déplacé
 * @raises Failure si on est déjà en fin de ligne
 *)

let move_right buf =
  let current_line = buf.current in
  if current_line.after = [] then (* Fin de la ligne courante *)
    match buf.after with
    | [] -> buf (* Déjà sur la dernière ligne, rien à faire *)
    | next_line :: rest_after -> (* Il y a une ligne en dessous *)
    (*Peut être utilisé empty line / buf*)
        let new_current = {
          before = [];
          current = Cursor;
          after = List.rev next_line.before @ next_line.after; (*Attention a l'ordre de concaténation*)
          pos = 0;
        } in
        {
          before = current_line :: buf.before;
          current = new_current;
          after = rest_after;
          (* Utiliser get_pos *)
          pos = buf.pos + 1;
        }
  else
    { buf with current = line_move_right buf.current } (* Cas normal: on se déplace simplement à droite *)

(**
 * Crée une nouvelle ligne dans le buffer
 * @param buf Buffer actuel
 * @return Nouveau buffer avec nouvelle ligne
 *)

let create_newline buf =
  let current_line = buf.current in
  let new_line = { empty_line with after = current_line.after } in (* texte après le curseur passe à la nouvelle ligne *)
  let updated_current = {
    (*Peut être utilisé empty line / buf*)
    before = current_line.before;
    current = Cursor;
    after = [];  (* on vide la partie après car elle a été déplacée *)
    pos = List.length current_line.before;
  } in
  {
    before = updated_current :: buf.before;
    current = new_line;
    after = buf.after;
    (* Utiliser get_pos *)
    pos = buf.pos + 1;
  }

  (**
 * Supprime le caractère avant le curseur
 * @param buf Buffer actuel
 * @return Nouveau buffer avec caractère supprimé
 *)
 
let do_backspace buf =
  let current_line = buf.current in
  match current_line.before with
  | [] -> 
      (* Cas où on est en début de ligne *)
      begin match buf.before with
      | [] -> buf (* Rien à faire si c'est la première ligne *)
      | prev_line::rest_before ->
          (* Fusionne avec la ligne précédente *)
          let merged_line = {
            before = prev_line.before;
            current = Cursor;
            after = prev_line.after @ current_line.after;
            pos = List.length prev_line.before + List.length prev_line.after
          } in
          { buf with 
            before = rest_before;
            current = merged_line;
            (* Utiliser get_pos *)
            pos = buf.pos - 1
          }
      end
  | _::tl ->
      (* Cas normal : supprime le caractère précédent *)
      let update_line line = { 
        line with 
        before = tl;
        pos = line.pos - 1 
      } in
      update_with update_line buf

(***** NE RIEN MODIFIER À PARTIR DE CE POINT **)       

let apply_action a buf =
    match a with
    | Up        -> move_up    buf
    | Left      -> move_left  buf
    | Down      -> move_down  buf
    | Right     -> move_right buf
    | Char ch   -> insert_char ch buf
    | Newline   -> create_newline buf
    | Delete    -> do_suppr buf
    | Backspace -> do_backspace buf
;;
let wopen () =
  let args = Printf.sprintf " %dx%d" sx sy in
  let _ = Graphics.open_graph args in
  let _ = Graphics.set_window_title "test" in
  ()

let font_width,font_height = 18,18

let line_height = font_height + 4

let line_width = font_width + 4
             
let default_char = Char.chr 167 



                 
let draw_square col row color c =
  let _ =
    Graphics.moveto (col*line_width+4) (Graphics.size_y () - row * line_height +2) in
  let _ = Graphics.set_color color in
  let _ = Graphics.fill_rect (col*(line_width)) (Graphics.size_y () - row * (line_width)) (line_width) (line_height) in
  let _ = Graphics.set_color Graphics.black in
  let _ = Graphics.draw_rect (col*(line_width)) (Graphics.size_y () - row * (line_width)) (line_width) (line_height)
  in
  Graphics.draw_char c



let draw_line is_current row l =
  let print i c =
    let _ = draw_square i row Graphics.white c in
    i+1
  in
  let col = List.fold_right (fun c i -> print i c) l.before 0 in
  let _ = List.fold_left print col l.after in 
  let _ =
    if is_current
    then
    let _ = Graphics.set_color Graphics.red in
      let _ = Graphics.fill_rect (col*(line_width)-2) (Graphics.size_y () - row * (line_width)) (4) (line_height) in
      Graphics.set_color Graphics.black
    else ()
  in
  ()

let draw_buffer buf =
  let print b j l =
    let _ = Format.printf "line : %d@." j in
    let _ = draw_line b j l in
    j+1
  in
  let row = List.fold_right (fun l j -> print false j l) buf.before 1 in
  let _ = print true row buf.current in
  List.fold_left (print false) (row+1) buf.after
  
  
let rec loop  buf =
  let _ = Graphics.clear_graph () in 
  let _ = draw_buffer buf in 
  let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
  let ch = ev.Graphics.key in
  if Char.code ch = 27 (* esc *) 
  then ()
  else 
    let laction = [
        Char.chr 26,Up;
        Char.chr 19,Down;
        Char.chr 17,Left;
        Char.chr 4,Right;
        Char.chr 13,Newline;
        Char.chr 127,Delete;
        Char.chr 8,Backspace
      ]
    in
    let buf1 = 
      match List.assoc_opt ch laction with
      | Some a -> apply_action a buf
      | None ->
                  if String.contains valid_chars ch
         then apply_action  (Char ch) buf
         else
           let code = Char.code ch in
           let msg = if code >= 1 && code <= 26
                     then Format.sprintf " (CTRL + %c)" (Char.chr (Char.code 'A' + code -1 ))
                     else ""
           in
           let _ = 
             Format.fprintf Format.err_formatter
               "Invalid char : ascii code %d %s@."
               code
               msg
           in 
           buf
    in
    loop buf1
  
let main () =
  let _ = wopen () in
  let _ = loop empty_buf in 
  let _ = Graphics.close_graph () in
  ()

let _ = main  ()
