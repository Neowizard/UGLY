#rectypes;;

type 'a structure =
  | Simple
  | HorzTop of 'a list
  | HorzMid of 'a list
  | HorzBot of 'a list
  | VertLeft of 'a list
  | VertCenter of 'a list
  | VertRight of 'a list;;

  
class box = fun rows cols box_struct getter ->
object (self)
  val rows : int = rows
  val cols : int = cols
  val tags : string list = []
  val getter : int -> int -> char = getter
  val box_struct : (box structure) = box_struct
  val decorator : char -> char = (fun c -> c)
  method box_struct = box_struct
  method width = cols
  method height = rows
  method get r c = decorator (getter r c)
  method decorate d = {< decorator = d >}
  method get_decorator = decorator
  method tag t = {< tags = t::tags >}
  method set_tags ts = {< tags = ts >}
  method get_tags = tags
  method has_tag t = List.exists (String.equal t) tags
  method sub_boxes =
    match self#box_struct with
    | Simple -> [];
    | (HorzTop(l) 
      | HorzMid(l) 
      | HorzBot (l)
      | VertLeft (l)
      | VertCenter(l)
      | VertRight(l)) -> l
                           
  method is_vert = match self#box_struct with
    | HorzTop _ | HorzMid _ | HorzBot _ | Simple _ -> false
    | _ -> true

  method is_Horz = match self#box_struct with
    | HorzTop _ | HorzMid _ | HorzBot _ -> true
    | _ -> false

  method is_simple = match self#box_struct with
    | Simple _ -> true
    | _ -> false
                                                              
  method to_list =
    List.map (fun row ->
        List.map (fun col -> self#get row col)
                 (0---(cols-1))) (* x---y is defined in uitls.ml *)
             (0---(rows-1)) 
  
  method to_string =
    self#to_string_with_delim "\n"
                         
  method to_string_with_delim row_delim =
    let rows = self#to_list in
    String.join row_delim (List.map String.of_list rows)

end;;

let rec box_struct_to_string b =
  let struct_to_string =
    function
    | Simple -> "S"
    | VertLeft l -> Printf.sprintf "VL%d" (List.length l)
    | VertRight l -> Printf.sprintf "VR%d" (List.length l)
    | VertCenter l -> Printf.sprintf "VC%d" (List.length l)
    | HorzTop l -> Printf.sprintf "HT%d" (List.length l)
    | HorzMid l -> Printf.sprintf "HM%d" (List.length l)
    | HorzBot l -> Printf.sprintf "HB%d" (List.length l) in
  let rec printer depth b = 
    match b#box_struct with
    | Simple -> Printf.sprintf "%s%s: %s"
                               (String.make depth ' ')
                               (struct_to_string b#box_struct)
                               b#to_string
    | (HorzTop(l) 
      | HorzMid(l) 
      | HorzBot (l)
      | VertLeft (l)
      | VertCenter(l)
      | VertRight(l)) -> Printf.sprintf
                           "%s%s:\n%s"
                           (String.make depth ' ')
                           (struct_to_string b#box_struct)
                           (String.join "\n" (List.map
                                                (printer (depth + 4))
                                                b#sub_boxes)) in
  printer 0 b
  
let empty_box = new box 0 0 Simple (fun _ _ -> ' ');;

let make_blank_box rows cols =
  new box rows cols Simple (fun _ _ -> ' ');;  

let horz_compose boxes structure make_g =
  match boxes with
  | [] -> empty_box
  | _ ->  let r = List.fold_left (fun r b -> max r (b#height)) 0 boxes in
          let c = List.fold_left (fun c b -> c+(b#width)) 0 boxes in
          let s = structure in
          new box r c s (make_g r c);;

let vert_compose boxes structure make_g =
  match boxes with
  | [] -> empty_box
  | _ -> let r = List.fold_left (fun r b -> r + (b#height)) 0 boxes in
         let c = List.fold_left (fun c b -> max c (b#width)) 0 boxes in
         let s =  structure in
         new box r c s (make_g r c)
             
let rec vert_find_box boxes row =
  match boxes with
  | [] -> (row, empty_box)
  | b::[] -> (row, b)
  | b::boxes -> if row < (b#height) then (row, b)
                else vert_find_box boxes (row - (b#height));;
                  
let rec horz_find_box boxes col =
  match boxes with
  | [] -> (col, empty_box)
  | b::[] -> (col, b)
  | b::boxes -> if col < (b#width) then (col,b)
                else horz_find_box boxes (col - (b#width));;

let horz_top boxes =
  horz_compose boxes (HorzTop boxes)
               (fun max_rows _ ->
                 (fun row col ->
                   let col,b = (horz_find_box boxes col) in
                   (b#get row col)));;

  
let horz_bot boxes =
  horz_compose boxes (HorzBot boxes)
               (fun max_rows _ ->
                 (fun row col ->
                   let col, b = horz_find_box boxes col in
                   let row = row - (max_rows - b#height) in
                   (b#get row col)));;

    
let horz_mid boxes =
  horz_compose boxes (HorzMid boxes)
               (fun max_rows _ ->
                 (fun row col ->
                   let col, b = horz_find_box boxes col in
                   let row = row - (max_rows - b#height)/2 in
                   (b#get row col)));;

let vert_left boxes =
  vert_compose boxes (VertLeft boxes)
               (fun _ max_col ->
                 (fun row col ->
                   let row, b = (vert_find_box boxes row) in
                   (b#get row col)));;

  
let vert_right boxes =
  vert_compose boxes (VertRight boxes)
               (fun _ max_col ->
                 (fun row col ->
                   let row, b = vert_find_box boxes row in
                   let col = col - (max_col - b#width) in
                   (b#get row col)));;

let vert_center boxes =
  vert_compose boxes (VertCenter boxes)
               (fun _ max_col ->
                 (fun row col ->
                   let row, b = vert_find_box boxes row in
                   let col = col - (max_col - b#width)/2 in
                   (b#get row col)));;

(*let string_to_box s =
  let rows = (String.split_on_char '\n' s) in
  if (rows = []) then empty_box
  else let r = List.length rows in
       let c = String.length (List.max rows) in
       let g r c =
         try String.get (List.nth rows r) c
         with Invalid_argument _ -> ' ' in
       new box r c Simple g;;*)
                                                   
let string_to_box s =
  let getter s r c =
    if (r != 0) then ' '
    else try String.get s c
         with e -> ' ' in
  let string_to_box s =
    if (s = "") then empty_box
    else new box 1 (String.length s) Simple (getter s) in
  let s = String.split_on_char '\n' s in
  let s = List.map string_to_box s in
  vert_left s;;
          
let string_to_vert_box s =
  let b = string_to_box s in
  let r = b#width in
  let c = b#height in
  let g = flip b#get in
  new box r c Simple g;;

let get_box_constructor b =
  let box_struct = b#box_struct in 
  match box_struct with
  | Simple -> horz_top
  | VertLeft _ -> vert_left
  | VertRight _ -> vert_right
  | VertCenter _ -> vert_center
  | HorzTop _ -> horz_top
  | HorzMid _ -> horz_mid
  | HorzBot _ -> horz_bot
                            
let rec insert_box b insert =
  let find_insert_anchor bs =
    List.fold_lefti (fun (m, mi) i b -> if (b#height >= m)
                                        then (b#height, i)
                                        else (m, mi))
                    (-1,-1)
                    bs in
  let insert_last l = 
    let firsts = List.take ((List.length l) - 1) l in
     let last = insert_bottom_right (List.last l) insert in
     firsts@[last] in
  let add_insert b max_h = 
    let pad = string_to_vert_box (String.make (max 0 (max_h - b#height - insert#height)) ' ') in
    insert_box b (vert_left [pad; insert])
  in
  let boxes = match b#box_struct with
  | Simple ->  [b;insert]
  | (VertLeft l
    | VertRight l
    | VertCenter l
    | HorzBot l) -> insert_last l
  | (HorzTop l 
    | HorzMid l) -> 
     let anchor_height, anchor_idx = find_insert_anchor l in
     if (anchor_idx = (List.length l) - 1)
     then insert_last l
     else List.mapi (fun i b -> if (i = anchor_idx)
                                then (add_insert b anchor_height)
                                else b)
                    l
  in
  let new_b = (get_box_constructor b) boxes in
  let new_b = new_b#decorate b#get_decorator in
  let new_b = new_b#set_tags b#get_tags in
  new_b

and insert_bottom_right b insert = insert_box b insert

and insert_boxes boxes =
  List.fold_left insert_bottom_right empty_box boxes
  
and append_box b1 b2 =
  if b2 = empty_box then b1
  else let pad_h = max (b2#height - 1) 0 in
       let pad = make_blank_box pad_h b1#width in
       let b1 = vert_left [b1;pad] in
       horz_bot [b1;b2]
             
and append_boxes boxes =
  (* let boxes = List.filter (fun b -> empty_box != b) boxes in  *)
  (* let boxes =  *)
  List.fold_left append_box empty_box boxes;;

let struct_to_string = function
  | Simple -> "S"
  | VertLeft l -> Printf.sprintf "VL%d" (List.length l)
  | VertRight l -> Printf.sprintf "VR%d" (List.length l)
  | VertCenter l -> Printf.sprintf "VC%d" (List.length l)
  | HorzTop l -> Printf.sprintf "HT%d" (List.length l)
  | HorzMid l -> Printf.sprintf "HM%d" (List.length l)
  | HorzBot l -> Printf.sprintf "HB%d" (List.length l) 
  
let print_box b =
  let make_frame_horz s =
    let hash_len = max 0 (b#width - (String.length s)) in
    horz_top [string_to_box (String.make hash_len '#'); string_to_box s] in
  let frame_vert = string_to_vert_box (String.make (b#height+2) '#') in
  let footer = Printf.sprintf "%d,%d" b#height b#width in
  let header = struct_to_string b#box_struct in
  let tags =  List.map string_to_box b#get_tags in
  let tags = if (tags != [])
             then horz_bot ((string_to_box "tags: ")::tags) 
             else empty_box in
  let display_box =
    vert_left [tags;
               horz_top [frame_vert;
                         vert_left [make_frame_horz header;
                                    b;
                                    make_frame_horz footer];
                         frame_vert]] in
  Printf.printf "%s\n" (display_box#to_string);;
                                       

  
