open Fanorona.Engine

let in_board x y= x>=0 && x<nb_rows && y>=0 && y<nb_cols

let valid_chain_move move chain=
  let rec aux chain=
    match chain with
    | [] -> true
    | m :: next -> if m.direction = move.direction || m.position = move.position then false else aux next
  in aux chain

let find_better list f chain=
  let rec aux l move=
    match l with
    |[] -> Some move
    |e :: next -> 
      if valid_chain_move e chain
        then if f move e then aux next move else aux next e
      else aux next move
  in
  if chain=[] then aux (List.tl list) (List.hd list)
  else let rec find_start l=
    match l with
    |[] -> None
    |m :: next -> if valid_chain_move m chain then aux next m else find_start next
  in find_start list

let is_taker player board move = 
  match type_capture_move board move player with
  | None -> false
  | _ -> true

 let is_side_secure player board (x1,y1) (x2,y2)= 
    if (in_board x1 y1) && (in_board x2 y2) then
      let cell x y= get board (Pos.h x) (Pos.v y) in
      match cell x1 y1 with
      | Empty -> (match cell x2 y2 with
         |Empty -> true
        |Pawn p -> p = player
      )
      | Pawn p -> if p= player then true else cell x2 y2 != Empty
    else true

let is_secure player board move =
  let aux h v=
    is_side_secure player board (h,v+1) (h,v+2) &&
    is_side_secure player board (h+1,v+1) (h+2,v+2) &&
    is_side_secure player board (h+1,v) (h+2,v)  &&
    is_side_secure player board (h+1,v-1) (h+2,v-2) &&
    is_side_secure player board (h,v-1) (h,v-2) &&
    is_side_secure player board (h-1,v-1) (h-2,v-2) &&
    is_side_secure player board (h-1,v) (h-2,v) &&
    is_side_secure player board (h-1,v+1) (h-2,v+2)
  in
  let h= get_line (fst move.position) in
  let v= get_col (snd move.position)  in
  match move.direction with
  |N -> aux h (v+1)
  |S -> aux h (v-1)
  |E -> aux (h+1) v
  |W -> aux (h-1) v
  |NE -> aux (h+1) (v+1)
  |SW -> aux (h-1) (v-1)
  |NW -> aux (h-1) (v+1)
  |SE -> aux (h+1) (v-1)
  

let count_takes player board move is_approach=
  let approach= if is_approach then 1 else 0 in
  let h= get_line (fst move.position) in
  let v= get_col (snd move.position)  in
  let rec aux h v hvar vvar acc=
    if (in_board h v) then
      let cell= get board (Pos.h h) (Pos.v v) in
      if cell = Empty || cell = Pawn player
      then acc
      else aux (h+hvar) (v+vvar) hvar vvar (acc+1)
    else acc 
  in
  match move.direction with
  |N -> aux h (v+1+approach) 0 1 0
  |S -> aux h (v-1-approach) 0 (-1) 0
  |E -> aux (h+1+approach) v 1 0 0
  |W -> aux (h-1-approach) v (-1) 0 0
  |NE -> aux (h+1+approach) (v+1+approach) 1 1 0
  |SW -> aux (h-1-approach) (v-1-approach) (-1) (-1) 0
  |NW -> aux (h-1-approach) (v+1+approach) (-1) 1 0 
  |SE -> aux (h+1+approach) (v-1-approach) 1 (-1) 0

let how_many_take player board move =
  match type_capture_move board move player with
  |None -> 0
  |Some Approach -> count_takes player board move true
  |Some Withdrawal-> count_takes player board move false
  |Some Both -> Stdlib.max (count_takes player board move true) (count_takes player board move false)

let how_close player board move =
  let h= get_line (fst move.position) in
  let v= get_col (snd move.position)  in
  let rec aux h v hvar vvar acc=
    if in_board h v then
      let cell=get board (Pos.h h) (Pos.v v) in 
      if cell = Empty || cell == Pawn player
      then aux (h+hvar) (v+vvar) hvar vvar (acc+1)
      else acc 
    else acc 
  in
  match move.direction with
  |N -> aux h (v+1) 0 1 0
  |S -> aux h (v-1) 0 (-1) 0
  |E -> aux (h+1) v 1 0 0
  |W -> aux (h-1) v (-1) 0 0
  |NE -> aux (h+1) (v+1) 1 1 0
  |SW -> aux (h-1) (v-1) (-1) (-1) 0
  |NW -> aux (h-1) (v+1) (-1) 1 0 
  |SE -> aux (h+1) (v-1) 1 (-1) 0

let count_side_lost player board (h,v) acch accv=
  if(in_board h v) then
    if (is_side_secure player board (h,v) (h+acch,v+accv))
      then 0
    else let rec aux h v hvar vvar acc=
     if (in_board h v) then
        if(get board (Pos.h h) (Pos. v v) != Pawn player)
        then acc
        else aux (h+hvar) (v+vvar) hvar vvar (acc+1)
      else acc
    in aux h v (- acch) (- accv) 0
  else 0

let how_many_lost player board move =
  let aux h v= 
    Stdlib.max(count_side_lost player board (h,v+1) 0 1,
    Stdlib.max(count_side_lost player board (h+1,v+1) 1 1,
    Stdlib.max(count_side_lost player board (h+1,v) 1 0,
    Stdlib.max(count_side_lost player board (h+1,v-1) 1 (-1),
    Stdlib.max(count_side_lost player board (h,v-1) 0 (-1),
    Stdlib.max(count_side_lost player board (h-1,v-1) (-1) (-1),
    Stdlib.max(count_side_lost player board (h-1,v) (-1) 0,
    count_side_lost player board (h-1,v+1) (-1) 1)))))))
  in
  let h= get_line (fst move.position) in
  let v= get_col (snd move.position)  in
  match move.direction with
  |N -> aux h (v+1)
  |S -> aux h (v-1)
  |E -> aux (h+1) v
  |W -> aux (h-1) v
  |NE -> aux (h+1) (v+1)
  |SW -> aux (h-1) (v-1)
  |NW -> aux (h-1) (v+1)
  |SE -> aux (h+1) (v-1)

let better_move p b moves move_chain=
  let taker_moves = List.filter (fun m -> is_taker p b m) moves in 
  if(List.length taker_moves != 0) 
    then let taker_secure_moves = List.filter (fun m -> is_secure p b m) taker_moves in 
    if(List.length taker_secure_moves != 0)
      then find_better taker_secure_moves (fun m1 m2-> (how_many_take p b m1) > (how_many_take p b m2)) move_chain
    else find_better taker_moves (fun m1 m2-> (how_many_take p b m1)>(how_many_take p b m2)) move_chain
  else let secure_moves= List.filter (fun m -> is_secure p b m) moves in
    if(List.length secure_moves != 0) 
      then find_better secure_moves (fun m1 m2 -> (how_close p b m1) < (how_close p b m2)) move_chain
    else find_better moves (fun m1 m2-> (how_many_lost p b m1) < (how_many_lost p b m2)) move_chain

let ia_player player board move_chain : (move option * capture option) option Lwt.t=
  let moves= get_all_moves board player in
  match moves with
  |[] -> Lwt.return None
  |_ -> match better_move player board moves move_chain with
        |None -> Lwt.return None
        |Some m -> Lwt.return (Some (Some m, type_capture_move board m player))