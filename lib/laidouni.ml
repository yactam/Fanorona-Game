open Engine

let my_ia2 player board move_chain =
  let all_moves = get_all_moves board player in
  if List.is_empty all_moves then Lwt.return None
  else
    let capture_moves =
      List.filter
        (fun m ->
          type_capture_move board m player <> None
          && (List.is_empty move_chain
             || is_last_pawn_position_move m move_chain))
        all_moves
    in
    let not_capture_moves =
      List.filter
        (fun m ->
          type_capture_move board m player = None && List.is_empty move_chain)
        all_moves
    in
    let diag_captures_moves =
      List.filter
        (fun m ->
          let dir = m.direction in
          dir = SE || dir = SW || dir = NE || dir = NW)
        capture_moves
    in
    let diag_not_captures_moves =
      List.filter
        (fun m ->
          let dir = m.direction in
          dir = S || dir = W || dir = E || dir = N)
        not_capture_moves
    in
    let random_element lst =
      let len = List.length lst in
      if len = 0 then None else Some (List.nth lst (Random.int len))
    in
    let move_option =
      if not (List.is_empty diag_captures_moves) then
        random_element diag_captures_moves
      else if not (List.is_empty capture_moves) then
        random_element capture_moves
      else if not (List.is_empty diag_not_captures_moves) then
        random_element diag_not_captures_moves
      else random_element all_moves
    in
    let type_capture_option =
      match move_option with
      | None -> None
      | Some m -> type_capture_move board m player
    in
    Lwt.return
      (Some
         ( move_option,
           if type_capture_option = Some Both then Some Approach
           else type_capture_option ))

(* ********************** Second IA ********************** *)

(** Get a readable board *)
let get_readable_board b =
  List.init nb_rows (fun x ->
      List.init nb_cols (fun y -> get b (Pos.h x) (Pos.v y)))

(** return [p] point of a readable_board [rb] *)
let count_points rb p =
  let rec aux_line line cnt =
    match line with
    | [] -> cnt
    | Pawn k :: others_cell when k = p -> aux_line others_cell (cnt + 1)
    | _ :: others_cell -> aux_line others_cell cnt
  in
  let rec aux lines cnt =
    match lines with
    | [] -> cnt
    | line :: others_lines -> aux others_lines (aux_line line cnt)
  in
  aux rb 0

(** try make [move] and return player points if we do this move *)
let point_of_move board player move move_chain =
  let capture_option = type_capture_move board move player in
  let b, _ =
    try make_move board player move capture_option move_chain
    with _ -> (board, move_chain)
  in
  count_points (get_readable_board b) player

let my_ia player board move_chain =
  let readable_board = get_readable_board board in
  let current = count_points readable_board player in
  let all_moves = get_all_moves board player in
  if List.is_empty all_moves then Lwt.return None
  else
    let capture_moves =
      List.filter
        (fun m ->
          type_capture_move board m player <> None
          && (List.is_empty move_chain
             || is_last_pawn_position_move m move_chain))
        all_moves
    in
    let random_element lst =
      let len = List.length lst in
      if len = 0 then None else Some (List.nth lst (Random.int len))
    in
    let choose lst =
      let rec aux lst acc =
        match lst with
        | [] -> acc
        | h :: t
          when point_of_move board player h move_chain
               > point_of_move board player acc move_chain ->
            aux t h
        | _ :: t -> aux t acc
      in
      aux lst (List.nth lst 0)
    in
    let move_option =
      if
        (not (List.is_empty capture_moves))
        && point_of_move board player (choose capture_moves) move_chain
           > current
      then Some (choose capture_moves)
      else random_element all_moves
    in
    let type_capture_option =
      match move_option with
      | None -> None
      | Some m -> type_capture_move board m player
    in
    Lwt.return
      (Some
         ( move_option,
           if type_capture_option = Some Both then Some Approach
           else type_capture_option ))
