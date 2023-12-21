open Engine

let swap = opponent

let player_random_for_minimax player board move_chain =
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
    let rec random_element lst =
      let len = List.length lst in
      if len = 0 then None
      else if
        position_or_direction_or_line_already_executed move_chain
          (List.nth lst (Random.int len))
      then random_element lst
      else Some (List.nth lst (Random.int len))
    in
    let move_option =
      let len = List.length capture_moves in
      if len = 0 then random_element all_moves else random_element capture_moves
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

let pawns_remaining board player =
  let rec aux i j n =
    let n =
      n
      +
      match get board (Pos.h i) (Pos.v j) with
      | Empty -> 0
      | Pawn p when p = player -> 0
      | _ -> 1
    in
    if i = 4 && j = 8 then n
    else if j = 8 then aux (i + 1) 0 n
    else aux i (j + 1) n
  in
  aux 0 0 0

let nearly_win board player = pawns_remaining board player <= 4

let minimax_player player board chain =
  let max_depth = 5 in

  if nearly_win board player || nearly_win board (swap player) then
    player_random_for_minimax player board chain
  else
    let rec minimax p b max_player move_chain depth =
      if depth = max_depth then (-pawns_remaining b p, -1)
      else if win b p then (1, -1)
      else if win b (swap p) then (-1, -1)
      else
        let all_moves = get_all_moves b p in
        let capture_moves =
          List.filter
            (fun m ->
              type_capture_move b m p <> None
              && (List.is_empty move_chain
                 || is_last_pawn_position_move m move_chain))
            all_moves
        in
        let possible_moves =
          if List.is_empty capture_moves then all_moves else capture_moves
        in

        let rec aux l value n best_move =
          match l with
          | [] -> (value, best_move)
          | h :: lm -> (
              match h with
              | None -> (0, 0)
              | Some move ->
                  if
                    position_or_direction_or_line_already_executed move_chain
                      move
                  then
                    if max_player then aux lm value (n + 1) best_move
                    else aux lm min_int (n + 1) n
                  else
                    let type_capture_option = type_capture_move b move p in
                    let type_capture =
                      if type_capture_option = Some Both then Some Approach
                      else type_capture_option
                    in
                    let is_capture = type_capture <> None in

                    let v, _ =
                      try
                        let board', move_chain' =
                          make_move b p move type_capture move_chain
                        in

                        if is_capture && can_continue board' p move move_chain'
                        then minimax p board' max_player move_chain' (depth + 1)
                        else
                          minimax (swap p) board' (not max_player) [] (depth + 1)
                      with e ->
                        Format.printf "%s@." (Printexc.to_string e);
                        minimax p b max_player move_chain (depth + 1)
                    in

                    if v > value then
                      if max_player then aux lm v (n + 1) n
                      else aux lm value (n + 1) best_move
                    else if max_player then aux lm value (n + 1) best_move
                    else aux lm v (n + 1) n)
        in
        let v, b =
          aux
            (List.map (fun x -> Some x) possible_moves)
            (if max_player then min_int else max_int)
            0 0
        in
        (v, b)
    in

    let _, child = minimax player board true chain 0 in
    let all_moves = get_all_moves board player in
    let capture_moves =
      List.filter
        (fun m ->
          type_capture_move board m player <> None
          && (List.is_empty chain || is_last_pawn_position_move m chain))
        all_moves
    in
    let possible_moves =
      if List.is_empty capture_moves then all_moves else capture_moves
    in
    let move = List.nth possible_moves child in
    Lwt.return
      (Some
         ( Some move,
           let t = type_capture_move board move player in
           if t = Some Both then Some Approach else t ))
