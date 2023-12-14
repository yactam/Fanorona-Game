open Engine

let count_player board player =
  let rec aux acc i j =
    match (i, j) with
    | i, _ when i >= nb_rows -> acc
    | i, j when j >= nb_cols -> aux acc (i + 1) 0
    | i, j ->
        let cell = get board (Pos.h i) (Pos.v j) in
        aux (if cell = Pawn player then acc + 1 else acc) i (j + 1)
  in
  aux 0 0 0

let center_x, center_y = (nb_cols / 2, nb_rows / 2)

let dist_from_center (x, y) =
  abs (Engine.get_line x - center_x) + abs (Engine.get_col y - center_y)

let closest_to_center moves =
  moves
  |> List.map (fun move -> (move.position |> dist_from_center, Some move))
  |> List.fold_left
       (fun (shortest_dist, move) (new_dist, new_move) ->
         if shortest_dist < new_dist then (shortest_dist, move)
         else (new_dist, new_move))
       (max_int, Some (List.hd moves))
  |> snd

let next_move (player : player) board list_move =
  get_all_moves board player |> function
  | [] -> None
  | [ move ] -> Some move
  | moves -> (
      let captures_moves, movements =
        List.partition
          (fun m ->
            type_capture_move board m player <> None
            && (List.is_empty list_move
               || is_last_pawn_position_move m list_move))
          moves
      in
      captures_moves |> function
      | [] -> closest_to_center movements
      | captures_moves -> (
          captures_moves
          |> List.filter_map (fun move ->
                 try
                   let _new_board, _new_list_move =
                     make_move board player move
                       (type_capture_move board move player)
                       list_move
                   in
                   let continue =
                     can_continue _new_board player move list_move
                   in
                   let value = if continue then 5 else 0 in
                   Some (count_player _new_board (opponent player) - value, move)
                 with
                 | Capture_move_restrictions_broken | Compulsory_capture -> None
                 | _ -> Some (-2, move))
          |> List.fold_left
               (fun (min, best_move) (remaining_opponents, move) ->
                 if min < remaining_opponents then (min, best_move)
                 else (remaining_opponents, Some move))
               (max_int, None)
          |> snd
          |> function
          | Some move -> Some move
          | None -> Some (List.hd movements)))

let player_lacenne player board move_chain =
  let all_moves = get_all_moves board player in
  if List.is_empty all_moves then Lwt.return None
  else
    let move_option = next_move player board move_chain in
    let type_capture_option =
      match move_option with
      | None -> None
      | Some move -> (
          type_capture_move board move player |> function
          | Some Both -> Some Approach
          | type_capture -> type_capture)
    in
    Lwt.return (Some (move_option, type_capture_option))
