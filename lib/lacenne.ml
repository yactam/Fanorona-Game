open Engine

module BotYanis = struct
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

  let keep_n =
    let rec to_keep acc n l =
      if n = 0 then acc
      else
        l |> function [] -> acc | el :: xs -> to_keep (el :: acc) (n - 1) xs
    in
    to_keep []

  let count element =
    let rec total n = function
      | [] -> n
      | x :: xs -> total (if x = element then n + 1 else n) xs
    in
    total 0

  let random_element lst =
    let len = List.length lst in
    if len = 0 then None else Some (List.nth lst (Random.int len))

  let center_x, center_y = (nb_cols / 2, nb_rows / 2)

  let control_points =
    [
      (center_x + 2, center_y + 1);
      (center_x - 2, center_y - 1);
      (center_x + 2, center_y - 1);
      (center_x - 2, center_y + 1);
      (center_x, 0);
      (center_x, nb_rows - 1);
      (0, center_y);
      (nb_cols - 1, center_y);
    ]

  let dist_from_controls_points (x, y) =
    List.map
      (fun (x', y') ->
        abs (Engine.get_line x - x') + abs (Engine.get_col y - y'))
      control_points

  let closest_to_center moves =
    moves
    |> List.map (fun move ->
           ( move.position |> dist_from_controls_points
             |> List.fold_left (fun min x -> Stdlib.min min x) max_int,
             Some move ))
    |> List.fold_left
         (fun (shortest_dist, move) (new_dist, new_move) ->
           if shortest_dist < new_dist then (shortest_dist, move)
           else (new_dist, new_move))
         (max_int, random_element moves)
    |> snd

  let filter_valid_moves board player list_move =
    List.filter_map (fun move ->
        try
          let _new_board, _new_list_move =
            make_move board player move
              (type_capture_move board move player)
              list_move
          in
          let continue = can_continue _new_board player move list_move in
          let value = if continue then 5 else 0 in
          Some (count_player _new_board (opponent player) - value, move)
        with
        | Capture_move_restrictions_broken | Compulsory_capture
        | Invalid_position ->
            None
        | _ -> Some (-2, move))

  let partition_capture_move board player list_move =
    List.partition (fun m ->
        type_capture_move board m player <> None
        && (List.is_empty list_move || is_last_pawn_position_move m list_move))

  let choose_best_capture_move board player movements list_move = function
    | [] ->
        let last_five_moves = keep_n 10 list_move in
        movements
        |> List.map (fun move -> (count move last_five_moves, move))
        |> List.filter_map (fun (counter, move) ->
               if counter <= 1 then Some move else None)
        |> closest_to_center
    | captures_moves -> (
        captures_moves
        |> filter_valid_moves board player list_move
        |> List.fold_left
             (fun (min, best_move) (remaining_opponents, move) ->
               if min < remaining_opponents then (min, best_move)
               else (remaining_opponents, Some move))
             (max_int, None)
        |> snd
        |> function
        | Some move -> Some move
        | None -> Some (List.hd movements))

  let next_move (player : player) board list_move =
    get_all_moves board player |> function
    | [] -> None
    | [ move ] -> Some move
    | moves ->
        let captures_moves, movements =
          partition_capture_move board player list_move moves
        in
        captures_moves
        |> choose_best_capture_move board player movements list_move

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
end

let bot = BotYanis.player_lacenne
