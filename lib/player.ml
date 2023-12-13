open Engine

let width = nb_cols
let height = nb_rows

type gameTree = Node of (gameTree * int) list | Leaf of int

let is_player_on_cell board player w h =
  get board (Pos.h h) (Pos.v w) |> function
  | Pawn p when p = player -> true
  | _ -> false

let rate_board board (player : player) =
  let rec count_cell h w total =
    if w < 0 && h < 0 then total
    else if h < 0 then
      count_cell height (width - 1)
        (total + if is_player_on_cell board player w h then 1 else 0)
    else
      count_cell (height - 1) width
        (total + if is_player_on_cell board player w h then 1 else 0)
  in
  count_cell height width 0

let _rating_function _board = 0

let rec _minimax (depth : int) (maxplayer : bool) (player : player)
    player_list_move opponent_list_move _actual_board =
  if depth < 0 then (0, None)
  else
    get_all_moves _actual_board player |> function
    | [] -> (-100, None) (* TODO : Obtain heuristic value *)
    | [ move ] -> (_rating_function _actual_board, Some move)
    | move :: _ when depth = 0 -> (0, Some move)
    | _possibles_moves ->
        if maxplayer then
          (* player *)
          List.fold_left
            (fun (max, move) (v, move2) ->
              if Stdlib.max max v = max then (max, move) else (v, move2))
            (0, None)
            (List.filter_map
               (fun move ->
                 let continue =
                   can_continue _actual_board player move player_list_move
                 in
                 try
                   let new_board, new_list_move =
                     make_move _actual_board player move
                       (type_capture_move _actual_board move player)
                       player_list_move
                   in
                   Some
                     ( _minimax (depth - 1) continue
                         (if continue then player else opponent player)
                         new_list_move opponent_list_move new_board
                       |> fst,
                       Some move )
                 with _ -> None)
               _possibles_moves)
        else
          (* opponent player *)
          List.fold_left
            (fun (min, move) (v, move2) ->
              if Stdlib.min min v = min then (min, move) else (v, move2))
            (max_int, None)
            (List.map
               (fun move ->
                 _minimax (depth - 1) true (opponent player)
                   (move :: player_list_move) [] _actual_board)
               _possibles_moves)

let player player board move_chain =
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
    let move_option =
      let len = List.length capture_moves in
      Some (if len = 0 then List.hd all_moves else List.hd capture_moves)
    in
    let type_capture_option =
      match move_option with
      | None -> None
      | Some move -> type_capture_move board move player
    in
    Lwt.return
      (Some
         ( move_option,
           if type_capture_option = Some Both then Some Approach
           else type_capture_option ))
