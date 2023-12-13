open Engine

let width = 9
let height = 5

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

let _rating_function = None

let rec _minimax (nodes : gameTree) (depth : int) (maxplayer : bool)
    (player : player) =
  if depth < 0 then 0
  else
    match nodes with
    | Leaf value -> value (* TODO : Obtain heuristic value *)
    | Node _possibles_moves ->
        if _possibles_moves = [] then 0
        else if depth = 0 then List.hd _possibles_moves |> snd
        else if maxplayer then
          (* max player *)
          List.fold_left
            (fun max v -> Stdlib.max max v)
            0
            (List.map
               (fun move ->
                 _minimax (fst move) (depth - 1) false (opponent player))
               _possibles_moves)
        else
          (* min player *)
          List.fold_left
            (fun min v -> Stdlib.min min v)
            max_int
            (List.map
               (fun move ->
                 _minimax (fst move) (depth - 1) true (opponent player))
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
