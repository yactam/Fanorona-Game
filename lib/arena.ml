open Engine

type trace = move list [@@deriving show, eq]
type endgame = Win of player | Giveup of player [@@deriving eq]
type result = { trace : trace; endgame : endgame; final : board }

let swap = opponent

let pp_endgame out_c = function
  | Win p -> Format.fprintf out_c "Player %a has won the game" pp_player p
  | Giveup p -> Format.fprintf out_c "Player %a gave up" pp_player p

let arena ?(init_player : player = W) ?(init_board = Engine.initial_state_5x9)
    players =
  let open Lwt.Syntax in
  let rec go n board player trace move_chain =
    if n >= 1000 then
      failwith "Taking too long";
    let opponent = swap player in
    if win board opponent then
      Lwt.return
        { trace = List.rev trace; endgame = Win opponent; final = board }
    else
      let* r = players player board move_chain in
      match r with
      | Some (None, None) | Some (None, _) -> go (n+1) board player trace move_chain
      | None ->
          Lwt.return
            { trace = List.rev trace; endgame = Giveup player; final = board }
      | Some (Some move, capture_option) ->
          let is_capture = capture_option <> None in
          let board', move_chain' =
            make_move board player move capture_option move_chain
          in
          if is_capture && can_continue board' player move move_chain' then
            go (n+1) board' player (move :: trace) move_chain'
          else go (n+1) board' opponent (move :: trace) []
  in
  go 0 init_board init_player [] []

let player_giveup _board _move_chain = Lwt.return None

let players_trace trace =
  let current_trace = ref trace in
  fun _player _board ->
    match !current_trace with
    | [] -> Lwt.return None
    | move :: trace ->
        current_trace := trace;
        Lwt.return (Some move)

let pair ~w:player_W ~b:player_B player =
  match player with B -> player_B | W -> player_W

let rec player_teletype_get_int () =
  try int_of_string (read_line ())
  with Failure _ ->
    Format.printf "Not a valid number please again: @?";
    player_teletype_get_int ()

let rec player_teletype_get_pos () =
  try
    Format.printf "Enter row number: @?";
    let row = player_teletype_get_int () in
    Format.printf "Enter column number: @?";
    let col = player_teletype_get_int () in
    (Pos.h row, Pos.v col)
  with Failure _ -> player_teletype_get_pos ()

let parse_direction dir_str =
  match String.uppercase_ascii dir_str with
  | "N" -> N
  | "S" -> S
  | "E" -> E
  | "W" -> W
  | "NE" -> NE
  | "SW" -> SW
  | "NW" -> NW
  | "SE" -> SE
  | _ -> raise (Failure "Invalid direction")

let rec player_teletype_get_dir () =
  try
    Format.printf "Enter direction (N, S, E, W, NE, SW, NW, SE): @?";
    let dir_str = read_line () in
    parse_direction dir_str
  with _ -> player_teletype_get_dir ()

let parse_capture_type capture_type_str =
  match String.uppercase_ascii capture_type_str with
  | "APPROACH" | "A" -> Some Approach
  | "WITHDRAWAL" | "W" -> Some Withdrawal
  | _ -> raise (Failure "Invalid capture type")

let rec player_teletype_get_capture_type () =
  try
    Format.printf "Select a capture type (Approach (A), Withdrawal (W)): ";
    Format.printf "%!";
    let capture_type_str = read_line () in
    parse_capture_type capture_type_str
  with _ -> player_teletype_get_capture_type ()

let rec player_teletype player board move_chain =
  Format.printf "@[<v>Player %a to play.@," pp_player player;
  Format.printf "Board: @[<v>%a@]@]@," pp_board board;
  Format.printf "Select a position: ";
  let pos = player_teletype_get_pos () in
  Format.printf "Select a direction: ";
  let dir = player_teletype_get_dir () in
  let move = { position = pos; direction = dir } in
  let type_capture = type_capture_move board move player in
  if
    (not (List.is_empty move_chain))
    && not (is_last_pawn_position_move move move_chain)
  then (
    Format.printf "You must continue with the same pawn.@,";
    player_teletype player board move_chain)
  else
    match type_capture with
    | Some Approach | Some Withdrawal | None ->
        Lwt.return (Some (Some move, type_capture))
    | Some Both ->
        Lwt.return (Some (Some move, player_teletype_get_capture_type ()))

let player_random player board move_chain =
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

let player_artacalan player board move_chain =
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
    let take_the_best_capture_move lst =
      let rec aux1 lst =
        match lst with
        | [] -> []
        | h :: t -> (type_capture_move board h player, h) :: aux1 t
      in
      let rec aux2 lst acc1 acc2 =
        match lst with
        | [] -> acc1
        | (None, _) :: _ -> acc1
        | (Some s, h2) :: t ->
            if s = Both then
              let pawns1 = count_pawn_to_down_approchal board player h2 in
              let pawns2 = count_pawn_to_down_withdrawal board player h2 in
              let pawns = if pawns1 > pawns2 then pawns1 else pawns2 in
              match acc1 with
              | None -> aux2 t (Some h2) (Some pawns)
              | _ ->
                  if pawns > Option.get acc2 then aux2 t (Some h2) (Some pawns)
                  else aux2 t acc1 acc2
            else if s = Approach then
              let pawns = count_pawn_to_down_approchal board player h2 in
              match acc1 with
              | None -> aux2 t (Some h2) (Some pawns)
              | _ ->
                  if pawns > Option.get acc2 then aux2 t (Some h2) (Some pawns)
                  else aux2 t acc1 acc2
            else if s = Withdrawal then
              let pawns = count_pawn_to_down_withdrawal board player h2 in
              match acc1 with
              | None -> aux2 t (Some h2) (Some pawns)
              | _ ->
                  if pawns > Option.get acc2 then aux2 t (Some h2) (Some pawns)
                  else aux2 t acc1 acc2
            else acc1
      in
      let l1 = aux1 lst in
      aux2 l1 None None
    in
    let move_option =
      let len = List.length capture_moves in
      if len = 0 then random_element all_moves
      else take_the_best_capture_move capture_moves
    in
    let type_capture_option =
      match move_option with
      | None -> None
      | Some m -> type_capture_move board m player
    in
    Lwt.return
      (Some
         ( move_option,
           if type_capture_option = Some Both then
             if Random.int 2 = 0 then Some Approach else Some Withdrawal
           else type_capture_option ))
