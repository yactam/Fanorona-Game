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
  let rec go board player trace move_chain =
    let opponent = swap player in
    if win board opponent then
      Lwt.return
        { trace = List.rev trace; endgame = Win opponent; final = board }
    else
      let* r = players player board in
      match r with
      | Some (None, None) | Some (None, _) -> go board player trace move_chain
      | None ->
          Lwt.return
            { trace = List.rev trace; endgame = Giveup player; final = board }
      | Some (Some move, capture_option) -> (
          let is_capture = capture_option <> None in
          if
            (not (List.is_empty move_chain))
            && not (is_last_pawn_position_move move move_chain)
          then go board player trace move_chain
          else
            try
              let board', move_chain' =
                make_move board player move capture_option move_chain
              in
              if is_capture && can_continue board' player move move_chain' then
                go board' player (move :: trace) move_chain'
              else go board' opponent (move :: trace) []
            with e ->
              Format.printf "%s@," (Printexc.to_string e);
              go board player trace move_chain)
  in
  go init_board init_player [] []

let player_giveup _board = Lwt.return None

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
    Format.printf "Not a valid number please again: %!";
    player_teletype_get_int ()

let rec player_teletype_get_pos () =
  try
    Format.printf "Enter row number: ";
    Format.printf "%!";
    let row = player_teletype_get_int () in
    Format.printf "Enter column number: ";
    Format.printf "%!";
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
    Format.printf "Enter direction (N, S, E, W, NE, SW, NW, SE): ";
    Format.printf "%!";
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

let player_teletype player board =
  Format.printf "@[<v>Player %a to play.@," pp_player player;
  Format.printf "Board: @[<v>%a@]@." pp_board board;
  Format.printf "Select a position: ";
  let pos = player_teletype_get_pos () in
  Format.printf "Select a direction: ";
  let dir = player_teletype_get_dir () in
  let move = { position = pos; direction = dir } in
  let type_capture = type_capture_move board move player in
  match type_capture with
  | Some Approach | Some Withdrawal | None ->
      Lwt.return (Some (Some move, type_capture))
  | Some Both ->
      Lwt.return (Some (Some move, player_teletype_get_capture_type ()))

