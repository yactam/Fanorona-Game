open Engine

type trace = move list
(** Game history *)

val pp_trace : Format.formatter -> trace -> unit
val equal_trace : trace -> trace -> bool

type endgame =
  | Win of player
  | Giveup of player  (** Possible scenarios of a game end *)

val equal_endgame : endgame -> endgame -> bool
val pp_endgame : Format.formatter -> endgame -> unit

type result = { trace : trace; endgame : endgame; final : board }
(** Resulted state of the game *)

val arena :
  ?init_player:player ->
  ?init_board:board ->
  (player -> board -> move list -> (move option * capture option) option Lwt.t) ->
  result Lwt.t
(** [arena ~init_player ~init_board players] simulates a game between
    [player W] and [player B]. By default, [init_player] is [W]
    while [init_board] is [initial_state_5x9]. *)

val players_trace : trace -> player -> board -> move option Lwt.t
(** Simulate a trace as a player *)

val pair :
  w:(board -> move list -> (move option * capture option) option Lwt.t) ->
  b:(board -> move list -> (move option * capture option) option Lwt.t) ->
  player ->
  board ->
  move list ->
  (move option * capture option) option Lwt.t

val player_teletype :
  player -> board -> move list -> (move option * capture option) option Lwt.t
(** Generate moves based of the computer user choice through terminal I/O *)

val player_giveup :
  board -> move list -> (move option * capture option) option Lwt.t
(** Always fail to generate a move *)

val player_random :
  player -> board -> move list -> (move option * capture option) option Lwt.t
(** A bot to generate moves *)
