open Engine

type trace = move list

val pp_trace : Format.formatter -> trace -> unit
val equal_trace : trace -> trace -> bool

type endgame = Win of player | Giveup of player

val equal_endgame : endgame -> endgame -> bool
val pp_endgame : Format.formatter -> endgame -> unit

type result = { trace : trace; endgame : endgame; final : board }

val arena :
  ?init_player:player ->
  ?init_board:board ->
  (player -> board -> (move option * capture option) option Lwt.t) ->
  result Lwt.t

val players_trace : trace -> player -> board -> move option Lwt.t

val pair :
  w:(board -> (move option * capture option) option Lwt.t) ->
  b:(board -> (move option * capture option) option Lwt.t) ->
  player ->
  board ->
  (move option * capture option) option Lwt.t

val player_teletype :
  player -> board -> (move option * capture option) option Lwt.t

val player_giveup : board -> (move option * capture option) option Lwt.t

val player_random :
  player -> board -> (move option * capture option) option Lwt.t
