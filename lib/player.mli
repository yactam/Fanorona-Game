open Engine

val player_lacenne :
  player -> board -> move list -> (move option * capture option) option Lwt.t
