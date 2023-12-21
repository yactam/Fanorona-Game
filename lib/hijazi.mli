open Engine

val custom_player : player -> board -> move list -> (move option * capture option) option Lwt.t
