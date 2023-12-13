open Engine

val player :
  player -> board -> move list -> (move option * capture option) option Lwt.t
