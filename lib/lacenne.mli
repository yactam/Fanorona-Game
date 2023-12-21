open Engine

val bot :
  player -> board -> move list -> (move option * capture option) option Lwt.t
