open Engine

val botYanis :
  player -> board -> move list -> (move option * capture option) option Lwt.t
