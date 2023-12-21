open Engine

val my_ia2 :
  player -> board -> move list -> (move option * capture option) option Lwt.t

val my_ia :
  player -> board -> move list -> (move option * capture option) option Lwt.t
