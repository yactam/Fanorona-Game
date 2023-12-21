open Engine

val minimax_player :
  player -> board -> move list -> (move option * capture option) option Lwt.t
(** A bot to generate moves against intelligent bots *)
