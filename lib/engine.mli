type player = W | B

val pp_player : Format.formatter -> player -> unit
val equal_player : player -> player -> bool
