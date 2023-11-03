type player = W | B

val pp_player : Format.formatter -> player -> unit
val show_player : player -> string
val equal_player : player -> player -> bool
val opponent : player -> player

type hpos = H of int

val equal_hpos : hpos -> hpos -> bool

type vpos = V of int

val equal_vpos : vpos -> vpos -> bool

exception Invalid_horizontal_pos
exception Invalid_vertical_pos

val nb_rows : int
val nb_cols : int

module Pos : sig
  val h : int -> hpos
  val v : int -> vpos
end

val get_line : hpos -> int
val get_col : vpos -> int
val pp_hpos : Format.formatter -> hpos -> unit
val pp_vpos : Format.formatter -> vpos -> unit

type pos = hpos * vpos
type dir = N | S | E | W | NE | SW | NW | SE

val get_vect : dir -> int * int
val rev_dir : dir -> dir

type move = { position : pos; direction : dir }
type cell = Empty | Pawn of player

val equal_cell : cell -> cell -> bool
val pp_pos : Format.formatter -> pos -> unit
val pp_move : Format.formatter -> move -> unit

type board = cell list list

val equal_board : board -> board -> bool
val init : 'a list list -> 'a list list
val initial_state_5x9 : cell list list
val get : 'a list list -> hpos -> vpos -> 'a
val pp_cell : Format.formatter -> cell -> unit
val pp_board : Format.formatter -> cell list list -> unit
val mapi : (hpos -> vpos -> 'a -> 'b) -> 'a list list -> 'b list list
val iteri : (hpos -> vpos -> 'a -> unit) -> 'a list list -> unit

exception Occupied_cell

val set : cell list list -> hpos -> vpos -> player -> cell list list
val free_cells : cell list list -> (hpos * vpos) list
val win : cell list list -> player -> bool
val is_valid_move_position: board -> move -> player -> bool
