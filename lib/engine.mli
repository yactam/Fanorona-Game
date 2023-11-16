type player = W | B

val pp_player : Format.formatter -> player -> unit
val equal_player : player -> player -> bool
val opponent : player -> player

type hpos = H of int
type vpos = V of int

val equal_hpos : hpos -> hpos -> bool
val equal_vpos : vpos -> vpos -> bool

exception Invalid_horizontal_pos
exception Invalid_vertical_pos
exception Invalid_position

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

val equal_pos : pos -> pos -> bool
val pp_pos : Format.formatter -> hpos * vpos -> unit

type dir = N | S | E | W | NE | SW | NW | SE

val pp_dir : Format.formatter -> dir -> unit
val equal_dir : dir -> dir -> bool

type move = { position : pos; direction : dir }

val equal_move : move -> move -> bool
val pp_move : Format.formatter -> move -> unit

type cell = Empty | Pawn of player

val pp_cell : Format.formatter -> cell -> unit
val equal_cell : cell -> cell -> bool

type board = cell list list

val equal_board : board -> board -> bool
val init : board -> board
val initial_state_5x9 : board
val pp_board : Format.formatter -> board -> unit
val get : board -> hpos -> vpos -> cell
val iteri : (hpos -> vpos -> cell -> unit) -> board -> unit

exception Occupied_cell

val free_cells : board -> (hpos * vpos) list
val win : board -> player -> bool
val get_all_moves : board -> player -> move list

type capture = Approach | Withdrawal | Both

val pp_capture : Format.formatter -> capture -> unit
val equal_capture : capture -> capture -> bool
val type_capture_move : board -> move -> player -> capture option

exception Compulsory_capture
exception Not_capture_by_approach
exception Not_capture_by_withdrawal
exception Choice_required
exception Capture_move_restrictions_broken

val position_or_direction_or_line_already_executed : move list -> move -> bool
val can_continue : board -> player -> move -> move list -> bool
val is_last_pawn_position_move : move -> move list -> bool

val make_move :
  board -> player -> move -> capture option -> move list -> board * move list
