type player = W | B

type hpos
(** horizontal position (row) *)

type vpos = V of int
(** vertical position (column) *)

exception Invalid_horizontal_pos
exception Invalid_vertical_pos

val nb_rows : int
(** the number of rows of the game board *)

val nb_cols : int
(** the number of columns of the game board *)

(** Smart position constructor *)
module Pos : sig 
    val h : int -> hpos
    (** Convert an integer into an horizontal position. [raise Invalid_horizontal_pos] if the integer is out of the range values (0 to [nb_rows]-1) *)
    val v : int -> vpos 
    (** Convert an integer into a vertical position. [raise Invalid_vertical_pos] if the integer is out of the range values (0 to [nb_cols]-1) *)
end

val get_line : hpos -> int
val get_col : vpos -> int
val pp_hpos : Format.formatter -> hpos -> unit
val pp_vpos : Format.formatter -> vpos -> unit

type pos
(** A pair coordinates to represent a position in a 2D space *)

type dir
(** A direction to move in a 2D space *)

type cell
(** A cell in the game board *)

type board
(** A [nb_rows]-by-[nb_cols] representing the game board *)
