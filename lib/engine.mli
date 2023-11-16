type player = W | B
(** A player is either the white [W] player or the black player [B] *)

val pp_player : Format.formatter -> player -> unit
val equal_player : player -> player -> bool
val opponent : player -> player
(** Get the opponent of a [player] *)

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
  (** Convert an integer into an horizontal position. [raise
      Invalid_horizontal_pos] if the integer is out of [[0; 1; ..; [nb_rows]] *)
  val v : int -> vpos
      (** Convert an integer into an horizontal position. [raise
      Invalid_vertical_pos] if the integer is out of [[0; 1; ..; [nb_rows]] *)
end
(** Smart position constructors *)

val get_line : hpos -> int
(** Extract the line number from a valid [hpos] *)
val get_col : vpos -> int
(** Extract the column number from a valid [vpos] *)
val pp_hpos : Format.formatter -> hpos -> unit
val pp_vpos : Format.formatter -> vpos -> unit

type pos = hpos * vpos
(** A pair of coordinates that combines an [hpos] and a [vpos] to represents positions on the board *)

val equal_pos : pos -> pos -> bool
val pp_pos : Format.formatter -> hpos * vpos -> unit

type dir = N | S | E | W | NE | SW | NW | SE
(** A direction is one of: North [N], South [S], East [E], West [W], North-East [NE], South-West [SW], North-West [NW], South-East [SE]. 
    (W : dir) is not the same as (W : player) !*)

val pp_dir : Format.formatter -> dir -> unit
val equal_dir : dir -> dir -> bool

type move = { position : pos; direction : dir }
(** A move is represented by a starting position and a direction *)

val equal_move : move -> move -> bool
val pp_move : Format.formatter -> move -> unit

type cell = Empty | Pawn of player
(** A cell in the board is either [Empty] or containing a [Pawn] of [Player] player  *)

val pp_cell : Format.formatter -> cell -> unit
val equal_cell : cell -> cell -> bool

type board = cell list list
(** The game board is represented by a list of list of [cell]s *)

val equal_board : board -> board -> bool
val init : board -> board
(** Check wether [model] is a valid game board and initialize it *)
val initial_state_5x9 : board
(** The initial state of a classic fanorana game board *)
val pp_board : Format.formatter -> board -> unit
val get : board -> hpos -> vpos -> cell
(** Get from the board the [cell] at position([i], [j]) *)
val iteri : (hpos -> vpos -> cell -> unit) -> board -> unit
(** Iterates the function [hpos -> vpos -> cell -> unit] over the board [board] *)

exception Occupied_cell

val free_cells : board -> (hpos * vpos) list
(** Return a [(hpos * vpos) list] of free cells in the board [board] *)
val win : board -> player -> bool
(** Check wether the player [player] has won according to [board] *)
val get_all_moves : board -> player -> move list
(** Get a list of all possible moves of player [player] on board [board] but not some of them may be illegal in a specific game *)

type capture = Approach | Withdrawal | Both
(** A capture may be by [Approach], by [Withdrawal] or [Both] where the player have to make a choice
    of the type of the capture that he want to execute *)

val pp_capture : Format.formatter -> capture -> unit
val equal_capture : capture -> capture -> bool
val type_capture_move : board -> move -> player -> capture option
(** check whether the move [move] executed by the player [player] on the board [board] is a capture move 
    and get the [capture] type of the move in this case else [None] *)

exception Compulsory_capture
exception Not_capture_by_approach
exception Not_capture_by_withdrawal
exception Choice_required
exception Capture_move_restrictions_broken

val position_or_direction_or_line_already_executed : move list -> move -> bool
(** Check if the [move] satisfies the three move constraints in multiple capturing moves
    which are stored in the [move list] *)
val can_continue : board -> player -> move -> move list -> bool
(** Check if [player] can continue capturing on the [board] based on his last [move] 
    and his history of captures [move list] *)
val is_last_pawn_position_move : move -> move list -> bool
(** Check if the [move] uses the same pawn as the last move in [move list] *)

val make_move :
  board -> player -> move -> capture option -> move list -> board * move list
(** Execute the [move] of [player] on the [board] based on [move list] history of 
    [player] if he is executing a multiple capture and take a [capture option] if 
    the move is a capture, returns the new board and the updated history [move list]
    [raise Invalid_position] if the destination position of the move is invalid ([Occupied_Cell] or out of the board)
    [raise Compulsory_capture] if the [move] is not a capture and the [player] have capture moves 
    [raise Not_capture_by_approach] or [raise Not_capture_by_withdrawal] if the capture specified in the [capture option] don't fit 
    the type of the capture of the [move]
    [raise Choice_required] if the [player] has both type captures but he didn't specify the type 
    of the capture he wants to execute
    [raise Capture_move_restrictions_broken] if the [move] brokes a restriction of multiple capture moves
    *)
