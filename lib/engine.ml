type player = W | B [@@deriving show { with_path = false }, eq]
(** A player is either the white [W] player or the black player [B] *)

type hpos = H of int [@@deriving eq] (** where [int] indicates a row number *)
type vpos = V of int [@@deriving eq] (** where [int] indicates a column number *)
(* The [hpos] and [vpos] types are defined representing horizontal and vertica positions and are limited to a specific range of values (0 to [nb_lines]-1) for rows
and (0 to [nb_clos]-1) for cols and module [Pos] is a guarantee of creating only valid instances of these types *)

exception Invalid_horizontal_pos
exception Invalid_vertical_pos

let nb_rows = 5
let nb_cols = 9

module Pos = struct
        let h i =
                if i < 0 || i >= nb_rows then raise Invalid_horizontal_pos;
                H i

        let v j =
                if j < 0 || j >= nb_cols then raise Invalid_vertical_pos;
                V j 
end

let get_line (H i) = i (** extract the line number from a valid [hpos] *)
let get_col (V j) = j (** extract the column number from a valid [vpos] *)

(* Make sure pretty-printed notation can be parsed back by OCaml *)
let pp_hpos out_channel = function H i -> Format.fprintf out_channel "Pos.(h %d)" i
let pp_vpos out_channel = function V j -> Format.fprintf out_channel "Pos.(v %d)" j

type pos = hpos * vpos 
(** A pair of coordinates that combines an [hpos] and a [vpos] to represents positions on the board *)

type dir = N | S | E | W | NE | SW | NW | SE
(** A direction is one of: North [N], South [S], East [E], West [W], North-East [NE], South-West [SW], North-West [NW], South-East [SE]. *)

type cell = Empty | Pawn of player [@@deriving eq]
(** A cell in the board is either [Empty] or containing a [Pawn] of [Player] player  *)

type board = cell list list [@@deriving eq]
(** The game board is represented by a list of list of [cell]s *)
