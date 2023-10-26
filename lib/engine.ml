(** A player is either the white [W] player or the black player [B] *)
type player = W | B [@@deriving show { with_path = false }, eq]

let opponent = function W -> B | B -> W

(** where [int] indicates a row number *)
type hpos = H of int [@@deriving eq]

(** where [int] indicates a column number *)
type vpos = V of int [@@deriving eq]

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

(** extract the line number from a valid [hpos] *)
let get_line (H i) = i

(** extract the column number from a valid [vpos] *)
let get_col (V j) = j

(* Make sure pretty-printed notation can be parsed back by OCaml *)
let pp_hpos out_channel = function
  | H i -> Format.fprintf out_channel "Pos.(h %d)" i

let pp_vpos out_channel = function
  | V j -> Format.fprintf out_channel "Pos.(v %d)" j

type pos = hpos * vpos
(** A pair of coordinates that combines an [hpos] and a [vpos] to represents positions on the board *)

type dir =
  | N
  | S
  | E
  | W
  | NE
  | SW
  | NW
  | SE
      (** A direction is one of: North [N], South [S], East [E], West [W], North-East [NE], South-West [SW], North-West [NW], South-East [SE]. *)

(** A cell in the board is either [Empty] or containing a [Pawn] of [Player] player  *)
type cell = Empty | Pawn of player [@@deriving eq]

type board = cell list list [@@deriving eq]
(** The game board is represented by a list of list of [cell]s *)

(** Check wether [model] is a valid game board *)
let init model =
  assert (List.length model = nb_rows);
  assert (List.for_all (fun row -> List.length row = nb_cols) model);
  model

(** The initial state of a classic fanorana game board *)
let initial_state_5x9 =
  let row p = List.init nb_cols (fun _ -> Pawn p) in
  let black_row = row B and white_row = row W in
  let mid_row =
    [ Pawn B; Pawn W; Pawn B; Pawn W; Empty; Pawn B; Pawn W; Pawn B; Pawn W ]
  in
  [ black_row; black_row; mid_row; white_row; white_row ]

(** Get from the board the [cell] at position([i], [j]) *)
let get board (H i) (V j) = List.nth (List.nth board i) j

let pp_cell out_c = function
  | Empty -> Format.fprintf out_c "."
  | Pawn p -> Format.fprintf out_c "%a" pp_player p

let pp_board out_c board =
  let row_sep = String.concat "" (List.init nb_cols (fun _ -> "+---+")) in
  let print_row out_c row =
    List.init nb_cols Fun.id
    |> List.iter (fun j ->
           Format.fprintf out_c "| %a " pp_cell (get board (H row) (V j)))
  in
  Format.fprintf out_c "%s" row_sep;
  Format.fprintf out_c "@,";
  List.iteri
    (fun i _ ->
      Format.fprintf out_c "%a|" print_row i;
      Format.fprintf out_c "@,";
      Format.fprintf out_c "%s" row_sep;
      Format.fprintf out_c "@,")
    board

let mapi f board =
  board
  |> List.mapi (fun i row -> row |> List.mapi (fun j v -> f (H i) (V j) v))

let iteri f board =
  board
  |> List.iteri (fun i row -> row |> List.iteri (fun j v -> f (H i) (V j) v))

exception Occupied_cell

(** Set the cell at position ([i], [j]) with the value [p] *)
let set board i j p =
  board
  |> mapi (fun i' j' p' ->
         if equal_hpos i' i && equal_vpos j' j then
           match p' with Empty -> Pawn p | _ -> raise Occupied_cell
         else p')

(** Return a [(hpos * vpos) list] of free cells in the board [board] *)
let free_cells board =
  List.init (nb_rows * nb_cols) (fun i ->
      (Pos.h (i / nb_cols), Pos.v (i mod nb_rows)))
  |> List.filter (fun (i, j) ->
         match get board i j with Empty -> true | _ -> false)

(** Check wether the player [player] has won according to [board] *)
let win board player =
  List.flatten board
  |> List.filter (fun cell ->
         match cell with
         | Empty -> false
         | Pawn p when p = player -> false
         | _ -> true)
  |> List.length = 0
