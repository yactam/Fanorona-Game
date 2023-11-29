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
exception Invalid_position

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

let get_line (H i) = i
let get_col (V j) = j

(* Make sure pretty-printed notation can be parsed back by OCaml *)
let pp_hpos out_channel = function
  | H i -> Format.fprintf out_channel "Pos.(h %d)" i

let pp_vpos out_channel = function
  | V j -> Format.fprintf out_channel "Pos.(v %d)" j

type pos = hpos * vpos [@@deriving eq]

let pp_pos out_channel pos =
  let hp, vp = pos in
  Format.fprintf out_channel "(%a, %a)" pp_hpos hp pp_vpos vp

type dir = N | S | E | W | NE | SW | NW | SE
[@@deriving show { with_path = false }, eq]

let get_vect = function
  | N -> (-1, 0)
  | S -> (1, 0)
  | E -> (0, 1)
  | W -> (0, -1)
  | NE -> (-1, 1)
  | SW -> (1, -1)
  | NW -> (-1, -1)
  | SE -> (1, 1)

let rev_dir = function
  | N -> S
  | S -> N
  | E -> W
  | W -> E
  | NE -> SW
  | SW -> NE
  | NW -> SE
  | SE -> NW

type move = { position : pos; direction : dir } [@@deriving eq]

let pp_move out_c move =
  Format.fprintf out_c "{%a; %s}" pp_pos move.position (show_dir move.direction)

let destination_pos move =
  let H i, V j = move.position and i', j' = get_vect move.direction in
  try Some (Pos.h (i + i'), Pos.v (j + j')) with _ -> None

let get_destination_pos move =
  match destination_pos move with None -> raise Invalid_position | Some p -> p

type cell = Empty | Pawn of player [@@deriving eq]
type board = cell list list [@@deriving eq]

let init model =
  assert (List.length model = nb_rows);
  assert (List.for_all (fun row -> List.length row = nb_cols) model);
  assert (List.exists (fun row -> List.exists (fun cell -> cell = Empty) row) model);
  model

let initial_state_5x9 =
  let row p = List.init nb_cols (fun _ -> Pawn p) in
  let black_row = row B and white_row = row W in
  let mid_row =
    [ Pawn B; Pawn W; Pawn B; Pawn W; Empty; Pawn B; Pawn W; Pawn B; Pawn W ]
  in
  [ black_row; black_row; mid_row; white_row; white_row ]

let get board (H i) (V j) = List.nth (List.nth board i) j
let get2 board (H i, V j) = get board (H i) (V j)
(* currified version of get *)

let pp_cell out_c = function
  | Empty -> Format.fprintf out_c "."
  | Pawn p -> Format.fprintf out_c "%a" pp_player p

let pp_board out_c board =
  let row_sep = String.concat "" (List.init nb_cols (fun _ -> "+---")) in
  let print_row out_c row =
    List.init nb_cols Fun.id
    |> List.iter (fun j ->
           if (row + j) mod 2 = 0 then
             Format.fprintf out_c "| \x1B[1m%a\x1B[0m " pp_cell
               (get board (H row) (V j))
           else Format.fprintf out_c "| %a " pp_cell (get board (H row) (V j)))
  in
  Format.fprintf out_c "%s+" row_sep;
  Format.fprintf out_c "@,";
  List.iteri
    (fun i _ ->
      Format.fprintf out_c "%a|" print_row i;
      Format.fprintf out_c "@,";
      Format.fprintf out_c "%s+" row_sep;
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

let set2 board (H i, V j) p = set board (H i) (V j) p

let clear_cell board i j =
  board
  |> mapi (fun i' j' p' ->
         if equal_hpos i i' && equal_vpos j' j then Empty else p')

let clear_cell2 board (H i, V j) = clear_cell board (H i) (V j)

let free_cells board =
  List.init (nb_rows * nb_cols) (fun i ->
      (Pos.h (i / nb_cols), Pos.v (i mod nb_cols)))
  |> List.filter (fun (i, j) ->
         match get board i j with Empty -> true | _ -> false)

let win board player =
  List.flatten board
  |> List.filter (fun cell ->
         match cell with
         | Empty -> false
         | Pawn p when p = player -> false
         | _ -> true)
  |> List.is_empty

let is_diagonal_move move =
  let dir = move.direction in
  dir = SE || dir = SW || dir = NE || dir = NW

let is_valid_diagonal_move move =
  let H i, V j = move.position in
  is_diagonal_move move && (i + j) mod 2 = 0

(** check if the [move] destination is in the board and the destination cell is empty and the [move] position contains a [cell] of pawn [player] *)
let is_valid_move_position board move player =
  match destination_pos move with
  | None -> false
  | Some p ->
      let cell = get2 board move.position in
      let target = get2 board p in
      cell = Pawn player && target = Empty
      && ((not (is_diagonal_move move)) || is_valid_diagonal_move move)

let get_all_moves board player =
  let all_positions =
    List.init nb_rows (fun i -> List.init nb_cols (fun j -> (Pos.h i, Pos.v j)))
  in
  all_positions |> List.flatten
  |> List.map (fun pos ->
         let all_directions = [ N; S; E; W; NE; SW; NW; SE ] in
         all_directions
         |> List.filter (fun dir ->
                let move = { position = pos; direction = dir } in
                is_valid_move_position board move player)
         |> List.map (fun dir -> { position = pos; direction = dir }))
  |> List.flatten

type capture = Approach | Withdrawal | Both [@@deriving show, eq]

let type_capture_move board move player =
  if not (is_valid_move_position board move player) then None
  else
    let is_opponent_pawn p = get2 board p = Pawn (opponent player) in
    match destination_pos move with
    | None -> None
    | Some p -> (
        (* test if it's a capturing by approach or capturing by withdrawal *)
        let move' = { position = p; direction = move.direction } in
        let move'' =
          { position = move.position; direction = rev_dir move.direction }
        in
        match (destination_pos move', destination_pos move'') with
        | None, None -> None
        | None, Some p' -> if is_opponent_pawn p' then Some Withdrawal else None
        | Some p', None -> if is_opponent_pawn p' then Some Approach else None
        | Some p', Some p'' -> (
            match (is_opponent_pawn p', is_opponent_pawn p'') with
            | true, true -> Some Both
            | true, false -> Some Approach
            | false, true -> Some Withdrawal
            | false, false -> None))

(** check whether the move [move] executed by the player [player] on the board [board] is a capture move *)
let is_capture_move board move player =
  type_capture_move board move player <> None

let rec aux_capture board pos dir player =
  match get2 board pos with
  | Empty -> board
  | Pawn p ->
      if p = opponent player then
        let board' = clear_cell2 board pos in
        let pos' = destination_pos { position = pos; direction = dir } in
        match pos' with
        | None -> board'
        | Some p' -> aux_capture board' p' dir player
      else board

let make_capture_by_approach board move player =
  let dest_pos = get_destination_pos move in
  let opponent_pos =
    get_destination_pos { position = dest_pos; direction = move.direction }
  in
  let board' = aux_capture board opponent_pos move.direction player in
  let board' = clear_cell2 board' move.position in
  set2 board' (get_destination_pos move) player

let make_capture_by_withdrawal board move player =
  let dir_rev = rev_dir move.direction in
  let opponent_pos =
    get_destination_pos { position = move.position; direction = dir_rev }
  in
  let board' = aux_capture board opponent_pos dir_rev player in
  let board' = clear_cell2 board' move.position in
  set2 board' (get_destination_pos move) player

exception Compulsory_capture
exception Not_capture_by_approach
exception Not_capture_by_withdrawal
exception Choice_required
exception Capture_move_restrictions_broken

let position_or_direction_or_line_already_executed move_chain move =
  let same_position =
    match destination_pos move with
    | None -> false
    | Some p -> List.exists (fun m -> m.position = p) move_chain
  in
  let same_direction =
    (not (List.is_empty move_chain))
    && (List.hd move_chain).direction = move.direction
  in
  let same_line =
    let line_between_positions (H i, V j) (H i', V j') =
      if i = i' then
        List.init (abs (j - j') + 1) (fun k -> (H i, V (min j (j' + k))))
      else if j = j' then
        List.init (abs (i - i') + 1) (fun k -> (H (min i (i' + k)), V j))
      else if abs (i - i') = abs (j - j') then
        List.init
          (abs (i - i') + 1)
          (fun k -> (H (min i (i' + k)), V (min j (j' + k))))
      else []
    in
    match destination_pos move with
    | Some p ->
        let line = line_between_positions move.position p in
        List.exists
          (fun m ->
            match destination_pos m with
            | None -> false
            | Some p' -> line_between_positions m.position p' = line)
          move_chain
    | _ -> false
  in
  same_position || same_direction || same_line

let is_last_pawn_position_move move move_chain =
  try
    let last_move = List.hd move_chain in
    move.position = get_destination_pos last_move
  with _ -> false

let can_continue board player move move_chain =
  [ N; S; E; W; NE; SW; NW; SE ]
  |> List.filter (fun dir ->
         match destination_pos move with
         | None -> false
         | Some p ->
             let move' = { position = p; direction = dir } in
             is_valid_move_position board move' player
             && is_capture_move board move' player
             && not
                  (position_or_direction_or_line_already_executed move_chain
                     move'))
  |> List.is_empty |> not

let make_move board player move capture move_chain =
  if not (is_valid_move_position board move player) then raise Invalid_position
  else
    let all_moves = get_all_moves board player in
    match type_capture_move board move player with
    | None -> (
        let capture_moves =
          List.filter (fun m -> is_capture_move board m player) all_moves
        in
        if
          (not (is_capture_move board move player))
          && not (List.is_empty capture_moves)
        then raise Compulsory_capture
        else
          match destination_pos move with
          | None -> (board, move_chain)
          | Some p' ->
              let board_aux = clear_cell2 board move.position in
              (set2 board_aux p' player, move :: move_chain))
    | Some Approach ->
        if capture = Some Approach then
          if position_or_direction_or_line_already_executed move_chain move then
            raise Capture_move_restrictions_broken
          else (make_capture_by_approach board move player, move :: move_chain)
        else raise Not_capture_by_approach
    | Some Withdrawal ->
        if capture = Some Withdrawal then
          if position_or_direction_or_line_already_executed move_chain move then
            raise Capture_move_restrictions_broken
          else (make_capture_by_withdrawal board move player, move :: move_chain)
        else raise Not_capture_by_withdrawal
    | Some Both ->
        if capture = Some Approach then
          if position_or_direction_or_line_already_executed move_chain move then
            raise Capture_move_restrictions_broken
          else (make_capture_by_approach board move player, move :: move_chain)
        else if capture = Some Withdrawal then
          if position_or_direction_or_line_already_executed move_chain move then
            raise Capture_move_restrictions_broken
          else (make_capture_by_withdrawal board move player, move :: move_chain)
        else raise Choice_required
