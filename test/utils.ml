open Fanorona.Engine
open QCheck

let player =
  Alcotest.testable
    (Fmt.of_to_string (Format.asprintf "%a" pp_player))
    equal_player

let board =
  Alcotest.testable
    (Fmt.of_to_string (Format.asprintf "%a" pp_board))
    equal_board

let hpos =
  Alcotest.testable (Fmt.of_to_string (Format.asprintf "%a" pp_hpos)) equal_hpos

let vpos =
  Alcotest.testable (Fmt.of_to_string (Format.asprintf "%a" pp_vpos)) equal_vpos

let cell =
  Alcotest.testable (Fmt.of_to_string (Format.asprintf "%a" pp_cell)) equal_cell

let move =
  Alcotest.testable (Fmt.of_to_string (Format.asprintf "%a" pp_move)) equal_move

let board_1 =
  init
    [
      [ Pawn B; Empty; Pawn W; Empty; Empty; Empty; Pawn B; Pawn B; Pawn B ];
      [ Pawn B; Pawn W; Empty; Empty; Pawn W; Empty; Empty; Pawn B; Empty ];
      [ Empty; Empty; Empty; Empty; Empty; Pawn B; Empty; Pawn W; Pawn W ];
      [ Pawn W; Empty; Pawn W; Pawn B; Empty; Empty; Pawn W; Pawn W; Pawn W ];
      [ Empty; Pawn B; Empty; Pawn W; Empty; Pawn W; Pawn W; Pawn W; Pawn W ];
    ]

let board_2 =
  init
    [
      [ Empty; Empty; Pawn W; Empty; Empty; Empty; Empty; Empty; Empty ];
      [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
      [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Pawn W; Empty ];
      [ Pawn W; Empty; Empty; Empty; Empty; Empty; Pawn W; Pawn W; Pawn W ];
      [ Empty; Empty; Empty; Pawn W; Empty; Empty; Pawn W; Pawn W; Pawn W ];
    ]

let board_2_set0_1_B =
  init
    [
      [ Empty; Pawn B; Pawn W; Empty; Empty; Empty; Empty; Empty; Empty ];
      [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
      [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Pawn W; Empty ];
      [ Pawn W; Empty; Empty; Empty; Empty; Empty; Pawn W; Pawn W; Pawn W ];
      [ Empty; Empty; Empty; Pawn W; Empty; Empty; Pawn W; Pawn W; Pawn W ];
    ]

let board_3 =
  init
    [
      [ Pawn B; Pawn B; Pawn W; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B ];
      [ Pawn B; Pawn W; Empty; Pawn B; Pawn W; Empty; Pawn B; Pawn B; Pawn B ];
      [ Empty; Empty; Pawn B; Pawn B; Empty; Empty; Pawn W; Pawn B; Pawn W ];
      [ Pawn W; Pawn W; Pawn W; Empty; Pawn B; Pawn W; Pawn W; Pawn W; Pawn W ];
      [ Pawn W; Pawn W; Pawn W; Pawn W; Empty; Pawn W; Pawn W; Pawn W; Pawn W ];
    ]

let pre_capture =
  init
    [
      [ Pawn B; Pawn B; Pawn W; Empty; Empty; Empty; Pawn B; Empty; Empty ];
      [ Empty; Pawn B; Pawn B; Pawn W; Empty; Pawn B; Pawn W; Empty; Empty ];
      [ Pawn W; Empty; Pawn B; Empty; Pawn B; Empty; Empty; Pawn B; Empty ];
      [ Pawn B; Pawn W; Empty; Empty; Empty; Empty; Pawn W; Pawn W; Pawn B ];
      [ Empty; Empty; Pawn W; Empty; Pawn B; Pawn B; Empty; Pawn B; Empty ];
    ]

let generate_pos ?(h = Gen.int_bound 4) ?(v = Gen.int_bound 8) =
  let h_pos = h |> Gen.map Pos.h in
  let v_pos = v |> Gen.map Pos.v in
  Gen.map2 (fun hp vp -> (hp, vp)) h_pos v_pos

let generate_move =
  Gen.map2
    (fun pos dir -> { position = pos; direction = dir })
    generate_pos
    (Gen.oneofl [ N; NE; E; SE; S; SW; W; NW ])

let generate_move2 { position = h, v; direction = dir } =
  Gen.map2
    (fun pos dir -> { position = pos; direction = dir })
    (generate_pos ~h:(Gen.return (get_line h)) ~v:(Gen.return (get_col v)))
    (Gen.return dir)

let move_arbitrary = QCheck.make generate_move
let generate_player = Gen.oneof [ Gen.return B; Gen.return (W : player) ]
let player_arbitrary = make generate_player

let generate_cell
    ?(cell =
      Gen.oneof [ Gen.return Empty; Gen.map (fun p -> Pawn p) generate_player ])
    =
  cell

let generate_board =
  let height = Gen.return 5 in
  let width = Gen.return 9 in
  Gen.list_size height (Gen.list_size width generate_cell)

let board_arbitrary = make generate_board

let generate_init_state : cell list list Gen.t =
  let nb_cols = Gen.return 9 in
  let row_gen p = Gen.list_size nb_cols (Gen.return (Pawn p)) in
  let black_row_gen = row_gen B in
  let white_row_gen = row_gen W in
  let mid_row_gen =
    List.init 9 (fun i ->
        if i = 4 then Gen.return Empty
        else if (i mod 2 = 0 && i < 4) || (i > 4 && i mod 2 = 1) then
          Gen.return (Pawn B)
        else Gen.return (Pawn W))
    |> Gen.flatten_l
  in
  Gen.flatten_l
    [ black_row_gen; black_row_gen; mid_row_gen; white_row_gen; white_row_gen ]

let init_board_arbitrary = make generate_init_state
