open Fanorona.Engine

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

let board_3x3 =
  init 
    [
      [Pawn B; Pawn B; Pawn B];
      [Pawn B; None  ; Pawn W];
      [Pawn W; Pawn W; Pawn W]
    ]

let board_5x5 =
  init 
    [
      [Pawn B; Pawn B; Pawn B; Pawn B; Pawn B];
      [Pawn B; Pawn B; Pawn B; Pawn B; Pawn B];
      [Pawn B; Pawn B; None  ; Pawn W; Pawn W];
      [Pawn W; Pawn W; Pawn W; Pawn W; Pawn W];
      [Pawn W; Pawn W; Pawn W; Pawn W; Pawn W]
    ]

let board_9x9 = 
  init 
  [
      [Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B];
      [Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B];
      [Pawn B; Pawn B; Pawn B; Pawn B; None  ; Pawn W; Pawn W; Pawn W; Pawn W];
      [Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W];
      [Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W]
    ]