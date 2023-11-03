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

let cell = 
  Alcotest.testable (Fmt.of_to_string (Format.asprintf "%a" pp_cell)) equal_cell

let board_1 =
  init 
    [
      [Pawn B;  None ; Pawn W;  None ;  None ;  None ; Pawn B; Pawn B; Pawn B];
      [Pawn B; Pawn W;  None ;  None ; Pawn W;  None ;  None ; Pawn B; None ;];
      [ None ;  None ;  None ;  None ;  None ; Pawn B;  None ; Pawn W; Pawn W];
      [Pawn W;  None ; Pawn W; Pawn B;  None ;  None ; Pawn W; Pawn W; Pawn W];
      [ None ; Pawn B;  None ; Pawn W;  None ; Pawn W; Pawn W; Pawn W; Pawn W]
    ]

let board_2 =
  init 
    [
      [ None ;  None ; Pawn W;  None ;  None ;  None ;  None ;  None ; None ;];
      [ None ; Pawn W;  None ;  None ; Pawn W;  None ;  None ;  None ; None ;];
      [ None ;  None ;  None ;  None ;  None ;  None ;  None ; Pawn W; Pawn W];
      [Pawn W;  None ; Pawn W;  None ;  None ;  None ; Pawn W; Pawn W; Pawn W];
      [ None ;  None ;  None ; Pawn W;  None ; Pawn W; Pawn W; Pawn W; Pawn W]
    ]

let board_2_set0_1_B = 
  init 
  [
      [Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B];
      [Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B; Pawn B];
      [Pawn B; Pawn B; Pawn B; Pawn B; None  ; Pawn W; Pawn W; Pawn W; Pawn W];
      [Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W];
      [Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W; Pawn W]
    ]
