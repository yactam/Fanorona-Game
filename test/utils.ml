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

let move =
  Alcotest.testable (Fmt.of_to_string (Format.asprintf "%a" pp_move)) equal_move

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

let pre_capture =
  init
    [
      [ Pawn B; Pawn B; Pawn W; Empty; Empty; Empty; Pawn B; Empty; Empty ];
      [ Empty; Pawn B; Pawn B; Pawn W; Empty; Pawn B; Pawn W; Empty; Empty ];
      [ Pawn W; Empty; Pawn B; Empty; Pawn B; Empty; Empty; Pawn B; Empty ];
      [ Pawn B; Pawn W; Empty; Empty; Empty; Empty; Pawn W; Pawn W; Pawn B ];
      [ Empty; Empty; Pawn W; Empty; Pawn B; Pawn B; Empty; Pawn B; Empty ];
    ]

let generate_dir = 
  let open QCheck in
  Gen.oneof (
    [N; S; E; W; NE; SE; NW; SW] 
    |> List.map (fun e -> Gen.return e)
  )

let dir_arbitrary = QCheck.make generate_dir

let generate_player =
  let open QCheck in
  Gen.oneof [Gen.return B; Gen.return (W : player)]

let player_arbitrary = QCheck.make generate_player

let generate_board_rand =
  let gen_line = List.init 9 (fun _ -> 
    match (Random.int 3) with
    | 0 -> Empty
    | 1 -> Pawn W
    | _ -> Pawn B)
  in List.init 5 (fun _ -> gen_line)

