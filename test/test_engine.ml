open Fanorona.Engine
open Utils

let board_1_get res i j () =
  Alcotest.(check cell) "same result" res (get board_1 (H i) (V j))

let () =
  let open Alcotest in
  run "Engine"
    [
      ( "init-get",
        [
          test_case "board_1-get-0-1" `Quick (board_1_get Empty 0 1);
          test_case "board_1-get-2-5" `Quick (board_1_get (Pawn B) 2 5);
          test_case "board_1-get-3-7" `Quick (board_1_get (Pawn W) 3 7);
          test_case "board_1-get-2-1" `Quick (board_1_get Empty 2 1);
          test_case "board_1-get-4-4" `Quick (board_1_get Empty 4 4);
        ] );
      (*( "pp",
        [
          test_case "board_1-pp" `Quick (fun () ->
            Alcotest.(check string)
              "same result" ""
              (Format.asprintf "@[<h>%a@]" pp_board board_1));
          test_case "board_2-pp" `Quick (fun () ->
            Alcotest.(check string)
              "same result" ""
              (Format.asprintf "@[<v>%a@]" pp_board board_2));
        ]);*)
      ( "set",
        [
          test_case "board_1-set-fail" `Quick (fun () ->
              Alcotest.(check_raises) "Occupied cell" Occupied_cell (fun () ->
                  ignore (set board_1 (H 4) (V 1) W)));
          test_case "board_2-set-success" `Quick (fun () ->
              Alcotest.(check cell)
                "same result" (Pawn B)
                (get (set board_2 (H 0) (V 1) B) (H 0) (V 1)));
          test_case "board_2_compare" `Quick (fun () ->
              Alcotest.(check board)
                "same result" board_2_set0_1_B
                (set board_2 (H 0) (V 1) B));
        ] );
      ( "free_cells",
        [
          test_case "starting-board-free-cells" `Quick (fun () ->
              Alcotest.(check (list (pair hpos vpos)))
                "same result"
                [ (Pos.(h 2), Pos.(v 4)) ]
                (free_cells initial_state_5x9));
          test_case "board_3-free-cells" `Quick (fun () ->
              Alcotest.(check (list (pair hpos vpos)))
                "same result"
                [
                  (Pos.(h 1), Pos.(v 2));
                  (Pos.(h 1), Pos.(v 5));
                  (Pos.(h 2), Pos.(v 0));
                  (Pos.(h 2), Pos.(v 1));
                  (Pos.(h 2), Pos.(v 4));
                  (Pos.(h 2), Pos.(v 5));
                  (Pos.(h 3), Pos.(v 3));
                  (Pos.(h 4), Pos.(v 4));
                ]
                (free_cells board_3));
        ] );
      ( "win",
        [
          test_case "starting_board-not-win-B" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false (win initial_state_5x9 B));
          test_case "board_1-not-win-W" `Quick (fun () ->
              Alcotest.(check bool) "same result" false (win board_1 W));
          test_case "board_2-win-W" `Quick (fun () ->
              Alcotest.(check bool) "same result" true (win board_2 W));
          test_case "board_2_comp-not-win-W" `Quick (fun () ->
              Alcotest.(check bool) "same result" false (win board_2_set0_1_B W));
          test_case "board_3-not-win-B" `Quick (fun () ->
              Alcotest.(check bool) "same result" false (win board_3 B));
        ] );
      ( "is_valid_move_position",
        [
          test_case "successful linear move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (is_valid_move_position initial_state_5x9
                   { position = (Pos.(h 3), Pos.(v 4)); direction = N }
                   W));
          test_case "successful diagonal move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (is_valid_move_position initial_state_5x9
                   { position = (Pos.(h 3), Pos.(v 3)); direction = NE }
                   W));
          test_case "incorrect pawn player move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (is_valid_move_position initial_state_5x9
                   { position = (Pos.(h 1), Pos.(v 4)); direction = S }
                   W));
          test_case "move empty cell" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (is_valid_move_position initial_state_5x9
                   { position = (Pos.(h 2), Pos.(v 4)); direction = N }
                   W));
          test_case "move into pawn" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (is_valid_move_position initial_state_5x9
                   { position = (Pos.(h 3), Pos.(v 4)); direction = S }
                   W));
          test_case "diagonal move on non diagonal pos" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (is_valid_move_position board_1
                   { position = (Pos.(h 1), Pos.(v 0)); direction = SE }
                   B));
        ] );
      ( "is_capture_move",
        [
          test_case "(3, 4) to (2,4) starter inward move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (is_capture_move initial_state_5x9
                   { position = (Pos.(h 3), Pos.(v 4)); direction = N }
                   W));
          test_case "(3,3) to (2,4) starter inward move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (is_capture_move initial_state_5x9
                   { position = (Pos.(h 3), Pos.(v 3)); direction = NE }
                   W));
          test_case "outward capture move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (is_capture_move board_1
                   { position = (Pos.(h 1), Pos.(v 1)); direction = E }
                   W));
          test_case "non capture move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (is_capture_move board_1
                   { position = (Pos.(h 3), Pos.(v 0)); direction = E }
                   W));
          test_case "invalid move into pawn capture move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (is_capture_move board_3
                   { position = (Pos.(h 2), Pos.(v 2)); direction = S }
                   B));
        ] );
      ( "get_all_moves",
        [
          test_case "starter moves white" `Quick (fun () ->
              Alcotest.(check (list move))
                "same result"
                [
                  { position = (Pos.(h 3), Pos.(v 4)); direction = N };
                  { position = (Pos.(h 3), Pos.(v 3)); direction = NE };
                  { position = (Pos.(h 3), Pos.(v 5)); direction = NW };
                  { position = (Pos.(h 2), Pos.(v 3)); direction = E };
                ]
                (get_all_moves initial_state_5x9 W));
          test_case "one valid capture move, one invalid escape move" `Quick
            (fun () ->
              Alcotest.(check (list move))
                "same result"
                [ { position = (Pos.(h 0), Pos.(v 1)); direction = W } ]
                (get_all_moves board_2_set0_1_B B));
          test_case "no capture moves" `Quick (fun () ->
              Alcotest.(check (list move))
                "same result"
                [
                  { position = (Pos.(h 0), Pos.(v 4)); direction = N };
                  { position = (Pos.(h 0), Pos.(v 4)); direction = E };
                  { position = (Pos.(h 0), Pos.(v 4)); direction = S };
                  { position = (Pos.(h 2), Pos.(v 0)); direction = W };
                  { position = (Pos.(h 2), Pos.(v 0)); direction = SW };
                  { position = (Pos.(h 2), Pos.(v 0)); direction = S };
                  { position = (Pos.(h 2), Pos.(v 0)); direction = SE };
                  { position = (Pos.(h 2), Pos.(v 0)); direction = E };
                  { position = (Pos.(h 4), Pos.(v 3)); direction = E };
                  { position = (Pos.(h 4), Pos.(v 3)); direction = N };
                  { position = (Pos.(h 4), Pos.(v 3)); direction = W };
                  { position = (Pos.(h 3), Pos.(v 6)); direction = N };
                  { position = (Pos.(h 3), Pos.(v 6)); direction = W };
                  { position = (Pos.(h 4), Pos.(v 6)); direction = NW };
                  { position = (Pos.(h 3), Pos.(v 6)); direction = W };
                  { position = (Pos.(h 2), Pos.(v 7)); direction = N };
                  { position = (Pos.(h 2), Pos.(v 7)); direction = W };
                  { position = (Pos.(h 3), Pos.(v 7)); direction = NW };
                  { position = (Pos.(h 3), Pos.(v 7)); direction = NE };
                  { position = (Pos.(h 3), Pos.(v 8)); direction = N };
                ]
                (get_all_moves board_2 W));
          test_case "no pawns" `Quick (fun () ->
              Alcotest.(check (list move))
                "same result" [] (get_all_moves board_2 B));
        ] );
    ]
