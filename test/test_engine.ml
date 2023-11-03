open Fanorona.Engine
open Utils

let board_1_get res i j () =
  Alcotest.(check (cell))
    "same result" res
    (get board_1 (H i) (V j))

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
        ]);
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
      ("set",
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

        ]);
      ("free_cells",
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
               (Pos.(h 1), Pos.(v 2)); (Pos.(h 1), Pos.(v 5)); 
               (Pos.(h 2), Pos.(v 0)); (Pos.(h 2), Pos.(v 1)); (Pos.(h 2), Pos.(v 4)); (Pos.(h 2), Pos.(v 5));
               (Pos.(h 3), Pos.(v 3));
               (Pos.(h 4), Pos.(v 4))]
              (free_cells board_3));
        ]);
      ("win",
        [
          test_case "starting_board-not-win-B" `Quick (fun () ->
            Alcotest.(check bool) "same result" false (win initial_state_5x9 B));
          test_case "board_1-not-win-W" `Quick (fun () ->
            Alcotest.(check bool) "same result" false (win board_1 W));
          test_case "board_2-win-W" `Quick (fun () ->
            Alcotest.(check bool) "same result" true (win board_2 W));
          test_case "board_2_comp-not-win-W" `Quick (fun () ->
            Alcotest.(check bool) "same result" false (win board_2_set0_1_B W));
          test_case "board_3-not-win-B" `Quick (fun () ->
            Alcotest.(check bool) "same result" false (win board_3 B));

        ]);
    ]