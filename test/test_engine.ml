open Fanorona.Engine
open Utils

let board_1_get res i j () =
  Alcotest.(check cell) "same result" res (get board_1 (Pos.h i) (Pos.v j))

let make_flat_pawn board =
  board |> List.flatten
  |> List.filter (fun cell -> match cell with Empty -> false | _ -> true)

let is_capture_move_test =
  let open QCheck in
  Test.make ~count:1000 ~name:"for all capture move, board size before > after"
    (pair player_arbitrary board_arbitrary) (fun (player, board) ->
      Format.printf "%a" pp_board board;
      Format.printf "%a" pp_player player;
      let moves = get_all_moves board player in
      let move = List.nth moves (Random.int (List.length moves)) in
      Format.printf "%a" pp_move move;
      let capture_type = type_capture_move board move player in
      let new_board =
        Format.printf "No failure yet.\n";
        match capture_type with
        | None -> board
        | Some Both | Some Approach ->
            fst (make_move board player move (Some Approach) [])
        | Some Withdrawal ->
            fst (make_move board player move (Some Withdrawal) [])
      in
      Format.printf "%a" pp_board new_board;
      capture_type <> None
      && List.length (make_flat_pawn board)
         > List.length (make_flat_pawn new_board)
      || capture_type == None
         && List.length (make_flat_pawn board)
            == List.length (make_flat_pawn new_board))

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
                (try
                   let _ =
                     make_move initial_state_5x9 W
                       { position = (Pos.(h 3), Pos.(v 4)); direction = N }
                       None []
                   in
                   true
                 with
                | Invalid_position -> false
                | _ -> true));
          test_case "successful diagonal move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (try
                   let _ =
                     make_move initial_state_5x9 W
                       { position = (Pos.(h 3), Pos.(v 3)); direction = NE }
                       None []
                   in
                   true
                 with
                | Invalid_position -> false
                | _ -> true));
          test_case "incorrect pawn player move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (try
                   let _ =
                     make_move initial_state_5x9 W
                       { position = (Pos.(h 1), Pos.(v 4)); direction = S }
                       None []
                   in
                   true
                 with
                | Invalid_position -> false
                | _ -> true));
          test_case "move empty cell" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (try
                   let _ =
                     make_move initial_state_5x9 W
                       { position = (Pos.(h 2), Pos.(v 4)); direction = N }
                       None []
                   in
                   true
                 with
                | Invalid_position -> false
                | _ -> true));
          test_case "move into pawn" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (try
                   let _ =
                     make_move initial_state_5x9 W
                       { position = (Pos.(h 3), Pos.(v 4)); direction = S }
                       None []
                   in
                   true
                 with
                | Invalid_position -> false
                | _ -> true));
          test_case "diagonal move on non diagonal pos" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (try
                   let _ =
                     make_move board_1 B
                       { position = (Pos.(h 1), Pos.(v 0)); direction = SE }
                       None []
                   in
                   true
                 with
                | Invalid_position -> false
                | _ -> true));
        ] );
      ( "is_capture_move",
        [
          test_case "(3, 4) to (2,4) starter inward move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (type_capture_move initial_state_5x9
                   { position = (Pos.(h 3), Pos.(v 4)); direction = N }
                   W
                <> None));
          test_case "(3,3) to (2,4) starter inward move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (type_capture_move initial_state_5x9
                   { position = (Pos.(h 3), Pos.(v 3)); direction = NE }
                   W
                <> None));
          test_case "outward capture move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (type_capture_move board_1
                   { position = (Pos.(h 1), Pos.(v 1)); direction = E }
                   W
                <> None));
          test_case "non capture move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (type_capture_move board_1
                   { position = (Pos.(h 3), Pos.(v 0)); direction = E }
                   W
                <> None));
          test_case "invalid move into pawn capture move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (type_capture_move board_3
                   { position = (Pos.(h 2), Pos.(v 2)); direction = S }
                   B
                <> None));
          QCheck_alcotest.to_alcotest is_capture_move_test;
        ] );
      ( "get_all_moves",
        [
          test_case "starter moves white" `Quick (fun () ->
              Alcotest.(check (list move))
                "same result"
                [
                  { position = (Pos.(h 2), Pos.(v 3)); direction = E };
                  { position = (Pos.(h 3), Pos.(v 3)); direction = NE };
                  { position = (Pos.(h 3), Pos.(v 4)); direction = N };
                  { position = (Pos.(h 3), Pos.(v 5)); direction = NW };
                ]
                (get_all_moves initial_state_5x9 W));
          test_case "one valid capture move, one invalid escape move" `Quick
            (fun () ->
              Alcotest.(check (list move))
                "same result"
                [
                  { position = (Pos.(h 0), Pos.(v 1)); direction = S };
                  { position = (Pos.(h 0), Pos.(v 1)); direction = W };
                ]
                (get_all_moves board_2_set0_1_B B));
          test_case "no capture moves" `Quick (fun () ->
              Alcotest.(check (list move))
                "same result"
                [
                  { position = (Pos.(h 0), Pos.(v 2)); direction = S };
                  { position = (Pos.(h 0), Pos.(v 2)); direction = E };
                  { position = (Pos.(h 0), Pos.(v 2)); direction = W };
                  { position = (Pos.(h 0), Pos.(v 2)); direction = SW };
                  { position = (Pos.(h 0), Pos.(v 2)); direction = SE };
                  { position = (Pos.(h 2), Pos.(v 7)); direction = N };
                  { position = (Pos.(h 2), Pos.(v 7)); direction = E };
                  { position = (Pos.(h 2), Pos.(v 7)); direction = W };
                  { position = (Pos.(h 3), Pos.(v 0)); direction = N };
                  { position = (Pos.(h 3), Pos.(v 0)); direction = S };
                  { position = (Pos.(h 3), Pos.(v 0)); direction = E };
                  { position = (Pos.(h 3), Pos.(v 6)); direction = N };
                  { position = (Pos.(h 3), Pos.(v 6)); direction = W };
                  { position = (Pos.(h 3), Pos.(v 7)); direction = NE };
                  { position = (Pos.(h 3), Pos.(v 7)); direction = NW };
                  { position = (Pos.(h 3), Pos.(v 8)); direction = N };
                  { position = (Pos.(h 4), Pos.(v 3)); direction = N };
                  { position = (Pos.(h 4), Pos.(v 3)); direction = E };
                  { position = (Pos.(h 4), Pos.(v 3)); direction = W };
                  { position = (Pos.(h 4), Pos.(v 6)); direction = W };
                  { position = (Pos.(h 4), Pos.(v 6)); direction = NW };
                ]
                (get_all_moves board_2 W));
          test_case "no pawns" `Quick (fun () ->
              Alcotest.(check (list move))
                "same result" [] (get_all_moves board_2 B));
        ] );
      ( "make_capture_by_approach",
        [
          test_case "capture one pawn" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture B
                      { position = (Pos.h 2, Pos.v 2); direction = W }
                      (Some Approach) [])));
          test_case "capture two pawn" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Pawn B;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Pawn W;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 3, Pos.v 1); direction = N }
                      (Some Approach) [])));
          test_case "capture own pawn split" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Pawn B;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Pawn W;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 4, Pos.v 2); direction = N }
                      (Some Approach) [])));
          test_case "capture by approach one pawn" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 2, Pos.v 0); direction = N }
                      (Some Approach) [])));
          test_case "capture by approach two pawns" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Pawn B;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Pawn W;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 3, Pos.v 1); direction = N }
                      (Some Approach) [])));
          test_case "capture by approach split line" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Empty;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 4, Pos.v 2); direction = E }
                      (Some Approach) [])));
          test_case "capture by approach one pawn diagonal" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Empty;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                    Pawn W;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 3, Pos.v 7); direction = NW }
                      (Some Approach) [])));
          test_case "capture by approach diagonal pawns" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Pawn W;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 4, Pos.v 2); direction = NE }
                      (Some Approach) [])));
        ] );
      ( "make_capture_by_withdrawal",
        [
          test_case "capture by withdrawal one pawn" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 2, Pos.v 0); direction = N }
                      (Some Withdrawal) [])));
          test_case "capture by withdrawal two pawns" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Empty;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 0, Pos.v 2); direction = E }
                      (Some Withdrawal) [])));
          test_case "capture by withdrawal diagonal" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Pawn B;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                  [
                    Pawn B;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn B;
                  ];
                  [
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Pawn B;
                    Pawn B;
                    Empty;
                    Pawn B;
                    Empty;
                  ];
                ]
                (fst
                   (make_move pre_capture W
                      { position = (Pos.h 1, Pos.v 3); direction = NE }
                      (Some Withdrawal) [])));
        ] );
      ( "position_or_direction_already_executed",
        [
          test_case "empty chain" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (let mc = [] in
                 position_or_direction_or_line_already_executed mc
                   { position = (Pos.h 2, Pos.v 3); direction = W }));
          test_case "valid move" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" false
                (let mc =
                   [
                     { position = (Pos.h 3, Pos.v 4); direction = N };
                     { position = (Pos.h 2, Pos.v 4); direction = E };
                   ]
                 in
                 position_or_direction_or_line_already_executed mc
                   { position = (Pos.h 2, Pos.v 3); direction = S }));
          test_case "return to pos" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (let mc =
                   [
                     { position = (Pos.h 3, Pos.v 4); direction = N };
                     { position = (Pos.h 2, Pos.v 4); direction = W };
                   ]
                 in
                 position_or_direction_or_line_already_executed mc
                   { position = (Pos.h 2, Pos.v 3); direction = E }));
          test_case "same line" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (let mc =
                   [
                     { position = (Pos.h 3, Pos.v 4); direction = N };
                     { position = (Pos.h 2, Pos.v 4); direction = E };
                     { position = (Pos.h 2, Pos.v 5); direction = N };
                     { position = (Pos.h 1, Pos.v 5); direction = W };
                   ]
                 in
                 position_or_direction_or_line_already_executed mc
                   { position = (Pos.h 1, Pos.v 4); direction = N }));
          test_case "same diagonal" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (let mc =
                   [
                     { position = (Pos.h 4, Pos.v 4); direction = NW };
                     { position = (Pos.h 3, Pos.v 3); direction = W };
                     { position = (Pos.h 3, Pos.v 2); direction = N };
                   ]
                 in
                 position_or_direction_or_line_already_executed mc
                   { position = (Pos.h 2, Pos.v 2); direction = NW }));
        ] );
      ( "make_move",
        [
          test_case "simple move" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn W;
                  ];
                  [
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn W;
                  ];
                ]
                (fst
                   (make_move board_2 W
                      { position = (Pos.h 0, Pos.v 2); direction = W }
                      (Some Approach) [])));
          test_case "capture move" `Quick (fun () ->
              Alcotest.(check board)
                "same result"
                [
                  [
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                  ];
                  [
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                  ];
                  [
                    Pawn W;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn W;
                  ];
                  [
                    Empty;
                    Empty;
                    Empty;
                    Pawn W;
                    Empty;
                    Empty;
                    Pawn W;
                    Pawn W;
                    Pawn W;
                  ];
                ]
                (fst
                   (make_move board_2_set0_1_B W
                      { position = (Pos.h 0, Pos.v 2); direction = E }
                      (Some Withdrawal) [])));
          test_case "wrong capture type" `Quick (fun () ->
              Alcotest.(check_raises)
                "Not_capture_by_withdrawal" Not_capture_by_withdrawal (fun () ->
                  ignore
                    (make_move board_2_set0_1_B W
                       { position = (Pos.h 0, Pos.v 2); direction = E }
                       (Some Approach) [])));
          test_case "wrong capture type" `Quick (fun () ->
              Alcotest.(check_raises)
                "Not_capture_by_withdrawal" Not_capture_by_withdrawal (fun () ->
                  ignore
                    (make_move board_2_set0_1_B W
                       { position = (Pos.h 0, Pos.v 2); direction = E }
                       (Some Approach) [])));
          test_case "Invalid move" `Quick (fun () ->
              Alcotest.(check_raises) "Invalid_position" Invalid_position
                (fun () ->
                  ignore
                    (make_move pre_capture W
                       { position = (Pos.h 2, Pos.v 0); direction = S }
                       (Some Approach) [])));
        ] );
    ]
