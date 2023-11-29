open Fanorona.Arena
open Fanorona.Engine
open Utils

let run_game_AI =
  Lwt_main.run
    (arena (Fanorona.Arena.pair ~w:(player_random W) ~b:(player_random B)))

let mult_run_test =
  let open QCheck in
  let iter = 10000 in
  Test.make ~count:iter
    ~name:(Format.asprintf "%d iterations of AI game run" iter)
    init_board_arbitrary (fun b ->
      let b = init b in
      Format.printf "%a" pp_board b;
      let result =
        Lwt_main.run
          (arena
             (Fanorona.Arena.pair ~w:(player_random W) ~b:(player_random B))
             ~init_board:b)
      in
      List.length result.trace > 0
      &&
      match result.endgame with
      | Win a -> win result.final a && not (win result.final (opponent a))
      | Giveup _ -> true)

let () =
  let open Alcotest in
  run "Arena"
    [
      ( "run test",
        [
          test_case "game ends" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (let result = run_game_AI in
                 List.length result.trace > 0
                 &&
                 match result.endgame with
                 | Win a ->
                     win result.final a && not (win result.final (opponent a))
                 | Giveup _ -> true));
          QCheck_alcotest.to_alcotest mult_run_test;
        ] );
      ( "pp test",
        [
          (let result = run_game_AI in
           test_case "pp_endgame" `Quick (fun () ->
               Alcotest.(check string)
                 "same result"
                 (match result.endgame with
                 | Win x ->
                     Format.asprintf "Player %a has won the game" pp_player x
                 | Giveup x -> Format.asprintf "Player %a gave up" pp_player x)
                 (Format.asprintf "%a" pp_endgame result.endgame)));
        ] );
    ]
