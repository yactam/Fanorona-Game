open Fanorona.Arena
open Fanorona.Engine
open Fanorona.Ia
open Utils

let nb_of_tests = 1000

let mult_run_test =
  let open QCheck in
  Test.make ~count:nb_of_tests
    ~name:(Format.asprintf "%d iterations of AI game run" nb_of_tests)
    init_board_arbitrary (fun b ->
      let b = init b in
      Format.printf "%a" pp_board b;
      let result =
        Lwt_main.run
          (arena
             (Fanorona.Arena.pair ~w:(my_ia W) ~b:(player_random B))
             ~init_board:b)
      in
      List.length result.trace > 0
      &&
      match result.endgame with
      | Win a -> win result.final a && not (win result.final (opponent a))
      | Giveup _ -> true)

let () =
  let rec aux n (wcnt, bcnt, gcnt) =
    let result =
      Lwt_main.run
        (arena (Fanorona.Arena.pair ~w:(my_ia W) ~b:(player_random B)))
    in
    match (result.endgame, n) with
    | _, 0 -> (wcnt, bcnt, gcnt)
    | Win W, _ -> aux (n - 1) (wcnt + 1, bcnt, gcnt)
    | Win B, _ -> aux (n - 1) (wcnt, bcnt + 1, gcnt)
    | Giveup _, _ -> aux (n - 1) (wcnt, bcnt, gcnt + 1)
  in
  let nbpart = nb_of_tests in
  let w, b, g = aux nbpart (0, 0, 0) in
  Format.open_vbox 0;
  Format.printf "Nombre de parties : %d@," nbpart;
  Format.printf "Pourcentage de victoires:@, White:%d@, Black:%d@, Giveup:%d"
    (100 * w / nbpart)
    (100 * b / nbpart)
    (100 * g / nbpart);
  Format.printf "@.";
  ()

let () =
  let open Alcotest in
  run "ia"
    [
      ( "run test",
        [
          test_case "game ends" `Quick (fun () ->
              Alcotest.(check bool)
                "same result" true
                (let result =
                   Lwt_main.run
                     (arena
                        (Fanorona.Arena.pair ~w:(my_ia W) ~b:(player_random B)))
                 in
                 List.length result.trace > 0
                 &&
                 match result.endgame with
                 | Win a ->
                     win result.final a && not (win result.final (opponent a))
                 | Giveup _ -> true));
          QCheck_alcotest.to_alcotest mult_run_test;
        ] );
    ]
