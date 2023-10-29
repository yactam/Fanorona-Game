open Fanorona.Engine
open Utils

let board_9x9_get res i j () =
  Alcotest.(check (option player))
    "same result" res
    (get example1 Pos.(H i) Pos.(V j))

let () =
  let open Alcotest in 
  run "Engine"
    [
      ( "init-get",
        [

        ]);
      ( "pp",
        [

        ]);
      ("set",
        [

        ]);
      ("free",
        [
          
        ]);
      ("win",
        [
          
        ]);
    ]