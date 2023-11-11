open Fanorona.Engine

let () = print_endline "Hello, World!"

let () =
  let board = init initial_state_5x9 in
  pp_board Format.std_formatter board
