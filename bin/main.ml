open Fanorona.Arena

let () =
  Format.open_vbox 0;
  let result =
    Lwt_main.run (arena (pair ~w:(player_teletype W) ~b:(player_teletype B)))
  in
  Format.printf "Game ends with %a@," pp_endplay result.endgame;
  Format.printf "Trace: @[<v>%a@]" pp_trace result.trace;
  Format.close_box ();
  Format.printf "@."
