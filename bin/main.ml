open Fanorona.Arena

let players = [| ("Fanorona.random1", player_random);
                 ("Fanorona.random2", player_random);
                 ("Lacenne", Fanorona.Lacenne.bot) |]

let () =
  Random.self_init ();
  Format.open_vbox 0;
  for i = 0 to Array.length players - 1 do
    let (pi, player_i) = players.(i) in
    for j = i + 1 to Array.length players - 1 do
      let (pj, player_j) = players.(j) in
      let max_games = 500 in
      let wins_i = ref 0 in
      let wins_j = ref 0 in
      for _ = 0 to max_games - 1 do
        let result =
          Lwt_main.run (arena (pair ~w:(player_i W) ~b:(player_j B))) in
        match result.endgame with
        | Win W -> incr wins_i
        | Win B -> incr wins_j
        | Giveup W -> decr wins_i
        | Giveup B -> decr wins_j
      done;
      for _ = 0 to max_games - 1 do
        let result =
          Lwt_main.run (arena (pair ~b:(player_i B) ~w:(player_j W))) in
        match result.endgame with
        | Win W -> incr wins_j
        | Win B -> incr wins_i
        | Giveup W -> decr wins_j
        | Giveup B -> decr wins_i
      done;
      Format.printf "@[<h 4>%s@;<4 4>%d@;<4 4>%s@;<4 4>%d@]@;" pi !wins_i pj !wins_j ;
    done
  done;
  Format.close_box ();
  Format.printf "@."
