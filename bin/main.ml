open Fanorona.Arena

let wrap s player = (s, fun p b ms -> Printf.eprintf "\r                    "; Printf.eprintf "\r%s%!" s; player p b ms)

let players = [| wrap "Fanorona.random" player_random;
                 wrap "Lacenne" Fanorona.Lacenne.bot;
                 wrap "Abounaim" Fanorona.Abounaim.minimax_player;
                 wrap "Rzeszutek" player_artacalan;
                 wrap "Coutard" Fanorona.Coutard.player;
                 wrap "Nico" Fanorona.Nico.ia_player;
                 wrap "Fitzgerald" Fanorona.Fitzgerald.execute ;
                 wrap "Hijazi" Fanorona.Hijazi.custom_player ;
                 wrap "Laidouni" Fanorona.Laidouni.my_ia2 |]

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
      let t = Sys.time () in
      begin
      try
        for k = 0 to max_games - 1 do
          if Sys.time () -. t >= float_of_int (k + 1)  then
            begin
              failwith "Taking too long"
            end;
          begin try
              let result =
                Lwt_main.run (arena (pair ~w:(player_i W) ~b:(player_j B))) in
              match result.endgame with
              | Win W -> incr wins_i
              | Win B -> incr wins_j
              | Giveup W -> incr wins_j
              | Giveup B -> incr wins_i
            with
            | _ -> (* Draw: no-one wins *) ()
          end;
          begin try
              let result =
                Lwt_main.run (arena (pair ~b:(player_i B) ~w:(player_j W))) in
              match result.endgame with
              | Win W -> incr wins_j
              | Win B -> incr wins_i
              | Giveup W -> incr wins_i
              | Giveup B -> incr wins_j
            with _ -> (* Draw: no-one wins *)  ()
          end;
        done;
      with
      | _ ->
        begin
          (* Time-out: produce awkward value *)
          wins_i := -1;
          wins_j := -1
        end;
    end;
      let wins_i_ratio = (!wins_i * 100) / (2 * max_games) in
      let wins_j_ratio = (!wins_j * 100) / (2 * max_games) in
      Format.printf "@[<h 4>%.1f@;<4 4>%s@;<4 4>%d@;<4 4>%s@;<4 4>%d@]@;" ((Sys.time () -. t) *. (1000. /. (2. *. float_of_int max_games))) pi wins_i_ratio pj wins_j_ratio ;
    done
  done;
  Format.close_box ();
  Format.printf "@.";
  Printf.eprintf "\n"
