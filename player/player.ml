open Fanorona.Engine

let moves_and_capture_startegy player board move_chain =
  match get_all_moves board player with
  | [] -> []
  | all_moves ->
      let capture_moves_combos =
        let combine_move_capture move =
          match type_capture_move board move player with
          | Some Both -> [ (move, Some Approach); (move, Some Withdrawal) ]
          | capture -> [ (move, capture) ]
        in
        List.map combine_move_capture all_moves |> List.flatten
      in

      let capture_moves_combos_weigths =
        let map_move_to_weigth (move, capture) =
          let weigth =
            if position_or_direction_or_line_already_executed move_chain move
            then min_int
            else if can_continue board player move move_chain then 3
            else if capture = None then 0
            else if capture = Some Approach then 2
            else if capture = Some Withdrawal then 1
            else 0
          in

          if weigth < 0 then None
          else
            let future =
              try Some (make_move board player move capture move_chain)
              with _ -> None
            in

            let is_winning_move =
              match future with
              | None -> false
              | Some (future_board, _) -> win future_board player
            in

            if is_winning_move then Some (move, capture, max_int)
            else Some (move, capture, weigth)
        in

        let sort_descending (_, _, weigth_a) (_, _, weigth_b) =
          compare weigth_b weigth_a
        in
        List.filter_map map_move_to_weigth capture_moves_combos
        |> List.sort sort_descending
      in

      capture_moves_combos_weigths

let execute player board move_chain =
  let moves = moves_and_capture_startegy player board move_chain in
  let chosen =
    (fun (move, capture, _weigth) -> Some (Some move, capture))
    |> Option.bind (List.nth_opt moves 0)
  in
  Lwt.return chosen
