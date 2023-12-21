open Engine

let count player board =
  let cnt = ref 0 in
  iteri (fun _ _ c -> if c = Pawn player then cnt := !cnt + 1) board;
  !cnt

let lmax =
  let rec aux l v = match l with [] -> v | h :: t -> aux t (max h v) in
  function [] -> None | h :: t -> Some (aux t h)

let random_element lst =
  let len = List.length lst in
  if len = 0 then None else Some (List.nth lst (Random.int len))

let get_all_real_moves empty player board move_chain =
  get_all_moves board player
  |> List.map (fun m ->
         match type_capture_move board m player with
         | Some Both -> [ (m, Some Approach); (m, Some Withdrawal) ]
         | c -> [ (m, c) ])
  |> List.flatten
  |> List.filter_map (fun (m, c) ->
         try
           let _ = make_move board player m c move_chain in
           Some (m, c)
         with _ -> None)
  |> List.map (fun (m, c) -> Some (m, c))
  |> fun l -> if empty then None :: l else l

let rec player_smort_aux n player board move_chain =
  get_all_real_moves (move_chain <> []) player board move_chain
  |> List.map (fun l -> (l, calc_points n player board move_chain l))
  |> (fun l ->
       let m = lmax (List.map (fun (_, v) -> v) l) in
       List.filter (fun (_, v) -> Some v = m) l)
  |> random_element

and calc_points n player board move_chain move =
  let board, _ =
    Option.fold ~none:(board, move_chain)
      ~some:(fun (m, c) -> make_move board player m c move_chain)
      move
  in
  if n = 0 then count player board - count (opponent player) board
  else
    match player_smort_aux (n - 1) (opponent player) board [] with
    | Some (_, s) -> -s
    | None -> 10000000000

let player p b m =
  Lwt.return
    (match player_smort_aux 1 p b m with
    | Some (Some (m, c), _) -> Some (Some m, c)
    | _ -> None)
