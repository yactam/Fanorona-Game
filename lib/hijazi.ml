open Engine

type mc = move * capture option
type mc_chain = mc list

let rec pp_list fmt pp = function
  | [] -> Format.fprintf fmt "[]"
  | h::t -> Format.fprintf fmt "%a :: " pp h; pp_list fmt pp t

let pp_capture_option fmt c_o =
  Format.pp_print_option pp_capture fmt c_o

let pp_mc fmt (m, c_o) =
  Format.fprintf fmt "(%a, %a)" pp_move m pp_capture_option c_o

let pp_mc_chain fmt mc_chain = pp_list fmt pp_mc mc_chain

let pp_mc_chain_list fmt mc_chain_list = pp_list fmt pp_mc_chain mc_chain_list

let count_pawns player board =
  let count = ref 0 in
  iteri
    (fun _ _ c ->
    match c with
    | Empty -> ()
    | Pawn p -> if p = player then count := !count + 1)
    board;
  !count

let all_capture_moves player board move_chain =
      get_all_moves board player |>
      List.filter
        (fun m ->
          type_capture_move board m player <> None
          && (List.is_empty move_chain
             || is_last_pawn_position_move m move_chain))

let rec move_chain_of_mc_chain = function
  | [] -> []
  | (m, _)::t -> m::(move_chain_of_mc_chain t)

let rec mc_chain_of_move_chain board player = function
  | [] -> []
  | h::t ->
    match type_capture_move board h player with
    | Some Both -> (h, Some Approach)::(h, Some Withdrawal)::mc_chain_of_move_chain board player t
    | c_o -> (h, c_o)::mc_chain_of_move_chain board player t

(* TODO debug this *)
let all_mc_chains player board move_chain : mc_chain list =
  let rec aux board acc : mc_chain list =
    let capture_moves = all_capture_moves player board (move_chain_of_mc_chain acc) in
    let capture_mcs = mc_chain_of_move_chain board player capture_moves in
    if capture_mcs = [] then [List.rev acc] else
    let to_be_explored = List.map (fun (m, c_o) ->
      let new_board, _ = make_move board player m c_o [] in
      (new_board, (m, c_o)::acc)
    ) capture_mcs in
    List.map (fun (b, mcs) -> aux b mcs) to_be_explored |> List.flatten
  in aux board (mc_chain_of_move_chain board player move_chain)

let count_pawns_captured_by_mc_chain player board mc_chain =
  let op = opponent player in
  let rec loop count board = function
    | [] -> count
    | (m, c_o)::t ->
      let new_board, _ = make_move board player m c_o [] in
      let captured = (count_pawns op board) - (count_pawns op new_board) in
      loop (count + captured) new_board t
  in loop 0 board mc_chain

let rec mc_chain_count_list_of_mc_chain_list player board = function
  | [] -> []
  | mc_chain::t -> (mc_chain, count_pawns_captured_by_mc_chain player board mc_chain)
                 ::(mc_chain_count_list_of_mc_chain_list player board t)

let filter_invalid player board mc_chains =
  let rec aux = function
    | [] -> []
    | (m, c_o)::t ->
      try
        let _ = make_move board player m c_o [] in
        (m, c_o)::t
      with Invalid_position -> aux t
  in

  let rec loop = function
    | [] -> []
    | h::t -> (aux h)::(loop t)

  in loop mc_chains


let cmp_mc_chain_count (_, a) (_, b) = a - b

let _ = pp_mc_chain_list

let best_mc_chain board player mc_chain_list =
  let mc_chain_count_list = mc_chain_count_list_of_mc_chain_list player board mc_chain_list in
  let sorted = List.sort cmp_mc_chain_count mc_chain_count_list |> List.rev in
  match sorted with
  | [] -> None
  | (mc_chain, _)::_ -> Some mc_chain

let custom_player player board move_chain =
  let all_mc_chains = all_mc_chains player board move_chain in
  let filtered = filter_invalid player board all_mc_chains in
    let best = best_mc_chain board player filtered in
    match best with
    | Some ((m, c_o)::_) -> Lwt.return (Some (Some m, c_o))
    | _ -> Arena.player_random player board move_chain
