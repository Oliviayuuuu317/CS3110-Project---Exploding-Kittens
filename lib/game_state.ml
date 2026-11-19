type direction =
  | Clockwise
  | Counterclockwise

type status =
  | Not_started
  | In_progress
  | Finished of int

type game_state = {
  draw_pile : Card.card list;
  discard_pile : Card.card list;
  players : Player.player list;
  current_player_id : int;
  direction : direction;
  remaining_players : int;
  status : status;
}

type invariant_violation =
  | Duplicate_player_id of int
  | Remaining_players_mismatch of { recorded : int; actual : int }
  | In_progress_player_count of int
  | Current_player_not_found of int
  | Current_player_eliminated of int
  | Finished_player_count of int
  | Finished_winner_not_active of int

let duplicate_ids players =
  let rec collect seen duplicates = function
    | [] -> List.rev duplicates
    | player :: rest ->
        let id = player.Player.player_id in
        if List.mem id seen then
          if List.mem id duplicates then collect seen duplicates rest
          else collect seen (id :: duplicates) rest
        else collect (id :: seen) duplicates rest
  in
  collect [] [] players

let validate state =
  let active_players =
    List.filter
      (fun player -> player.Player.status = Player.Active)
      state.players
  in
  let active_count = List.length active_players in
  let errors =
    List.map
      (fun id -> Duplicate_player_id id)
      (duplicate_ids state.players)
  in
  let errors =
    if state.remaining_players = active_count then errors
    else
      Remaining_players_mismatch
        { recorded = state.remaining_players; actual = active_count }
      :: errors
  in
  let errors =
    match state.status with
    | Not_started -> errors
    | In_progress ->
        let errors =
          if active_count >= 2 then errors
          else In_progress_player_count active_count :: errors
        in
        (match
           List.find_opt
             (fun player ->
               player.Player.player_id = state.current_player_id)
             state.players
         with
        | None -> Current_player_not_found state.current_player_id :: errors
        | Some player ->
            if player.Player.status = Player.Active then errors
            else Current_player_eliminated state.current_player_id :: errors)
    | Finished winner_id ->
        let errors =
          if active_count = 1 then errors
          else Finished_player_count active_count :: errors
        in
        if
          active_count = 1
          && List.exists
               (fun player ->
                 player.Player.player_id = winner_id
                 && player.Player.status = Player.Active)
               active_players
        then errors
        else Finished_winner_not_active winner_id :: errors
  in
  match errors with [] -> Ok () | _ -> Error (List.rev errors)
