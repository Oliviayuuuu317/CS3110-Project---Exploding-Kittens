type player_summary = {
  player_id : int;
  kind : Player.player_kind;
  status : Player.player_status;
  hand_size : int;
}

type observation = {
  player_id : int;
  hand : Card.card list;
  player_summaries : player_summary list;
  draw_pile_size : int;
  discard_pile : Card.card list;
  current_player_id : int;
  direction : Game_state.direction;
  remaining_players : int;
  game_status : Game_state.status;
}

type observation_error =
  | Player_not_found of int
  | Player_is_not_ai of int
  | Player_is_eliminated of int
  | Game_not_in_progress

let observe state player_id =
  if state.Game_state.status <> Game_state.In_progress then
    Error Game_not_in_progress
  else
    match
      List.find_opt
        (fun player -> player.Player.player_id = player_id)
        state.Game_state.players
    with
    | None -> Error (Player_not_found player_id)
    | Some player ->
        if player.Player.kind <> Player.AI then
          Error (Player_is_not_ai player_id)
        else if player.Player.status = Player.Eliminated then
          Error (Player_is_eliminated player_id)
        else
          let player_summaries =
            List.map
              (fun entry ->
                {
                  player_id = entry.Player.player_id;
                  kind = entry.Player.kind;
                  status = entry.Player.status;
                  hand_size = List.length entry.Player.hand;
                })
              state.Game_state.players
          in
          Ok
            {
              player_id;
              hand = player.Player.hand;
              player_summaries;
              draw_pile_size = List.length state.Game_state.draw_pile;
              discard_pile = state.Game_state.discard_pile;
              current_player_id = state.Game_state.current_player_id;
              direction = state.Game_state.direction;
              remaining_players = state.Game_state.remaining_players;
              game_status = state.Game_state.status;
            }
