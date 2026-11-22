type error =
  | Game_not_in_progress
  | Invalid_state of Game_state.invariant_violation list
  | Not_current_player of int
  | Empty_draw_pile
  | Card_not_in_hand of Card.card
  | Card_cannot_be_played of Card.card

type outcome =
  | Drew_card of Card.card
  | Defused_kitten
  | Player_eliminated of int
  | Played_card of Card.card

(** [shuffle rng cards] returns a uniformly random permutation of [cards]
    using [rng] as the source of randomness. *)
let shuffle rng cards =
  let cards = Array.of_list cards in
  for i = Array.length cards - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let card = cards.(i) in
    cards.(i) <- cards.(j);
    cards.(j) <- card
  done;
  Array.to_list cards

(** [shuffle_draw_pile rng state] returns a copy of [state] with just its draw
    pile shuffled. *)
let shuffle_draw_pile rng (state : Game_state.game_state) =
  { state with draw_pile = shuffle rng state.draw_pile }

(** [remove_one card cards] removes the first occurrence of [card] and
    returns [Some remaining] in the original order, or [None] if absent. *)
let rec remove_one card = function
  | [] -> None
  | first :: rest when first = card -> Some rest
  | first :: rest -> Option.map (List.cons first) (remove_one card rest)

(** [update_player state player] replaces the seated player with the same ID
    and preserves the seating order. Requires that the ID occurs exactly once. *)
let update_player (state : Game_state.game_state) (player : Player.player) =
  let players =
    List.map
      (fun seated ->
        if seated.Player.player_id = player.player_id then player else seated)
      state.players
  in
  { state with players }

(** [insert_at index card cards] inserts [card] at the zero-based [index],
    preserving the order of [cards]. An index below zero inserts at the front.
    An index at or beyond the length appends. *)
let rec insert_at index card cards =
  match cards with
  | first :: rest when index > 0 -> first :: insert_at (index - 1) card rest
  | _ -> card :: cards

(** [resolve_kitten rng state player] resolves a kitten draw after the kitten
    has been removed from the pile. Requires [player] to be the active current
    player. With a Defuse, it discards one Defuse, reinserts the kitten at a
    uniformly random position, and reports [Defused_kitten]. Otherwise it
    discards the kitten and the player's hand and reports [Player_eliminated].
    Both outcomes end the turn and declare a winner if one player remains. *)
let resolve_kitten rng (state : Game_state.game_state) (player : Player.player) =
  match remove_one Card.Defuse player.hand with
  | Some hand ->
      let index = Random.State.int rng (List.length state.draw_pile + 1) in
      let state = update_player state { player with hand } in
      let state =
        {
          state with
          draw_pile = insert_at index Card.Exploding_Kitten state.draw_pile;
          discard_pile = Card.Defuse :: state.discard_pile;
        }
      in
      (Game_state.end_turn state, Defused_kitten)
  | None ->
      let state =
        update_player state { player with hand = []; status = Player.Eliminated }
      in
      let state =
        {
          state with
          discard_pile =
            Card.Exploding_Kitten :: (player.hand @ state.discard_pile);
          remaining_players = state.remaining_players - 1;
        }
      in
      (Game_state.end_turn state, Player_eliminated player.player_id)

(** [draw rng state player] draws the top card for the active current player
    and ends their turn. Ordinary cards enter the hand and report [Drew_card].
    Exploding Kittens are handled by [resolve_kitten]. An empty pile returns
    [Error Empty_draw_pile]. *)
let draw rng (state : Game_state.game_state) (player : Player.player) =
  match state.draw_pile with
  | [] -> Error Empty_draw_pile
  | Card.Exploding_Kitten :: draw_pile ->
      Ok (resolve_kitten rng { state with draw_pile } player)
  | card :: draw_pile ->
      let player = { player with hand = card :: player.hand } in
      let state = update_player { state with draw_pile } player in
      Ok (Game_state.end_turn state, Drew_card card)

(** [play rng state player card] spends one copy of [card] from the active
    current player's hand to play Skip or Shuffle. Skip ends the turn, and
    Shuffle reshuffles the draw pile. *)
let play rng (state : Game_state.game_state) (player : Player.player) card =
  match (card, remove_one card player.hand) with
  | _, None -> Error (Card_not_in_hand card)
  | (Card.Skip | Card.Shuffle), Some hand ->
      let state = update_player state { player with hand } in
      let state = { state with discard_pile = card :: state.discard_pile } in
      let state =
        if card = Card.Skip then Game_state.end_turn state
        else shuffle_draw_pile rng state
      in
      Ok (state, Played_card card)
  | _, Some _ -> Error (Card_cannot_be_played card)

(** [apply_action rng state ~player_id action] checks that the game is in
    progress, the state passes [Game_state.validate], and it is [player_id]'s
    turn. It then applies [action] and returns the new state and outcome.
    Errors leave the game state and [rng] unchanged. *)
let apply_action rng (state : Game_state.game_state) ~player_id action =
  if state.status <> Game_state.In_progress then Error Game_not_in_progress
  else
    match Game_state.validate state with
    | Error violations -> Error (Invalid_state violations)
    | Ok () when player_id <> state.current_player_id ->
        Error (Not_current_player player_id)
    | Ok () -> (
        let player =
          List.find (fun p -> p.Player.player_id = player_id) state.players
        in
        match action with
        | Card.Draw_card -> draw rng state player
        | Card.Play_card card -> play rng state player card)

(** [draw_card rng state ~player_id] submits [Card.Draw_card] through
    [apply_action] and returns the same result. *)
let draw_card rng state ~player_id =
  apply_action rng state ~player_id Card.Draw_card

(** [play_card rng state ~player_id card] submits [Card.Play_card card]
    through [apply_action] and returns the same result. *)
let play_card rng state ~player_id card =
  apply_action rng state ~player_id (Card.Play_card card)
