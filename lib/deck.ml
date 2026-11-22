type error =
  | Game_not_in_progress
  | Invalid_state of Game_state.invariant_violation list
  | Not_current_player of int
  | Empty_draw_pile
  | Card_not_in_hand of Card.card
  | Card_cannot_be_played of Card.card

type outcome =
  | Drew_card of Card.card
  | Defused_kitten of int
  | Player_eliminated of int
  | Played_card of Card.card
  | Saw_the_future of Card.card list

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

(** [future_size] is how many cards See the Future reveals. *)
let future_size = 3

(** [take n cards] is the first [n] cards of [cards], or all of them if there
    are fewer than [n]. *)
let rec take n = function
  | first :: rest when n > 0 -> first :: take (n - 1) rest
  | _ -> []

(** [kitten_index rng place_kitten size] picks where a defused kitten goes in a
    draw pile of [size] cards. Without [place_kitten] the index is uniformly
    random. A chosen index is clamped to [0..size], so any number below zero
    means the top and any number past the end means the bottom. *)
let kitten_index rng place_kitten size =
  match place_kitten with
  | None -> Random.State.int rng (size + 1)
  | Some choose -> max 0 (min size (choose size))

(** [resolve_kitten rng place_kitten state player] resolves a kitten draw after
    the kitten has been removed from the pile. Requires [player] to be the
    active current player. With a Defuse, it discards one Defuse, reinserts the
    kitten at the index from [kitten_index], and reports [Defused_kitten index].
    Otherwise it discards the kitten and the player's hand and reports
    [Player_eliminated]. Both outcomes end the turn and declare a winner if one
    player remains. *)
let resolve_kitten rng place_kitten (state : Game_state.game_state)
    (player : Player.player) =
  match remove_one Card.Defuse player.hand with
  | Some hand ->
      let index =
        kitten_index rng place_kitten (List.length state.draw_pile)
      in
      let state = update_player state { player with hand } in
      let state =
        {
          state with
          draw_pile = insert_at index Card.Exploding_Kitten state.draw_pile;
          discard_pile = Card.Defuse :: state.discard_pile;
        }
      in
      (Game_state.end_turn state, Defused_kitten index)
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

(** [draw rng place_kitten state player] draws the top card for the active
    current player and ends their turn. Ordinary cards enter the hand and
    report [Drew_card]. Exploding Kittens are handled by [resolve_kitten]. An
    empty pile returns [Error Empty_draw_pile]. *)
let draw rng place_kitten (state : Game_state.game_state)
    (player : Player.player) =
  match state.draw_pile with
  | [] -> Error Empty_draw_pile
  | Card.Exploding_Kitten :: draw_pile ->
      Ok (resolve_kitten rng place_kitten { state with draw_pile } player)
  | card :: draw_pile ->
      let player = { player with hand = card :: player.hand } in
      let state = update_player { state with draw_pile } player in
      Ok (Game_state.end_turn state, Drew_card card)

(** [reverse direction] is the opposite play direction. *)
let reverse = function
  | Game_state.Clockwise -> Game_state.Counterclockwise
  | Game_state.Counterclockwise -> Game_state.Clockwise

(** [play rng state player card] spends one copy of [card] from the active
    current player's hand. Skip ends the turn. Reverse flips the direction
    first and then ends the turn, so the next player is found in the new
    direction. Shuffle reshuffles the draw pile and See the Future reveals the
    top cards; both let the player keep going. Defuse and Exploding Kitten
    cannot be played by hand. *)
let play rng (state : Game_state.game_state) (player : Player.player) card =
  match (card, remove_one card player.hand) with
  | _, None -> Error (Card_not_in_hand card)
  | (Card.Defuse | Card.Exploding_Kitten), Some _ ->
      Error (Card_cannot_be_played card)
  | (Card.Skip | Card.Reverse | Card.Shuffle | Card.See_the_Future), Some hand
    -> (
      let state = update_player state { player with hand } in
      let state = { state with discard_pile = card :: state.discard_pile } in
      match card with
      | Card.Skip -> Ok (Game_state.end_turn state, Played_card card)
      | Card.Reverse ->
          let state = { state with direction = reverse state.direction } in
          Ok (Game_state.end_turn state, Played_card card)
      | Card.Shuffle -> Ok (shuffle_draw_pile rng state, Played_card card)
      | _ -> Ok (state, Saw_the_future (take future_size state.draw_pile)))

(** [apply_action ?place_kitten rng state ~player_id action] checks that the
    game is in progress, the state passes [Game_state.validate], and it is
    [player_id]'s turn. It then applies [action] and returns the new state and
    outcome. [place_kitten] is only used if the action defuses a kitten. Errors
    leave the game state and [rng] unchanged. *)
let apply_action ?place_kitten rng (state : Game_state.game_state) ~player_id
    action =
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
        | Card.Draw_card -> draw rng place_kitten state player
        | Card.Play_card card -> play rng state player card)

(** [draw_card ?place_kitten rng state ~player_id] submits [Card.Draw_card]
    through [apply_action] and returns the same result. *)
let draw_card ?place_kitten rng state ~player_id =
  apply_action ?place_kitten rng state ~player_id Card.Draw_card

(** [play_card rng state ~player_id card] submits [Card.Play_card card]
    through [apply_action] and returns the same result. *)
let play_card rng state ~player_id card =
  apply_action rng state ~player_id (Card.Play_card card)
