(** Shuffling and card actions shared by human and AI players. *)

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

val shuffle : Random.State.t -> Card.card list -> Card.card list
(** [shuffle rng cards] returns a uniformly random permutation of [cards]
    using [rng] as the source of randomness. *)

val shuffle_draw_pile : Random.State.t -> Game_state.game_state -> Game_state.game_state
(** Shuffles only the draw pile. Playing [Card.Shuffle] through [apply_action]
    also spends the card from the current player's hand. *)

val apply_action :
  Random.State.t ->
  Game_state.game_state ->
  player_id:int ->
  Card.action ->
  (Game_state.game_state * outcome, error) result
(** Validates the in-progress state and checks that [player_id] is the
    current player before applying the action. A draw or Skip ends the turn.
    Shuffle lets the player continue. The card in [Drew_card] is private to
    the acting player. Errors leave the state and RNG unchanged. *)

val draw_card :
  Random.State.t ->
  Game_state.game_state ->
  player_id:int ->
  (Game_state.game_state * outcome, error) result
(** Draws the top card and ends the turn. An Exploding Kitten automatically
    spends one Defuse and is reinserted at a uniformly random position. A
    player without a Defuse is eliminated and discards their hand and kitten.
    Drawing from an empty pile returns [Empty_draw_pile]. *)

val play_card :
  Random.State.t ->
  Game_state.game_state ->
  player_id:int ->
  Card.card ->
  (Game_state.game_state * outcome, error) result
(** Plays and discards one copy of Skip or Shuffle from the current player's
    hand. Returns [Card_not_in_hand] for a missing card or
    [Card_cannot_be_played] for any other held card. *)
