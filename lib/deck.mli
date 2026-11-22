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
  | Defused_kitten of int
      (** The kitten went back into the draw pile at this zero-based index
          (0 is the top). The index is private to the acting player. *)
  | Player_eliminated of int
  | Played_card of Card.card
  | Saw_the_future of Card.card list
      (** The top cards of the draw pile, top first, after playing See the
          Future. At most three cards, fewer if the pile is smaller. Private to
          the acting player. *)

val shuffle : Random.State.t -> Card.card list -> Card.card list
(** [shuffle rng cards] returns a uniformly random permutation of [cards]
    using [rng] as the source of randomness. *)

val shuffle_draw_pile : Random.State.t -> Game_state.game_state -> Game_state.game_state
(** Shuffles only the draw pile. Playing [Card.Shuffle] through [apply_action]
    also spends the card from the current player's hand. *)

val apply_action :
  ?place_kitten:(int -> int) ->
  Random.State.t ->
  Game_state.game_state ->
  player_id:int ->
  Card.action ->
  (Game_state.game_state * outcome, error) result
(** Validates the in-progress state and checks that [player_id] is the
    current player before applying the action. A draw, Skip, or Reverse ends
    the turn. Shuffle and See the Future let the player continue. The card in
    [Drew_card] is private to the acting player.

    If the draw is a defused Exploding Kitten, [place_kitten size] is asked
    where to put it back, where [size] is the size of the draw pile without
    the kitten. The answer is a zero-based index, so [0] is the top and [size]
    is the bottom. Out-of-range answers are clamped. Without [place_kitten]
    the position is uniformly random. Errors leave the state and RNG
    unchanged. *)

val draw_card :
  ?place_kitten:(int -> int) ->
  Random.State.t ->
  Game_state.game_state ->
  player_id:int ->
  (Game_state.game_state * outcome, error) result
(** Draws the top card and ends the turn. An Exploding Kitten automatically
    spends one Defuse and is reinserted where [place_kitten] says (see
    [apply_action]). A player without a Defuse is eliminated and discards
    their hand and kitten. Drawing from an empty pile returns
    [Empty_draw_pile]. *)

val play_card :
  Random.State.t ->
  Game_state.game_state ->
  player_id:int ->
  Card.card ->
  (Game_state.game_state * outcome, error) result
(** Plays and discards one copy of Skip, Reverse, Shuffle, or See the Future
    from the current player's hand. Reverse flips the direction and then ends
    the turn. See the Future returns [Saw_the_future] with up to three top
    cards and does not change the draw pile. Returns [Card_not_in_hand] for a
    missing card or [Card_cannot_be_played] for Defuse or Exploding Kitten. *)
