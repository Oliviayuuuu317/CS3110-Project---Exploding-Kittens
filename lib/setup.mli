(** Game initialization: deck generation, player creation, and dealing.

    + Build the base deck of Skip, Reverse, Shuffle, and See the Future cards
      (no Exploding Kittens and no Defuse cards yet).
    + Shuffle it and deal 4 cards to every player.
    + Give every player exactly one Defuse card, so every starting hand has
      {!starting_hand_size} = 5 cards.
    + Add {!extra_defuses} = 2 extra Defuse cards and [n - 1] Exploding Kittens
      to the remaining cards and shuffle them to form the draw pile.

    The game ends when one active player remains. Human and AI players get
    the same starting deal, and each AI seat also gets an {!ai_profile}. *)

(** Strategy configuration for AI move selection. *)
type ai_strategy =
  | Random_play
      (** Picks uniformly among the legal function cards and ending actions. *)
  | Cautious
      (** Uses See the Future to look ahead and plays Skip/Reverse instead of
          drawing when an Exploding Kitten is known or likely to be on top. *)
  | Aggressive
      (** Draws whenever it holds a Defuse and saves Skip/Reverse for when it
          has no Defuse left. *)

(** How one seat at the table is controlled. *)
type player_config =
  | Human_player  (** A seat controlled by a person. *)
  | Ai_player of ai_strategy  (** A seat controlled by the given strategy. *)

type ai_profile = {
  ai_id : int;  (** ID of the AI player this profile belongs to. *)
  strategy : ai_strategy;  (** The strategy this AI uses. *)
  known_top_cards : Card.card list;
      (** Storage for cards revealed to this AI by See the Future, top first.
          Setup initializes it to the empty list. *)
}
(** Per-AI configuration and memory initialized at setup. *)

type setup = {
  state : Game_state.game_state;  (** The initial, in-progress game state. *)
  ai_profiles : ai_profile list;
      (** One profile per AI player, in player-ID order. *)
}
(** The result of a successful setup. *)

(** Why a game could not be initialized. *)
type setup_error =
  | Too_few_players of int  (** Fewer than {!min_players} seats. *)
  | Too_many_players of int  (** More than {!max_players} seats. *)

val min_players : int
(** Minimum number of players in a game (2). *)

val max_players : int
(** Maximum number of players in a game (8). *)

val starting_hand_size : int
(** Number of cards in every starting hand (5), including one Defuse. *)

val extra_defuses : int
(** Number of Defuse cards shuffled into the draw pile in addition to the one
    each player starts with (2). A game with [n] players therefore has
    [n + extra_defuses] Defuse cards in total. *)

val base_deck : int -> Card.card list
(** [base_deck n] is the unshuffled deck used for dealing in an [n]-player
    game. It contains no [Exploding_Kitten] and no [Defuse]. *)

val shuffle : Random.State.t -> Card.card list -> Card.card list
(** [shuffle rng cards] returns a uniformly random permutation of [cards]
    using [rng] as the source of randomness. *)

val count : Card.card -> Card.card list -> int
(** [count card cards] is the number of occurrences of [card] in [cards]. *)

val make_configs : humans:int -> ais:ai_strategy list -> player_config list
(** [make_configs ~humans ~ais] lists [humans] human seats followed by one AI
    seat per strategy in [ais]. Requires: [humans >= 0]. *)

val create_players : player_config list -> Player.player list
(** [create_players configs] creates one active player per config with IDs [1],
    [2], ..., in order, and empty hands. *)

val init_ai_profiles :
  Player.player list -> player_config list -> ai_profile list
(** [init_ai_profiles players configs] creates an {!ai_profile} for every AI
    seat, pairing each player with the config at the same position. Requires:
    [players] and [configs] have the same length. *)

val init :
  ?rng:Random.State.t -> player_config list -> (setup, setup_error) result
(** [init ?rng configs] initializes a new game for the given seats. Player IDs
    are [1..n] in the order of [configs]. Player [1] moves first and play starts
    [Clockwise] with an empty discard pile. The returned state is [In_progress]
    and passes {!Game_state.validate}.

    [rng] defaults to a freshly self-initialized generator. Pass an explicit
    generator for reproducible deals. *)

val strategy_of : setup -> int -> ai_strategy option
(** [strategy_of setup player_id] is the strategy of AI player [player_id], or
    [None] if that player is human or does not exist. *)
