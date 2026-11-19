(** Public summary of a player, without exposing their hand. *)
type player_summary = {
  player_id : int;  (** Player identifier. *)
  kind : Player.player_kind;  (** Human or AI controller. *)
  status : Player.player_status;  (** Active or eliminated. *)
  hand_size : int;  (** Number of cards held. *)
}

(** Information available to one AI player when making a decision.
    Only [hand] contains that player's private cards. *)
type observation = {
  player_id : int;  (** ID of the observing AI. *)
  hand : Card.card list;  (** The observing AI's hand. *)
  player_summaries : player_summary list;  (** Public information for all players. *)
  draw_pile_size : int;  (** Number of cards remaining to draw. *)
  discard_pile : Card.card list;  (** Publicly visible discarded cards. *)
  current_player_id : int;  (** ID of the current player. *)
  direction : Game_state.direction;  (** Current play direction. *)
  remaining_players : int;  (** Number of active players. *)
  game_status : Game_state.status;  (** Current game lifecycle state. *)
}

(** Why an AI observation could not be created. *)
type observation_error =
  | Player_not_found of int  (** No player has the requested ID. *)
  | Player_is_not_ai of int  (** The requested player is human-controlled. *)
  | Player_is_eliminated of int  (** The requested AI has been eliminated. *)
  | Game_not_in_progress  (** Observations are only available during a game. *)

(** [observe state player_id] returns the AI player's own hand and public game
    information, or a specific error if the game/player is ineligible.
    The function does not require that it is that AI's turn. *)
val observe :
  Game_state.game_state -> int -> (observation, observation_error) result
