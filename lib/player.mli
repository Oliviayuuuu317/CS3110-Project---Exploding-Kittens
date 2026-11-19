(** Whether a player may continue participating in the game. *)
type player_status =
  | Active  (** The player has not been eliminated. *)
  | Eliminated  (** The player has been eliminated. *)

(** The controller type for a player. *)
type player_kind =
  | Human  (** A player controlled by a person. *)
  | AI  (** A player controlled by an AI strategy. *)

(** A player's identity, cards, status, and controller type. *)
type player = {
  player_id : int;  (** Unique identifier within a game. *)
  hand : Card.card list;  (** Cards currently held by the player. *)
  status : player_status;  (** Whether the player is still active. *)
  kind : player_kind;  (** Whether the player is human- or AI-controlled. *)
}