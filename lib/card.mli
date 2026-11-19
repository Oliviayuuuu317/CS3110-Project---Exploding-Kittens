(** The six card kinds used by the game. *)
type card =
  | Exploding_Kitten  (** A hazard that can eliminate a player. *)
  | Defuse  (** Prevents elimination from an Exploding Kitten. *)
  | Skip  (** Ends the current turn without drawing. *)
  | Reverse  (** Reverses the direction of play. *)
  | Shuffle  (** Shuffles the draw pile. *)
  | See_the_Future  (** Reveals upcoming cards to the current player. *)

(** A basic player choice. Card-specific effects are handled by their owners. *)
type action =
  | Draw_card  (** Draw the top card of the draw pile. *)
  | Play_card of card  (** Play the given card from the player's hand. *)

(** The rules-level category of a card. *)
type card_category =
  | Action_card  (** Defuse, Skip, and Reverse. *)
  | Function_card  (** Shuffle and See the Future. *)
  | Hazard_card  (** Exploding Kitten. *)

(** [category card] returns the rules category assigned to [card]. *)
val category : card -> card_category
