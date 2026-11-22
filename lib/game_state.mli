(** The order in which turns advance through the player list. *)
type direction =
  | Clockwise  (** Advance clockwise. *)
  | Counterclockwise  (** Advance counterclockwise. *)

(** The lifecycle state of a game. *)
type status =
  | Not_started  (** Setup has not completed. *)
  | In_progress  (** Players are taking turns. *)
  | Finished of int  (** The game ended; the payload is the winner's ID. *)

(** Shared immutable snapshot of all game data, replaced when state changes.
    [remaining_players] must equal the number of active players. *)
type game_state = {
  draw_pile : Card.card list;  (** Cards available to draw, top card first. *)
  discard_pile : Card.card list;  (** Cards already discarded. *)
  players : Player.player list;  (** All players, including eliminated ones. *)
  current_player_id : int;  (** ID of the player whose turn is current. *)
  direction : direction;  (** Current turn order. *)
  remaining_players : int;  (** Number of players whose status is [Active]. *)
  status : status;  (** Whether the game has started or ended. *)
}

(** A violated structural or lifecycle invariant reported by [validate]. *)
type invariant_violation =
  | Duplicate_player_id of int
      (** The player ID occurs more than once. *)
  | Remaining_players_mismatch of { recorded : int; actual : int }
      (** [recorded] differs from the number of active players [actual]. *)
  | In_progress_player_count of int
      (** An in-progress game must have at least two active players. *)
  | Current_player_not_found of int
      (** The in-progress current player ID is absent from [players]. *)
  | Current_player_eliminated of int
      (** The in-progress current player has been eliminated. *)
  | Finished_player_count of int
      (** A finished game must have exactly one active player. *)
  | Finished_winner_not_active of int
      (** The declared winner is not the sole active player. *)

(** [validate state] checks player-ID uniqueness, the active-player count,
    and status-dependent current-player/winner invariants.
    It returns [Ok ()] when these invariants hold, otherwise [Error errors].
    This does not check deck composition, card conservation, or card effects;
    callers should run it after producing a complete state update. *)
val validate : game_state -> (unit, invariant_violation list) result

val end_turn : game_state -> game_state
(** [end_turn state] advances in the current direction, skipping eliminated
    players. If one active player remains, that player wins. Call during an
    in-progress game with the current player still in the seating list and
    at least one active player. The current player may have just been
    eliminated. *)
