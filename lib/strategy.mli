(** AI decision-making for the three strategies in {!Setup.ai_strategy}.

    The game loop uses this module in three places on an AI player's turn:
    + Call {!choose_action} with the AI's profile and its {!Ai.observation},
      then pass the action to {!Deck.apply_action}.
    + Pass [choose_kitten_position rng profile] as [~place_kitten] so the AI
      decides where a defused kitten goes.
    + After {e every} successful action, by any player, call {!remember_all}
      so each AI keeps its See the Future memory up to date.

    A strategy only uses the AI's own observation and its own memory. It never
    looks at other players' hands or at what other players saw. *)

val legal_actions : Ai.observation -> Card.action list
(** [legal_actions obs] is every action the observing AI is allowed to take
    right now: [Draw_card] first, then one [Play_card] for each distinct
    playable card in its hand (Skip, Reverse, Shuffle, See the Future). Defuse
    and Exploding Kitten are never included. *)

val kittens_in_pile : Ai.observation -> int
(** [kittens_in_pile obs] is how many Exploding Kittens are still in the draw
    pile. A game starts with one fewer kitten than players, and each
    elimination discards one kitten, so this is [remaining_players - 1]. *)

val choose_action :
  Random.State.t -> Setup.ai_profile -> Ai.observation -> Card.action
(** [choose_action rng profile obs] picks one action from [legal_actions obs]
    using [profile.strategy]:
    - [Random_play] picks uniformly among the legal actions.
    - [Cautious] plays See the Future when it does not know the top card,
      avoids drawing with Skip, Reverse, or Shuffle when a kitten is known or
      likely to be on top, and otherwise draws.
    - [Aggressive] draws whenever it holds a Defuse. Without a Defuse it uses
      See the Future first, draws if the top card is known to be safe, and
      otherwise spends Skip, Reverse, or Shuffle.

    The result is always legal for [obs]. Requires: [obs] belongs to the AI in
    [profile]. *)

val choose_kitten_position : Random.State.t -> Setup.ai_profile -> int -> int
(** [choose_kitten_position rng profile size] is where the AI puts a defused
    kitten back into a draw pile of [size] cards. The answer is a zero-based
    index from the rules' choices: 1st, 2nd, 3rd, 4th, or last.
    - [Random_play] picks one of those choices uniformly.
    - [Cautious] puts the kitten on the bottom, as far from its next turn as
      possible.
    - [Aggressive] puts the kitten on top so the next player draws it.

    Requires: [size >= 0]. The result is between [0] and [size]. *)

val remember :
  Setup.ai_profile -> actor:int -> Deck.outcome -> Setup.ai_profile
(** [remember profile ~actor outcome] updates [profile.known_top_cards] after
    player [actor] got [outcome] from {!Deck.apply_action}.
    - The AI's own See the Future replaces its memory with the revealed cards.
    - Any draw or elimination removes the top card it remembered.
    - Any Shuffle clears the memory.
    - The AI's own Defuse puts the kitten into its memory if the kitten landed
      inside the part it already knew. Another player's Defuse clears the
      memory, since the position is secret.
    - Skip, Reverse, and other players' See the Future change nothing. *)

val remember_all :
  Setup.ai_profile list -> actor:int -> Deck.outcome -> Setup.ai_profile list
(** [remember_all profiles ~actor outcome] applies {!remember} to every
    profile. *)
