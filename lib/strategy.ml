(** [playable card] is true for cards a player may choose to play. *)
let playable = function
  | Card.Skip | Card.Reverse | Card.Shuffle | Card.See_the_Future -> true
  | Card.Defuse | Card.Exploding_Kitten -> false

(** [distinct cards] removes repeated cards and keeps the first occurrence of
    each one in order. *)
let distinct cards =
  List.rev
    (List.fold_left
       (fun acc card -> if List.mem card acc then acc else card :: acc)
       [] cards)

let legal_actions (obs : Ai.observation) =
  Card.Draw_card
  :: List.map
       (fun card -> Card.Play_card card)
       (distinct (List.filter playable obs.hand))

let kittens_in_pile (obs : Ai.observation) = obs.remaining_players - 1

(** [holds card obs] is true when the observing AI has [card] in its hand. *)
let holds card (obs : Ai.observation) = List.mem card obs.hand

(** [pick_random rng items] is a uniformly random element of [items].
    Requires: [items] is not empty. *)
let pick_random rng items =
  List.nth items (Random.State.int rng (List.length items))

(** [first_held cards obs] plays the first card in [cards] that the AI holds,
    or draws if it holds none of them. *)
let first_held cards obs =
  match List.find_opt (fun card -> holds card obs) cards with
  | Some card -> Card.Play_card card
  | None -> Card.Draw_card

(** What the AI believes about the top card of the draw pile. *)
type top_card =
  | Known_kitten
  | Known_safe
  | Unknown

(** [top_card profile] reads the top card from the AI's memory. *)
let top_card (profile : Setup.ai_profile) =
  match profile.known_top_cards with
  | Card.Exploding_Kitten :: _ -> Known_kitten
  | _ :: _ -> Known_safe
  | [] -> Unknown

(** [likely_kitten obs] is true when at least one in three cards left in the
    draw pile is a kitten. We picked one third because that is where drawing
    starts to feel too risky without any information. *)
let likely_kitten (obs : Ai.observation) =
  3 * kittens_in_pile obs >= obs.draw_pile_size

(** Cards that avoid drawing, in the order an AI would rather spend them. Skip
    and Reverse end the turn safely. Shuffle does not end the turn, but it
    moves a known kitten away from the top. *)
let escape_cards = [ Card.Skip; Card.Reverse; Card.Shuffle ]

let cautious profile obs =
  match top_card profile with
  | Known_kitten -> first_held escape_cards obs
  | Known_safe -> Card.Draw_card
  | Unknown ->
      if holds Card.See_the_Future obs then Card.Play_card Card.See_the_Future
      else if likely_kitten obs || not (holds Card.Defuse obs) then
        (* Without a Defuse any unknown draw could be the last one, so only
           Skip and Reverse are worth it here. Shuffling would not help since
           the top card would still be unknown. *)
        first_held [ Card.Skip; Card.Reverse ] obs
      else Card.Draw_card

let aggressive profile obs =
  if holds Card.Defuse obs then
    (* Drawing a kitten while holding a Defuse is fine for this AI, since it
       then gets to put the kitten on top for the next player. *)
    Card.Draw_card
  else
    match top_card profile with
    | Known_safe -> Card.Draw_card
    | Known_kitten -> first_held escape_cards obs
    | Unknown ->
        if holds Card.See_the_Future obs then
          Card.Play_card Card.See_the_Future
        else first_held [ Card.Skip; Card.Reverse ] obs

let choose_action rng (profile : Setup.ai_profile) obs =
  match profile.strategy with
  | Setup.Random_play -> pick_random rng (legal_actions obs)
  | Setup.Cautious -> cautious profile obs
  | Setup.Aggressive -> aggressive profile obs

(** [kitten_choices size] is the rules' choices of 1st, 2nd, 3rd, 4th, or last
    as zero-based indexes, without duplicates when the pile is small. *)
let kitten_choices size =
  distinct (List.filter (fun i -> i <= size) [ 0; 1; 2; 3 ] @ [ size ])

let choose_kitten_position rng (profile : Setup.ai_profile) size =
  match profile.strategy with
  | Setup.Random_play -> pick_random rng (kitten_choices size)
  | Setup.Cautious -> size
  | Setup.Aggressive -> 0

(** [drop_top cards] removes the first card, if there is one. *)
let drop_top = function
  | [] -> []
  | _ :: rest -> rest

(** [insert_known index known] adds the kitten to the remembered cards when it
    was placed at [index]. If [index] is past the remembered part, the cards
    the AI knows are still on top and nothing changes. *)
let insert_known index known =
  if index > List.length known then known
  else
    List.filteri (fun i _ -> i < index) known
    @ (Card.Exploding_Kitten :: List.filteri (fun i _ -> i >= index) known)

let remember (profile : Setup.ai_profile) ~actor outcome =
  let mine = actor = profile.ai_id in
  let known = profile.known_top_cards in
  let known =
    match outcome with
    | Deck.Saw_the_future cards -> if mine then cards else known
    | Deck.Drew_card _ | Deck.Player_eliminated _ -> drop_top known
    | Deck.Defused_kitten index ->
        if mine then insert_known index (drop_top known) else []
    | Deck.Played_card Card.Shuffle -> []
    | Deck.Played_card _ -> known
  in
  { profile with known_top_cards = known }

let remember_all profiles ~actor outcome =
  List.map (fun profile -> remember profile ~actor outcome) profiles
