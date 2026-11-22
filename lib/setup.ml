type ai_strategy =
  | Random_play
  | Cautious
  | Aggressive

type player_config =
  | Human_player
  | Ai_player of ai_strategy

type ai_profile = {
  ai_id : int;
  strategy : ai_strategy;
  known_top_cards : Card.card list;
}

type setup = {
  state : Game_state.game_state;
  ai_profiles : ai_profile list;
}

type setup_error =
  | Too_few_players of int
  | Too_many_players of int

let min_players = 2
let max_players = 8
let starting_hand_size = 5
let extra_defuses = 2

(* Number of non-Defuse cards dealt to each player: the starting hand minus the
   one guaranteed Defuse. *)
let dealt_per_player = starting_hand_size - 1

(* [base_counts n] is the number of copies of each action/function card for an
   [n]-player game: 6n + 4 cards in total. Dealing uses 4n of them, so 2n + 4
   remain, and the draw pile starts with (2n + 4) + 2 Defuses + (n - 1)
   Exploding Kittens = 3n + 5 cards (11 for 2 players, 29 for 8 players). *)
let base_counts n =
  [
    (Card.Skip, (2 * n) + 1);
    (Card.Reverse, n + 1);
    (Card.Shuffle, n + 1);
    (Card.See_the_Future, (2 * n) + 1);
  ]

(* [replicate n x] is a list containing [n] copies of [x]. *)
let replicate n x = List.init n (fun _ -> x)

let base_deck n =
  List.concat_map (fun (card, k) -> replicate k card) (base_counts n)

let shuffle = Deck.shuffle

let count card cards =
  List.fold_left (fun acc c -> if c = card then acc + 1 else acc) 0 cards

let make_configs ~humans ~ais =
  replicate humans Human_player @ List.map (fun s -> Ai_player s) ais

let create_players configs =
  List.mapi
    (fun i config ->
      {
        Player.player_id = i + 1;
        hand = [];
        status = Player.Active;
        kind =
          (match config with
          | Human_player -> Player.Human
          | Ai_player _ -> Player.AI);
      })
    configs

let init_ai_profiles players configs =
  List.filter_map
    (fun (player, config) ->
      match config with
      | Human_player -> None
      | Ai_player strategy ->
          Some
            { ai_id = player.Player.player_id; strategy; known_top_cards = [] })
    (List.combine players configs)

(* [take n lst] splits [lst] into its first [n] elements and the rest. Requires:
   [List.length lst >= n]. *)
let take n lst =
  let rec go n acc rest =
    if n = 0 then (List.rev acc, rest)
    else
      match rest with
      | [] -> invalid_arg "Setup.take: not enough cards"
      | x :: xs -> go (n - 1) (x :: acc) xs
  in
  go n [] lst

(* [deal players deck] gives each player [dealt_per_player] cards from the top
   of [deck] plus one Defuse card, and returns the updated players together with
   the undealt remainder of [deck]. *)
let deal players deck =
  let players_rev, rest =
    List.fold_left
      (fun (acc, deck) player ->
        let hand, deck = take dealt_per_player deck in
        ({ player with Player.hand = Card.Defuse :: hand } :: acc, deck))
      ([], deck) players
  in
  (List.rev players_rev, rest)

(* [check configs] validates the number of seats. *)
let check configs =
  let n = List.length configs in
  if n < min_players then Error (Too_few_players n)
  else if n > max_players then Error (Too_many_players n)
  else Ok n

let init ?rng configs =
  match check configs with
  | Error e -> Error e
  | Ok n ->
      let rng =
        match rng with
        | Some r -> r
        | None -> Random.State.make_self_init ()
      in
      (* Steps 1-3: shuffle the base deck, deal 4 cards to each player, and give
         each player one Defuse. *)
      let players = create_players configs in
      let players, remaining = deal players (shuffle rng (base_deck n)) in
      (* Step 4: add the 2 extra Defuses and n - 1 Exploding Kittens to the
         remaining cards, then shuffle so their positions are unknown. *)
      let draw_pile =
        shuffle rng
          (remaining
          @ replicate extra_defuses Card.Defuse
          @ replicate (n - 1) Card.Exploding_Kitten)
      in
      let state =
        {
          Game_state.draw_pile;
          discard_pile = [];
          players;
          current_player_id = 1;
          direction = Game_state.Clockwise;
          remaining_players = n;
          status = Game_state.In_progress;
        }
      in
      Ok { state; ai_profiles = init_ai_profiles players configs }

let strategy_of setup player_id =
  List.find_map
    (fun p -> if p.ai_id = player_id then Some p.strategy else None)
    setup.ai_profiles
