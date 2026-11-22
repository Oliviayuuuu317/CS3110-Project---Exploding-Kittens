open Kittens

let player ?(kind = Player.Human) ?(status = Player.Active) player_id hand =
  { Player.player_id; hand; status; kind }

let ai = player ~kind:Player.AI

let state ?(direction = Game_state.Clockwise) ?(current_player_id = 1) players
    draw_pile =
  {
    Game_state.players;
    draw_pile;
    discard_pile = [];
    current_player_id;
    direction;
    remaining_players =
      List.length
        (List.filter (fun p -> p.Player.status = Player.Active) players);
    status = Game_state.In_progress;
  }

let rng () = Random.State.make [| 42 |]

let all_cards (state : Game_state.game_state) =
  List.sort compare
    (state.draw_pile @ state.discard_pile
    @ List.concat_map (fun p -> p.Player.hand) state.players)

(** [checked before result] unwraps a successful action and checks that the
    new state is valid and no card was lost or created. *)
let checked before = function
  | Error _ -> failwith "Expected a successful action"
  | Ok (after, outcome) ->
      assert (Game_state.validate after = Ok ());
      assert (all_cards before = all_cards after);
      (after, outcome)

let profile ?(known = []) ai_id strategy =
  { Setup.ai_id; strategy; known_top_cards = known }

(** [observe state id] is the observation for AI [id], which must exist. *)
let observe state id =
  match Ai.observe state id with
  | Ok obs -> obs
  | Error _ -> failwith "Expected an observation"

(* ---------- Reverse ---------- *)

(** Reverse flips the direction before ending the turn, so with three players
    the turn goes from player 1 back to player 3. *)
let test_reverse () =
  let before =
    state [ player 1 [ Card.Reverse; Card.Skip ]; player 2 []; player 3 [] ]
      [ Card.Defuse ]
  in
  let after, outcome =
    checked before (Deck.play_card (rng ()) before ~player_id:1 Card.Reverse)
  in
  assert (outcome = Deck.Played_card Card.Reverse);
  assert (after.direction = Game_state.Counterclockwise);
  assert (after.current_player_id = 3);
  assert (after.discard_pile = [ Card.Reverse ]);
  assert (after.draw_pile = before.draw_pile);
  assert (List.hd after.players = player 1 [ Card.Skip ]);
  (* Reversing again goes back to clockwise. *)
  let after = { after with players = [ player 1 []; player 2 []; player 3 [ Card.Reverse ] ] } in
  let again, _ =
    checked after (Deck.play_card (rng ()) after ~player_id:3 Card.Reverse)
  in
  assert (again.direction = Game_state.Clockwise);
  assert (again.current_player_id = 1)

(** Reverse skips eliminated seats in the new direction, and with only two
    players left it hands the turn to the other player like Skip. *)
let test_reverse_edge_cases () =
  let before =
    state
      [ player 1 [ Card.Reverse ]; player 2 [];
        player 3 []; player ~status:Player.Eliminated 4 [] ]
      []
  in
  let after, _ =
    checked before (Deck.play_card (rng ()) before ~player_id:1 Card.Reverse)
  in
  assert (after.current_player_id = 3);
  let two = state ~current_player_id:2 [ player 1 []; ai 2 [ Card.Reverse ] ] [] in
  let after, _ = checked two (Deck.play_card (rng ()) two ~player_id:2 Card.Reverse) in
  assert (after.current_player_id = 1);
  assert (after.direction = Game_state.Counterclockwise)

(* ---------- See the Future ---------- *)

(** See the Future shows the top three cards in order, keeps the draw pile as
    it was, discards the card, and lets the same player continue. *)
let test_see_the_future () =
  let pile = Card.[ Skip; Exploding_Kitten; Defuse; Shuffle ] in
  let before = state [ player 1 [ Card.See_the_Future; Card.Skip ]; player 2 [] ] pile in
  let after, outcome =
    checked before
      (Deck.play_card (rng ()) before ~player_id:1 Card.See_the_Future)
  in
  assert (outcome = Deck.Saw_the_future Card.[ Skip; Exploding_Kitten; Defuse ]);
  assert (after.draw_pile = pile);
  assert (after.discard_pile = [ Card.See_the_Future ]);
  assert (after.current_player_id = 1);
  assert (List.hd after.players = player 1 [ Card.Skip ])

(** With fewer than three cards left, See the Future shows what is there. *)
let test_see_the_future_small_pile () =
  let see pile =
    let before = state [ player 1 [ Card.See_the_Future ]; player 2 [] ] pile in
    snd (checked before (Deck.play_card (rng ()) before ~player_id:1 Card.See_the_Future))
  in
  assert (see [ Card.Skip; Card.Reverse ] = Deck.Saw_the_future [ Card.Skip; Card.Reverse ]);
  assert (see [ Card.Exploding_Kitten ] = Deck.Saw_the_future [ Card.Exploding_Kitten ]);
  assert (see [] = Deck.Saw_the_future [])

(* ---------- Defuse position ---------- *)

(** The player chooses where the defused kitten goes. The chooser is told the
    pile size without the kitten, and its answer is clamped to the pile. *)
let test_choose_kitten_position () =
  let pile = Card.[ Exploding_Kitten; Skip; Reverse; Shuffle; Skip; Reverse ] in
  let defuse choose =
    let before = state [ player 1 [ Card.Defuse ]; player 2 [] ] pile in
    checked before
      (Deck.draw_card ~place_kitten:choose (rng ()) before ~player_id:1)
  in
  let after, outcome =
    defuse (fun size ->
        assert (size = 5);
        2)
  in
  assert (outcome = Deck.Defused_kitten 2);
  assert (after.draw_pile = Card.[ Skip; Reverse; Exploding_Kitten; Shuffle; Skip; Reverse ]);
  assert (after.discard_pile = [ Card.Defuse ]);
  assert (after.current_player_id = 2);
  let after, outcome = defuse (fun _ -> 100) in
  assert (outcome = Deck.Defused_kitten 5);
  assert (List.nth after.draw_pile 5 = Card.Exploding_Kitten);
  let after, outcome = defuse (fun _ -> -3) in
  assert (outcome = Deck.Defused_kitten 0);
  assert (List.hd after.draw_pile = Card.Exploding_Kitten)

(** Without a chooser the kitten still goes back somewhere in the pile. *)
let test_default_kitten_position () =
  let pile = Card.[ Exploding_Kitten; Skip; Reverse; Shuffle ] in
  let before = state [ player 1 [ Card.Defuse ]; player 2 [] ] pile in
  match checked before (Deck.draw_card (rng ()) before ~player_id:1) with
  | after, Deck.Defused_kitten index ->
      assert (0 <= index && index <= 3);
      assert (List.nth after.draw_pile index = Card.Exploding_Kitten)
  | _ -> failwith "Expected a defused kitten"

(* ---------- Strategy: legal actions and memory ---------- *)

(** Legal actions list Draw first, then each playable card once. Defuse is
    never offered, and kittens in the pile follow the remaining players. *)
let test_legal_actions () =
  let s =
    state ~current_player_id:2
      [ player 1 []; ai 2 Card.[ Defuse; Skip; See_the_Future; Skip ]; player 3 [] ]
      Card.[ Exploding_Kitten; Exploding_Kitten; Skip ]
  in
  let obs = observe s 2 in
  assert (
    Strategy.legal_actions obs
    = Card.[ Draw_card; Play_card Skip; Play_card See_the_Future ]);
  assert (Strategy.kittens_in_pile obs = 2);
  let empty = observe (state [ ai 1 []; player 2 [] ] []) 1 in
  assert (Strategy.legal_actions empty = [ Card.Draw_card ])

(** Memory follows the pile: own See the Future fills it, draws pop it,
    Shuffle and other players' Defuse clear it, and the AI's own Defuse
    records where it put the kitten. *)
let test_remember () =
  let p = profile 2 Setup.Cautious in
  let p = Strategy.remember p ~actor:1 (Deck.Saw_the_future Card.[ Skip ]) in
  assert (p.known_top_cards = []);
  let seen = Card.[ Skip; Exploding_Kitten; Reverse ] in
  let p = Strategy.remember p ~actor:2 (Deck.Saw_the_future seen) in
  assert (p.known_top_cards = seen);
  assert (Strategy.remember p ~actor:1 (Deck.Played_card Card.Skip) = p);
  assert (Strategy.remember p ~actor:3 (Deck.Played_card Card.Reverse) = p);
  let p = Strategy.remember p ~actor:1 (Deck.Drew_card Card.Skip) in
  assert (p.known_top_cards = Card.[ Exploding_Kitten; Reverse ]);
  assert (
    (Strategy.remember p ~actor:3 (Deck.Player_eliminated 3)).known_top_cards
    = [ Card.Reverse ]);
  assert ((Strategy.remember p ~actor:1 (Deck.Played_card Card.Shuffle)).known_top_cards = []);
  assert ((Strategy.remember p ~actor:1 (Deck.Defused_kitten 0)).known_top_cards = []);
  let mine index = (Strategy.remember p ~actor:2 (Deck.Defused_kitten index)).known_top_cards in
  assert (mine 0 = Card.[ Exploding_Kitten; Reverse ]);
  assert (mine 1 = Card.[ Reverse; Exploding_Kitten ]);
  assert (mine 4 = [ Card.Reverse ]);
  assert (
    Strategy.remember_all [ p; profile 3 Setup.Aggressive ] ~actor:1
      (Deck.Played_card Card.Shuffle)
    = [ { p with known_top_cards = [] }; profile 3 Setup.Aggressive ])

(* ---------- Strategy: decisions ---------- *)

(** [decide ?known strategy hand pile] is what an AI with [hand] picks when
    the draw pile is [pile] and its memory is [known]. *)
let decide ?(known = []) strategy hand pile =
  let s = state ~current_player_id:2 [ player 1 []; ai 2 hand ] pile in
  Strategy.choose_action (rng ()) (profile ~known 2 strategy) (observe s 2)

(** A pile of [n] safe cards. With two players there is one kitten. *)
let safe n = List.init n (fun _ -> Card.Skip)

(** Random play always picks a legal action and over many seeds tries each
    of them. *)
let test_random_play () =
  let s =
    state ~current_player_id:2
      [ player 1 []; ai 2 Card.[ Defuse; Skip; Reverse; Shuffle; See_the_Future ] ]
      (safe 5)
  in
  let obs = observe s 2 in
  let legal = Strategy.legal_actions obs in
  let picks =
    List.init 200 (fun seed ->
        Strategy.choose_action (Random.State.make [| seed |])
          (profile 2 Setup.Random_play) obs)
  in
  List.iter (fun action -> assert (List.mem action legal)) picks;
  List.iter (fun action -> assert (List.mem action picks)) legal

(** Cautious looks ahead when it can, avoids a known or likely kitten, and
    draws when the top card is safe or the risk is low. *)
let test_cautious () =
  let c = Setup.Cautious in
  assert (decide c Card.[ See_the_Future; Skip ] (safe 9) = Card.Play_card Card.See_the_Future);
  assert (
    decide ~known:[ Card.Exploding_Kitten ] c Card.[ See_the_Future; Shuffle; Skip ] (safe 9)
    = Card.Play_card Card.Skip);
  assert (
    decide ~known:[ Card.Exploding_Kitten ] c [ Card.Shuffle ] (safe 9)
    = Card.Play_card Card.Shuffle);
  assert (decide ~known:[ Card.Skip ] c Card.[ Skip; Reverse ] (safe 9) = Card.Draw_card);
  (* One kitten in a pile of 3 is likely enough to avoid drawing. *)
  assert (decide c Card.[ Defuse; Reverse ] (safe 3) = Card.Play_card Card.Reverse);
  assert (decide c Card.[ Defuse; Reverse ] (safe 9) = Card.Draw_card);
  (* Without a Defuse even a low risk is avoided, but Shuffle is kept. *)
  assert (decide c Card.[ Shuffle; Skip ] (safe 9) = Card.Play_card Card.Skip);
  assert (decide c [ Card.Shuffle ] (safe 9) = Card.Draw_card)

(** Aggressive draws whenever it has a Defuse and only spends its other cards
    once the Defuse is gone. *)
let test_aggressive () =
  let a = Setup.Aggressive in
  assert (
    decide ~known:[ Card.Exploding_Kitten ] a Card.[ Defuse; Skip; See_the_Future ] (safe 2)
    = Card.Draw_card);
  assert (decide a Card.[ See_the_Future; Skip ] (safe 2) = Card.Play_card Card.See_the_Future);
  assert (decide ~known:[ Card.Reverse ] a Card.[ Skip; Reverse ] (safe 2) = Card.Draw_card);
  assert (
    decide ~known:[ Card.Exploding_Kitten ] a [ Card.Shuffle ] (safe 2)
    = Card.Play_card Card.Shuffle);
  assert (decide a Card.[ Shuffle; Reverse ] (safe 2) = Card.Play_card Card.Reverse);
  assert (decide a [] (safe 2) = Card.Draw_card)

(** Each strategy places a defused kitten at one of the rules' choices. *)
let test_kitten_position_choices () =
  let place strategy seed size =
    Strategy.choose_kitten_position (Random.State.make [| seed |])
      (profile 1 strategy) size
  in
  assert (place Setup.Cautious 0 7 = 7);
  assert (place Setup.Aggressive 0 7 = 0);
  let picks size = List.sort_uniq compare (List.init 100 (fun seed -> place Setup.Random_play seed size)) in
  assert (picks 7 = [ 0; 1; 2; 3; 7 ]);
  assert (picks 2 = [ 0; 1; 2 ]);
  assert (picks 0 = [ 0 ])

(* ---------- Full games ---------- *)

(** [prefix known pile] is true when [known] is the start of [pile]. *)
let rec prefix known pile =
  match (known, pile) with
  | [], _ -> true
  | k :: known, p :: pile -> k = p && prefix known pile
  | _ :: _, [] -> false

(** Plays all-AI games from a real deal to a winner with every strategy mix.
    Every action must be legal and keep the state valid, and every AI's
    memory must always match the real top of the draw pile. *)
let test_complete_ai_games () =
  let mixes =
    Setup.[ [ Random_play ]; [ Cautious ]; [ Aggressive ];
            [ Random_play; Cautious; Aggressive ] ]
  in
  let rec play random actions_left profiles (state : Game_state.game_state) =
    List.iter
      (fun p -> assert (prefix p.Setup.known_top_cards state.draw_pile))
      profiles;
    match state.status with
    | Game_state.Finished winner ->
        assert (state.current_player_id = winner);
        assert (state.remaining_players = 1)
    | Game_state.Not_started -> failwith "Expected an initialized game"
    | Game_state.In_progress ->
        assert (actions_left > 0);
        let id = state.current_player_id in
        let me = List.find (fun p -> p.Setup.ai_id = id) profiles in
        let obs = observe state id in
        let action = Strategy.choose_action random me obs in
        assert (List.mem action (Strategy.legal_actions obs));
        let next, outcome =
          checked state
            (Deck.apply_action
               ~place_kitten:(Strategy.choose_kitten_position random me)
               random state ~player_id:id action)
        in
        play random (actions_left - 1)
          (Strategy.remember_all profiles ~actor:id outcome)
          next
  in
  List.iter
    (fun mix ->
      for seats = Setup.min_players to Setup.max_players do
        for seed = 1 to 5 do
          let ais = List.init seats (fun i -> List.nth mix (i mod List.length mix)) in
          let random = Random.State.make [| seats; seed |] in
          match Setup.init ~rng:random (Setup.make_configs ~humans:0 ~ais) with
          | Error _ -> failwith "Expected a valid player configuration"
          | Ok { state; ai_profiles } ->
              play random (2 * List.length (all_cards state)) ai_profiles state
        done
      done)
    mixes

let () =
  test_reverse ();
  test_reverse_edge_cases ();
  test_see_the_future ();
  test_see_the_future_small_pile ();
  test_choose_kitten_position ();
  test_default_kitten_position ();
  test_legal_actions ();
  test_remember ();
  test_random_play ();
  test_cautious ();
  test_aggressive ();
  test_kitten_position_choices ();
  test_complete_ai_games ()
