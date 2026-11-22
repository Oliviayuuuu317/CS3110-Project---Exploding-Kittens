open Kittens

let player ?(kind = Player.Human) ?(status = Player.Active) player_id hand =
  { Player.player_id; hand; status; kind }

let ai = player ~kind:Player.AI 2

let state ?(direction = Game_state.Clockwise) ?(current_player_id = 1)
    ?(discard_pile = []) players draw_pile =
  {
    Game_state.players;
    draw_pile;
    discard_pile;
    current_player_id;
    direction;
    remaining_players =
      List.length (List.filter (fun p -> p.Player.status = Player.Active) players);
    status = Game_state.In_progress;
  }

let rng () = Random.State.make [| 42 |]

let all_cards (state : Game_state.game_state) =
  List.sort compare
    (state.draw_pile @ state.discard_pile
    @ List.concat_map (fun p -> p.Player.hand) state.players)

(** [checked before result] unwraps a successful action and checks that the
    new state is valid and every card is accounted for across piles and hands. *)
let checked before = function
  | Error _ -> failwith "Expected a successful deck action"
  | Ok (after, outcome) ->
      assert (Game_state.validate after = Ok ());
      assert (all_cards before = all_cards after);
      (after, outcome)

(** A human draw moves the top card into their hand. The AI then discards
    one copy of Skip and passes the turn without drawing. *)
let test_draw_and_skip () =
  let before =
    state [ player 1 []; ai [ Card.Skip; Card.Defuse; Card.Skip ] ]
      [ Card.Shuffle; Card.Reverse ]
  in
  let after, outcome = checked before (Deck.draw_card (rng ()) before ~player_id:1) in
  assert (outcome = Deck.Drew_card Card.Shuffle);
  assert (after.players = [ player 1 [ Card.Shuffle ]; ai [ Card.Skip; Card.Defuse; Card.Skip ] ]);
  assert (after.draw_pile = [ Card.Reverse ]);
  assert (after.current_player_id = 2);
  let skipped, outcome =
    checked after (Deck.apply_action (rng ()) after ~player_id:2 (Card.Play_card Card.Skip))
  in
  assert (outcome = Deck.Played_card Card.Skip);
  assert (skipped.players = [ player 1 [ Card.Shuffle ]; ai [ Card.Defuse; Card.Skip ] ]);
  assert (skipped.draw_pile = after.draw_pile);
  assert (skipped.discard_pile = [ Card.Skip ]);
  assert (skipped.current_player_id = 1)

(** Shuffling preserves the cards, including for empty and singleton piles.
    Playing Shuffle discards the card and lets the current player continue. *)
let test_shuffle () =
  let pile = Card.[ Skip; Reverse; Defuse; See_the_Future; Shuffle; Exploding_Kitten ] in
  let expected = Deck.shuffle (rng ()) pile in
  assert (List.sort compare expected = List.sort compare pile);
  assert (List.exists (fun seed -> Deck.shuffle (Random.State.make [| seed |]) pile <> pile) [ 1; 2; 3 ]);
  assert (Deck.shuffle (rng ()) [] = []);
  assert (Deck.shuffle (rng ()) [ Card.Skip ] = [ Card.Skip ]);
  let before = state [ player 1 [ Card.Shuffle ]; ai [] ] pile in
  assert (Deck.shuffle_draw_pile (rng ()) before = { before with draw_pile = expected });
  let after, outcome =
    checked before (Deck.play_card (rng ()) before ~player_id:1 Card.Shuffle)
  in
  assert (outcome = Deck.Played_card Card.Shuffle);
  assert (after.draw_pile = expected);
  assert (after.discard_pile = [ Card.Shuffle ]);
  assert (after.current_player_id = 1)

(** Drawing a kitten spends exactly one Defuse, returns the kitten to the
    draw pile, and advances the turn. *)
let test_defuse () =
  let before =
    state ~current_player_id:2
      [ player 1 []; ai [ Card.Defuse; Card.Defuse; Card.Skip ] ]
      [ Card.Exploding_Kitten ]
  in
  let after, outcome = checked before (Deck.draw_card (rng ()) before ~player_id:2) in
  assert (outcome = Deck.Defused_kitten);
  assert (after.draw_pile = [ Card.Exploding_Kitten ]);
  assert (after.discard_pile = [ Card.Defuse ]);
  assert (after.players = [ player 1 []; ai [ Card.Defuse; Card.Skip ] ]);
  assert (after.current_player_id = 1)

(** Drawing a kitten without a Defuse discards the player's hand, skips
    eliminated seats, and declares a winner once one player remains. *)
let test_elimination_and_winner () =
  let before =
    state ~direction:Game_state.Counterclockwise ~current_player_id:2
      [ player ~status:Player.Eliminated 1 []; ai [ Card.Skip ]; player 3 []; player 4 [] ]
      [ Card.Exploding_Kitten; Card.Exploding_Kitten ]
  in
  let after, outcome = checked before (Deck.draw_card (rng ()) before ~player_id:2) in
  assert (outcome = Deck.Player_eliminated 2);
  assert (after.players =
          [ player ~status:Player.Eliminated 1 [];
            player ~kind:Player.AI ~status:Player.Eliminated 2 [];
            player 3 []; player 4 [] ]);
  assert (after.discard_pile = [ Card.Exploding_Kitten; Card.Skip ]);
  assert (after.remaining_players = 2);
  assert (after.current_player_id = 4);
  let finished, outcome = checked after (Deck.draw_card (rng ()) after ~player_id:4) in
  assert (outcome = Deck.Player_eliminated 4);
  assert (finished.remaining_players = 1);
  assert (finished.status = Game_state.Finished 3);
  assert (finished.current_player_id = 3);
  assert (Deck.draw_card (rng ()) finished ~player_id:3 = Error Deck.Game_not_in_progress)

(** Invalid actions report the expected errors and leave the random number
    generator unchanged. *)
let test_invalid_actions () =
  let before = state ~current_player_id:2 [ player 1 []; ai [ Card.Defuse ] ] [] in
  let random = rng () in
  assert (Deck.draw_card random before ~player_id:1 = Error (Deck.Not_current_player 1));
  assert (Deck.draw_card random before ~player_id:99 = Error (Deck.Not_current_player 99));
  assert (Deck.draw_card random before ~player_id:2 = Error Deck.Empty_draw_pile);
  assert (Deck.play_card random before ~player_id:2 Card.Skip = Error (Deck.Card_not_in_hand Card.Skip));
  assert (Deck.play_card random before ~player_id:2 Card.Defuse = Error (Deck.Card_cannot_be_played Card.Defuse));
  let invalid = { before with current_player_id = 3 } in
  assert (Deck.draw_card random invalid ~player_id:3 =
          Error (Deck.Invalid_state [ Game_state.Current_player_not_found 3 ]));
  assert (Random.State.bits random = Random.State.bits (rng ()))

(** Plays mixed human and AI seats from a real deal through the last
    elimination. Checks every transition and bounds the number of actions. *)
let test_complete_games () =
  let choose_action random hand =
    let playable = function
      | Card.Skip | Card.Shuffle -> true
      | Card.Defuse | Card.Exploding_Kitten | Card.Reverse | Card.See_the_Future ->
          false
    in
    if Random.State.bool random then
      match List.find_opt playable hand with
      | Some card -> Card.Play_card card
      | None -> Card.Draw_card
    else Card.Draw_card
  in
  let rec play_game random actions_left (state : Game_state.game_state) =
    match state.status with
    | Game_state.Finished winner ->
        assert (state.current_player_id = winner);
        assert (state.remaining_players = 1)
    | Game_state.Not_started -> failwith "Expected an initialized game"
    | Game_state.In_progress ->
        assert (actions_left > 0);
        let current =
          List.find
            (fun p -> p.Player.player_id = state.current_player_id)
            state.players
        in
        let hand =
          match current.kind with
          | Player.Human -> current.hand
          | Player.AI -> (
              match Ai.observe state current.player_id with
              | Ok observation ->
                  assert (observation.hand = current.hand);
                  observation.hand
              | Error _ -> failwith "Expected an observation for the current AI")
        in
        let action = choose_action random hand in
        let next, _ =
          checked state
            (Deck.apply_action random state ~player_id:current.player_id action)
        in
        play_game random (actions_left - 1) next
  in
  for seats = Setup.min_players to Setup.max_players do
    let configs =
      Setup.make_configs ~humans:1
        ~ais:(List.init (seats - 1) (fun _ -> Setup.Random_play))
    in
    let random = Random.State.make [| seats |] in
    match Setup.init ~rng:random configs with
    | Error _ -> failwith "Expected a valid player configuration"
    | Ok { state; _ } ->
        (* Each card can enter a hand and be discarded once. A kitten that
           returns to the pile costs a Defuse, which is also discarded. *)
        play_game random (2 * List.length (all_cards state)) state
  done

let () =
  test_draw_and_skip ();
  test_shuffle ();
  test_defuse ();
  test_elimination_and_winner ();
  test_invalid_actions ();
  test_complete_games ()
