(** [active_player id kind] creates an active player with an empty hand. *)
let active_player player_id kind =
  {
    Kittens.Player.player_id;
    hand = [];
    status = Kittens.Player.Active;
    kind;
  }

(** A valid in-progress state containing one human and one AI player. *)
let valid_state =
  {
    Kittens.Game_state.draw_pile = [];
    discard_pile = [];
    players =
      [
        active_player 1 Kittens.Player.Human;
        active_player 2 Kittens.Player.AI;
      ];
    current_player_id = 1;
    direction = Kittens.Game_state.Clockwise;
    remaining_players = 2;
    status = Kittens.Game_state.In_progress;
  }

(** [has_error expected result] is true when [result] contains [expected]. *)
let has_error expected = function
  | Error errors -> List.mem expected errors
  | Ok () -> false

(** Each card maps to its specified rules category. *)
let test_card_categories () =
  assert (
    Kittens.Card.category Kittens.Card.Exploding_Kitten
    = Kittens.Card.Hazard_card);
  assert (
    Kittens.Card.category Kittens.Card.Defuse = Kittens.Card.Action_card);
  assert (Kittens.Card.category Kittens.Card.Skip = Kittens.Card.Action_card);
  assert (
    Kittens.Card.category Kittens.Card.Reverse = Kittens.Card.Action_card);
  assert (
    Kittens.Card.category Kittens.Card.Shuffle = Kittens.Card.Function_card);
  assert (
    Kittens.Card.category Kittens.Card.See_the_Future
    = Kittens.Card.Function_card)

(** A structurally consistent in-progress state passes validation. *)
let test_valid_in_progress_state () =
  assert (Kittens.Game_state.validate valid_state = Ok ())

(** Validation reports a mismatch between recorded and actual active players. *)
let test_remaining_players_mismatch () =
  assert
    (has_error
       (Kittens.Game_state.Remaining_players_mismatch
          { recorded = 1; actual = 2 })
       (Kittens.Game_state.validate
          { valid_state with remaining_players = 1 }))

(** Validation reports repeated player IDs. *)
let test_duplicate_player_ids () =
  assert
    (has_error (Kittens.Game_state.Duplicate_player_id 1)
       (Kittens.Game_state.validate
          {
            valid_state with
            players =
              [
                active_player 1 Kittens.Player.Human;
                active_player 1 Kittens.Player.AI;
              ];
          }))

(** Validation reports a current player ID absent from the player list. *)
let test_current_player_not_found () =
  assert
    (has_error (Kittens.Game_state.Current_player_not_found 9)
       (Kittens.Game_state.validate
          { valid_state with current_player_id = 9 }))

(** An in-progress game with fewer than two active players is invalid. *)
let test_in_progress_player_count () =
  assert
    (has_error (Kittens.Game_state.In_progress_player_count 1)
       (Kittens.Game_state.validate
          {
            valid_state with
            players =
              [
                active_player 1 Kittens.Player.Human;
                {
                  (active_player 2 Kittens.Player.AI) with
                  status = Kittens.Player.Eliminated;
                };
              ];
            remaining_players = 1;
          }))

(** An eliminated player cannot be the current player in an active game. *)
let test_current_player_eliminated () =
  assert
    (has_error (Kittens.Game_state.Current_player_eliminated 2)
       (Kittens.Game_state.validate
          {
            valid_state with
            players =
              [
                active_player 1 Kittens.Player.Human;
                {
                  (active_player 2 Kittens.Player.AI) with
                  status = Kittens.Player.Eliminated;
                };
                active_player 3 Kittens.Player.Human;
              ];
            current_player_id = 2;
          }))

(** A finished game with its sole active player as winner is valid. *)
let test_valid_finished_game () =
  assert
    (Kittens.Game_state.validate
       {
         valid_state with
         players = [ active_player 2 Kittens.Player.AI ];
         current_player_id = 2;
         remaining_players = 1;
         status = Kittens.Game_state.Finished 2;
       }
    = Ok ())

(** Validation rejects a finished-game winner who is not the active player. *)
let test_finished_winner_not_active () =
  assert
    (has_error (Kittens.Game_state.Finished_winner_not_active 1)
       (Kittens.Game_state.validate
          {
            valid_state with
            players = [ active_player 2 Kittens.Player.AI ];
            current_player_id = 2;
            remaining_players = 1;
            status = Kittens.Game_state.Finished 1;
          }))

(** An active AI receives its own hand and the expected public game data. *)
let test_ai_observes_own_state () =
  let state =
    {
      valid_state with
      draw_pile = [ Kittens.Card.Skip; Kittens.Card.Reverse ];
      discard_pile = [ Kittens.Card.Shuffle ];
      players =
        [
          active_player 1 Kittens.Player.Human;
          {
            (active_player 2 Kittens.Player.AI) with
            hand = [ Kittens.Card.Defuse; Kittens.Card.See_the_Future ];
          };
        ];
    }
  in
  match Kittens.Ai.observe state 2 with
  | Error _ -> assert false
  | Ok observation ->
      assert (observation.player_id = 2);
      assert
        (observation.hand
        = [ Kittens.Card.Defuse; Kittens.Card.See_the_Future ]);
      assert (observation.draw_pile_size = 2);
      assert (observation.discard_pile = [ Kittens.Card.Shuffle ]);
      assert (observation.current_player_id = 1);
      assert (observation.remaining_players = 2);
      assert (observation.game_status = Kittens.Game_state.In_progress);
      (match observation.player_summaries with
      | [ human; ai ] ->
          assert (human.player_id = 1);
          assert (human.kind = Kittens.Player.Human);
          assert (human.status = Kittens.Player.Active);
          assert (human.hand_size = 0);
          assert (ai.player_id = 2);
          assert (ai.kind = Kittens.Player.AI);
          assert (ai.status = Kittens.Player.Active);
          assert (ai.hand_size = 2)
      | _ -> assert false)

(** A human player cannot request an AI observation. *)
let test_human_cannot_request_ai_observation () =
  assert
    (Kittens.Ai.observe valid_state 1
    = Error (Kittens.Ai.Player_is_not_ai 1))

(** A missing player ID returns [Player_not_found]. *)
let test_ai_observation_player_not_found () =
  assert
    (Kittens.Ai.observe valid_state 99
    = Error (Kittens.Ai.Player_not_found 99))

(** An eliminated AI cannot request an observation. *)
let test_eliminated_ai_cannot_observe () =
  let state =
    {
      valid_state with
      players =
        [
          active_player 1 Kittens.Player.Human;
          {
            (active_player 2 Kittens.Player.AI) with
            status = Kittens.Player.Eliminated;
          };
        ];
    }
  in
  assert
    (Kittens.Ai.observe state 2
    = Error (Kittens.Ai.Player_is_eliminated 2))

(** Observations are rejected before a game starts. *)
let test_ai_observation_game_not_in_progress () =
  assert
    (Kittens.Ai.observe
       { valid_state with status = Kittens.Game_state.Not_started }
       2
    = Error Kittens.Ai.Game_not_in_progress)

(** Runs all card, game-state, and AI interface assertions. *)
let run_tests () =
  test_card_categories ();
  test_valid_in_progress_state ();
  test_remaining_players_mismatch ();
  test_duplicate_player_ids ();
  test_current_player_not_found ();
  test_in_progress_player_count ();
  test_current_player_eliminated ();
  test_valid_finished_game ();
  test_finished_winner_not_active ();
  test_ai_observes_own_state ();
  test_human_cannot_request_ai_observation ();
  test_ai_observation_player_not_found ();
  test_eliminated_ai_cannot_observe ();
  test_ai_observation_game_not_in_progress ()

let () = run_tests ()
