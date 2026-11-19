type player_status =
  | Active
  | Eliminated

type player_kind =
  | Human
  | AI

type player = {
  player_id : int;
  hand : Card.card list;
  status : player_status;
  kind : player_kind;
}
