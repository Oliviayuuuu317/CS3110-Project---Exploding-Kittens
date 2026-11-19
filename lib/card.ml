type card =
  | Exploding_Kitten
  | Defuse
  | Skip
  | Reverse
  | Shuffle
  | See_the_Future

type action =
  | Draw_card
  | Play_card of card

type card_category =
  | Action_card
  | Function_card
  | Hazard_card

let category = function
  | Exploding_Kitten -> Hazard_card
  | Defuse | Skip | Reverse -> Action_card
  | Shuffle | See_the_Future -> Function_card
