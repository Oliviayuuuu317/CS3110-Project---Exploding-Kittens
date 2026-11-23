# CS3110-Project---Exploding-Kittens--Team 18 

## Team Members
- Daniel Chen (yc2886)
- Olivia Yu (qy265)
- Eleene Yang (sy848)
- Lora Ding (yd464)
- Suzy Zeng (xz952)

## MS2 version

This submission is based on commit
aff5e6b95502813ce2cb34e37c5e67f08fdf1eb3.

It includes game-state validation and setup for two to eight
players. Deck operations support drawing, Skip, and Shuffle,
along with automatic Defuse use and player elimination.
Turn progression and winner detection are also implemented.

AI setup and observations are available. AI decision-making,
Reverse, See the Future, and player-selected kitten placement
are planned for MS3.

## Build and test

Run these commands from the project root:

```sh
dune build
dune test
```

The terminal interface is still under development.
`dune exec kittens` currently prints a placeholder.
Complete-game simulations are included in the automated tests.