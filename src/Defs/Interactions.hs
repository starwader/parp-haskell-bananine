module Defs.Interactions where

data Interaction
  = Attack
  | Talk
  | Pickup
  | Drop
  | Open
  | Put
  | Takeout
  deriving (Eq)
