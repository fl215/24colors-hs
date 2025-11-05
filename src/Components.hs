module Components
  (Position(..),
   Velocity(..),
   Block(..),
   Player(..),
   Gravity(..),
   Action(..), ActionState(..),
   Jump(..),JumpCooldown(..),
   Solid(..),
   Size(..)) where

import Apecs
import Linear

data ActionState = Pressed | Released deriving Show

newtype Position = Position (V2 Float) deriving Show

newtype Velocity = Velocity (V2 Float) deriving Show

newtype Size = Size (V2 Float) deriving Show

data Action = ActLeft ActionState | ActRight ActionState deriving Show

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data Block = Block deriving Show

data Jump = Jump deriving Show
data JumpCooldown = JumpCooldown Float deriving Show

data Solid = Solid deriving Show

data Gravity = Gravity deriving Show

