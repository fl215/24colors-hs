module Game(runGame, collidesWith, collidesWithAny) where

import Apecs
import Apecs.TH (makeWorld, makeMapComponents)
import Apecs.Brillo

import System.Random
import System.Exit
import Control.Monad

import Linear

import Components

makeMapComponents [''Position, ''Velocity, ''Block, ''Gravity, ''Action, ''Jump, ''JumpCooldown, ''Solid, ''Size]
makeWorld "World" [''Position, ''Velocity, ''Block, ''Player, ''Gravity, ''Action, ''Jump, ''JumpCooldown, ''Solid, ''Size, ''Camera]

type System' a = System World a

gravity :: V2 Float
gravity = V2 0 (-800)

initialize :: System' ()
initialize = do
  playerEty <- newEntity (Player, Gravity, Position (V2 0 0), Velocity (V2 0 0), Size (V2 32 32))
  blockEty <- newEntity (Block, Solid, Position (V2 (-240) (-128)), Size (V2 128 32))
  blockEty2 <- newEntity (Block, Solid, Position (V2 0 (-128)), Size (V2 128 32))
  return ()

stepGravity :: Float -> System' ()
stepGravity dT = cmap $ \(Gravity, Velocity v) -> Velocity $ v ^+^ dT *^ gravity

stepPosition :: Float -> System' ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position $ p + dT *^ v

doMove :: System' ()
doMove = do
  cmap $ \(a, v@(Velocity (V2 _ y))) ->
    case a of
      ActLeft Pressed     -> Velocity $ V2 (-200) y
      ActRight Pressed    -> Velocity $ V2 200 y
      _ -> v
  cmap $ \(a, v@(Velocity (V2 _ y))) ->
    if isReleased a
       then Left $ (Not @Action, Velocity $ V2 0 y)
       else Right $ v
    where
      isReleased (ActLeft Released) = True
      isReleased (ActRight Released) = True
      isReleased _ = False

doJump :: System' ()
doJump = cmap $ \(Jump, Velocity (V2 x y)) -> (Not @Jump, Velocity $ V2 x (y+400))

stepJumpCooldown :: Float -> System' ()
stepJumpCooldown dT = cmap $ \(JumpCooldown c) ->
  if c < 0
    then Left $ Not @(JumpCooldown)
    else Right $ JumpCooldown $ c-dT

collidesWith :: (Size, Position) -> (Size, Position) -> Bool
collidesWith (Size (V2 boxSizeX boxSizeY), Position (V2 boxPosX boxPosY))
             (Size (V2 plrSizeX plrSizeY), Position (V2 plrPosX plrPosY))
  |    (abs (plrPosX - boxPosX)) * 2 < (plrSizeX + boxSizeX)
    && (abs (plrPosY - boxPosY)) * 2 < (plrSizeY + boxSizeY) = True
  |  otherwise                                               = False

collidesWithAny :: Float -> (Size, Position, Velocity) -> [(Size, Position)] -> Bool
collidesWithAny dT (s, (Position p), (Velocity v)) = foldl (\acc target -> if acc then True else (s, Position (p + dT *^ v)) `collidesWith` target) False

handleCollisions :: Float -> System' ()
handleCollisions dT = do
  solids <- collect $ \(s, p, Solid) -> Just (s, p) :: Maybe (Size, Position)
  cmap $ \(stuffs, v, Not @Solid) ->
    if collidesWithAny dT stuffs solids
       then Velocity 0
       else v :: Velocity

step :: Float -> System' ()
step dT = do
  stepPosition dT
  stepJumpCooldown dT
  stepGravity dT
  doMove
  doJump
  handleCollisions dT

handleEvent :: Event -> System' ()

handleEvent (EventKey (Char 'a') Down _ _) =
  cmap $ \Player -> ActLeft Pressed
handleEvent (EventKey (Char 'a') Up _ _) =
  cmap $ \Player -> ActLeft Released

handleEvent (EventKey (Char 'd') Down _ _) =
  cmap $ \Player -> ActRight Pressed
handleEvent (EventKey (Char 'd') Up _ _) =
  cmap $ \Player -> ActRight Released

handleEvent (EventKey (Char 'w') Down _ _) =
  cmap $ \(Player, Not @JumpCooldown) -> (Jump, JumpCooldown 1)

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess

handleEvent _ = return ()

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

scale' :: Size -> Picture -> Picture
scale' (Size (V2 x y)) = scale x y

triangle :: Picture
triangle = Line [(0,0),(-0.5,-1),(0.5,-1),(0,0)]
rectangle = Line $ rectanglePath 1 1

draw :: System' Picture
draw = do
  player <- foldDraw $ \(Player, p, s) -> translate' p . color white . scale' s $ rectangle
  blocks <- foldDraw $ \(Block, p, s) -> translate' p . color yellow . scale' s $ rectangle

  return $ player <> blocks

runGame = do
  w <- initWorld
  runWith w $ do
    initialize
    play (InWindow "24colors" (240, 480) (10, 10)) black 60 draw handleEvent step
