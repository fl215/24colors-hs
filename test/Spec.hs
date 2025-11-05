import Test.Tasty
import Test.Tasty.HUnit

import Apecs
import Linear

import Components
import Game

main = defaultMain tests

tests :: TestTree
tests = testGroup "collision detection"
  [  testGroup "collidesWith"
    [
      testCase "while player is outside the block" $ do
        let block = (Size (V2 32 32), Position (V2 (-4) (-12)))
        let player = (Size (V2 32 32), Position (V2 0 64))

        block `collidesWith` player @?= False
        player `collidesWith` block @?= False
    , testCase "while player is inside the block" $ do
        let block = (Size (V2 32 32), Position (V2 0 0))
        let player = (Size (V2 32 32), Position (V2 0 0))

        block `collidesWith` player @?= True
        player `collidesWith` block @?= True
    ]
  ,  testGroup "collidesWithAny"
    [
      testCase "while the player is inside one block and outside the other" $ do
        let blocks = [(Size (V2 32 32), Position (V2 0 0)), (Size (V2 32 32), Position (V2 (-4) (-20)))]
        let player = (Size (V2 32 32), Position (V2 0 20))

        player `collidesWithAny` blocks @?= True
    , testCase "while the player is outside both blocks" $ do
        let blocks = [(Size (V2 32 32), Position (V2 0 0)), (Size (V2 32 32), Position (V2 (-4) (-20)))]
        let player = (Size (V2 32 32), Position (V2 0 60))

        player `collidesWithAny` blocks @?= False
    ]
  ]
  
