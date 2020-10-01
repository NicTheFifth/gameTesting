module Main where

import System.IO
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import System.Random
import Data.List

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAString String
                | ShowImg

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0

view :: GameState -> Picture -> IO Picture
view gameState = return . viewPure gameState

viewPure :: GameState -> Picture -> Picture
viewPure gstate texture = case infoToShow gstate of
  ShowNothing   -> blank
  ShowImg       -> texture
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowAString s -> color green (text s)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- show nothing
    return $ GameState ShowNothing 0
                 | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- If the user presses a wasd (+ shift), show that
inputKey (EventKey (Char c) _ _ _) gstate
  =
    gstate { infoToShow = showz (elemIndex c "wasdWASD") c }
    where 
      showz (Just _) c      = ShowAChar c
      showz _        'i'      = ShowImg
      showz _        _      = ShowNothing
-- If the user presses space
inputKey (EventKey (SpecialKey sk) _ _ _) gstate
  =
    gstate { infoToShow = showz sk}
    where
      showz KeySpace = ShowAString "Space"
      showz _        = ShowNothing
-- Otherwise keep the same
inputKey _ gstate = gstate 

main :: IO ()
main = do
    image <- loadBMP "images/img.bmp"
    playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              (\x -> view x image)-- View function
              input            -- Event function
              step             -- Step function
    
            