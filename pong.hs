-- import Prelude
import Data.Text hiding (any)
import SDL
import Linear (V4(..))
import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow (Data.Text.pack "My SDL Application") defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  print renderer
  appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  rendererDrawColor renderer $= V4 255 0 0 255
  fillRect renderer $ Just (Rectangle (P $ V2 10 20) (V2 30 10))
  present renderer
  unless qPressed (appLoop renderer)
