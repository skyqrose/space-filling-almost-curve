-- import Prelude
import Data.Text hiding (any)
import SDL
import Linear (V4(..))
import Control.Monad (unless)
import Foreign.C.Types

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
  drawMain renderer
  unless qPressed (appLoop renderer)

drawMain :: Renderer -> IO ()
drawMain renderer = do
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  rendererDrawColor renderer $= V4 255 0 0 255
  fillRect renderer $ Just (Rectangle (P $ V2 10 20) (V2 30 10))
  present renderer

modelToView :: (RealFrac a) => V2 a -> Point V2 CInt
modelToView model = let
    o00 :: Point V2 CInt
    o00 = P $ V2 (CInt 10) (CInt 110)
    scale = V2 100 100
  in
    o00 + (P $ fmap round (model * scale))
