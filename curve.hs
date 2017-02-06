import Prelude hiding (reverse)
import qualified Data.Text as Text
import SDL
import Linear (V4(..))
import Control.Monad (unless)
import Foreign.C.Types

-- based on the starter code in the docs
-- http://hackage.haskell.org/package/sdl2-2.1.3.1/docs/SDL.html
main :: IO ()
main = do
  initializeAll
  window <- createWindow (Text.pack "My SDL Application") defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  drawMain renderer
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
  unless qPressed (appLoop renderer)

drawMain :: Renderer -> IO ()
drawMain renderer = do
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 0 0 255
  drawLine renderer (modelToView (V2 0 0)) (modelToView (V2 1 0))
  drawLine renderer (modelToView (V2 0 0)) (modelToView (V2 0 1))
  rendererDrawColor renderer $= V4 0 255 0 255
  let
    xs :: [BinStr]
    xs = generateStrings 8
    points :: (RealFrac a) => [V2 a]
    points = map (\x -> fmap toReal (forward x)) xs
    pairs = zip points (tail points)
  print xs
  print points
  --mapM (\pt -> drawPoint renderer (modelToView pt)) points
  mapM (\(p1, p2) -> drawLine renderer (modelToView p1) (modelToView p2)) pairs
  present renderer

modelToView :: (RealFrac a) => V2 a -> Point V2 CInt
modelToView model = let
    o00 :: Point V2 CInt
    o00 = P $ V2 (CInt 10) (CInt 510)
    scale = V2 500 (-500)
  in
    o00 + (P $ fmap round (model * scale))

data BinStr = BinStr [Bool] deriving (Show)

forward :: BinStr -> V2 BinStr
forward (BinStr a) = let
    (xs, ys) = deinterlace a
  in
    V2 (BinStr xs) (BinStr ys)
reverse :: V2 BinStr -> BinStr
reverse (V2 (BinStr xs) (BinStr ys)) = BinStr $ interlace (xs, ys)

deinterlace :: [a] -> ([a], [a])
deinterlace [] = ([], [])
deinterlace (x:y:rest) = let
    (xs, ys) = deinterlace rest
  in
    (x:xs, y:ys)

interlace :: ([a], [a]) -> [a]
interlace ([], []) = []
interlace (x:xs, y:ys) = x:y:(interlace (xs, ys))

toReal :: (RealFrac a) => BinStr -> a
toReal (BinStr []) = 0
toReal (BinStr (True:xs)) = 0.5 + 0.5 * (toReal $ BinStr xs)
toReal (BinStr (False:xs)) = 0.0 + 0.5 * (toReal $ BinStr xs)

prepend :: Bool -> BinStr -> BinStr
prepend b (BinStr a) = BinStr (b:a)

generateStrings :: Integer -> [BinStr]
generateStrings 0 = [BinStr []]
generateStrings n | n > 0 = let
    prev :: [BinStr]
    prev = generateStrings (n-1)
  in
    (map (prepend False) prev) ++ (map (prepend True) prev)
