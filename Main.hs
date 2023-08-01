module Main (main) where

import Data.Colour.RGBSpace (RGB (..))
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Complex (Complex (..))
import Data.List (iterate')
import Debug.Trace ()
import Graphics.Gloss (
  Color,
  Display (InWindow),
  Picture,
  circle,
  circleSolid,
  color,
  line,
  makeColor,
  play,
  translate,
  white,
 )
import Graphics.Gloss.Interface.IO.Game (
  Event (EventKey, EventMotion),
  Key (MouseButton),
  KeyState (Down),
  MouseButton (LeftButton),
 )

data World = World
  { locked :: Bool
  , cValue :: Complex Float
  , scaleFactor :: Float
  }

initWorld :: World
initWorld = World False (1 :+ 1) 100

handleEvent :: Event -> World -> World
handleEvent (EventMotion (x, y)) world | not $ locked world = setWorldC x y world
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) world = setWorldC x y world{locked = not $ locked world}
handleEvent _ world = world

setWorldC :: Float -> Float -> World -> World
setWorldC x y world = world{cValue = (x / scaleFactor world) :+ (y / scaleFactor world)}

draw :: World -> Picture
draw World{cValue, scaleFactor} = xAxis <> yAxis <> circle1 <> circle2 <> pathPic <> pointsPic
 where
  xAxis = line [(-10 * scaleFactor, 0), (10 * scaleFactor, 0)]
  yAxis = line [(0, -10 * scaleFactor), (0, 10 * scaleFactor)]
  circle1 = circle scaleFactor
  circle2 = circle (2 * scaleFactor)
  pointsPic =
    foldMap
      ( \(n, x :+ y) ->
          translate (x * scaleFactor) (y * scaleFactor) $
            color (floatToColor (n / fromIntegral numPoints)) $
              circleSolid (0.1 * scaleFactor)
      )
      $ zip [0 ..] points
  points = take numPoints $ mandelbrot cValue
  pathPic = line [(x * scaleFactor, y * scaleFactor) | (x :+ y) <- points]

  numPoints = 8

floatToColor :: Float -> Color
floatToColor t = makeColor r g b 1
 where
  (RGB r g b) = hsv (t * 360) 1 1

mandelbrot :: (RealFloat a) => Complex a -> [Complex a]
mandelbrot c = iterate' (\z -> z ^ 2 + c) 0

main :: IO ()
main =
  play
    (InWindow "Mandelbrot" (1000, 800) (500, 100))
    white
    60
    initWorld
    draw
    handleEvent
    (const id)
