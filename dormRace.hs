{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Data.Text
import CodeWorld.Sketches


data Screen = Start | Game | GameOver
data State = State
             Int         -- ^ Final, Retake 1, Retake 2
             Double      -- ^ y-coord
             [Drawbrick] -- ^ Obstacles list
             Double      -- ^ Time passed
             Double      -- ^ Shield time left
             Screen      -- ^ Active screen mode
data Dir = Up | Down
data Drawbrick = Drawbrick Double Double


heart :: Picture 
heart = scaled 2.5 2.5
       (colored red (lettering (Data.Text.singleton '\x2665')))


logo :: Picture
logo = pictures [
  colored (light (bright green)) (thickCurve 0.2 [(2.02,1.4), (2.02,0.6)]),
  colored (light (bright green)) (thickCurve 0.2 [(1.38,1.28), (1.38,-0.54)]),
  colored (light (bright green)) (thickCurve 0.2 [(0.68,0.72), (0.68, -0.4)]),
  colored (light (bright green)) (thickCurve 0.2 [(0,1), (0,-0.62)]),
  colored (light (bright green)) (thickCurve 0.2 [(-0.7,0.66), (-0.7,-0.62)]),
  colored (light (bright green)) (thickCurve 0.2 [(-1.4,0.82), (-1.4,-0.78)]),
  colored (light (bright green)) (thickPolygon 0.2 [(2, -1),
                                                    (-2, -1),
                                                    (-2.02,-0.5),
                                                    (-2.32,-0.18),
                                                    (-2.08,1.18),
                                                    (-0.48,1.18),
                                                    (2,2),
                                                    (2.88,0.8),
                                                    (2,0)
                                                   ]
                                 )
                ]


gameOver :: Picture
gameOver = pictures [
  translated (-6)  7  logo,
  translated   0   6  (scaled 6 6 (lettering (Data.Text.singleton '\x263A'))),
  translated   0   2  (scaled 2 2 (styledLettering Bold Handwriting "You were")),
  translated   0 (-1) (scaled 3 3 (colored red (styledLettering Bold Handwriting "DROPPED"))),
  scaled 0.9 0.9 (translated 0 (-5) (lettering "Press SPACE to restart the game")),
  colored white (solidRectangle 42 20)
  ]


startGame :: Picture
startGame = pictures [
  (scaled 2 2 (styledLettering Bold Handwriting "DORM RACE")),
  translated 0 (-2) ((styledLettering Bold Handwriting "Try to attend lecture at 9:00")),
  scaled 0.9 0.9 (translated 0 (-6) (lettering "Press SPACE to start the game")),
  translated 0 4 (scaled 2 2 logo),
  colored white (solidRectangle 42 20)
  ]


excursion :: Picture
excursion = scaled 4 4 (pictures [
  translated   0.4  (-0.1) (lettering (Data.Text.singleton '\x1F93E')),
  translated (-0.1) (-0.1) (lettering (Data.Text.singleton '\x1F483')),
  translated   0.3    0.2  (lettering (Data.Text.singleton '\x1F46C')),
  translated (-0.4)   0.15 (lettering (Data.Text.singleton '\x1F9CD'))
  ])


runner :: Picture 
runner = scaled (-3) 3 (lettering (Data.Text.singleton '\x1F3C3'))

detail :: Picture
detail = colored grey (thickClosedCurve 0.06 [(-0.48,0), (0.66,1.26),
                                              (-0.68,0.5), (0.56,-1.16), 
                                              (-0.28,-0.24), (-1.2,-0.8),
                                              (0.26,1.54), (1.7,-1), (0.98,2)])
 
lightBulb :: Picture
lightBulb = pictures [
  translated 0 (1) (pictures [
  detail,
  rotated 1 detail,
  rotated 2 detail,
  rotated 3 detail,
  rotated 4 detail,
  rotated 5 detail]),
  translated (0) 1.8 (colored (bright yellow) (solidCircle 0.2) <>
                      colored (lighter 0.35 yellow) (solidCircle 0.4)),
  translated (1.1) 1.1 (colored(bright yellow) (solidCircle 0.2) <>
                        colored (lighter 0.35 yellow) (solidCircle 0.4)),
  translated (-1) 1 (colored(bright yellow) (solidCircle 0.2) <>
                     colored (lighter 0.35 yellow) (solidCircle 0.4)),
  colored (bright yellow) (solidCircle 0.2),
  colored (lighter 0.35 yellow) (solidCircle 0.4),
  colored (dark grey) (thickPolygon 0.1 [(0,1.8), (0, 5)])
  ]

sign :: Color -> Picture
sign color = colored (dark grey)
                (solidPolygon [(-0.6, 3.2), (-0.6, 1.8), (0.6, 2.5)]) <>
  colored color (solidPolygon [(-0.7, 3.3), (-0.7, 1.7), (0.7, 2.5)])
  
  
door :: Picture
door = 
  colored (light grey) (solidPolygon [(-0.5, 3), (-0.5, 2), (0.5, 2.5)]) <>
  translated 1.4 0 (colored (dark grey) (solidRectangle 0.7 0.3)) <>
  translated 2.5 1.2 (colored (dark grey) (solidRectangle 0.4 0.7)) <>
  colored grey (solidRectangle 4 8)
  
  
dormFloor :: Color -> Picture
dormFloor color = translated 0 (-7) $
  colored (lighter 0.5 brown) (solidPolygon [(-5, 3.5), (-3, 8),
                                             (3, 8), (5, 3.5)]) <>
  colored (color)             (solidPolygon [(0, -3.5), (21, -3.5),
                                             (21, 3.5), (5, 3.5)])<>
  colored (lighter 0.5 brown) (solidRectangle 42 7)
  
  
dormitory :: Color -> Picture
dormitory color = pictures [
  translated (-6) 3 (sign color),
  colored (lighter 0.4 brown) (polygon [(-5, -3.5), (-5, 10)]),
  colored (lighter 0.4 brown) (polygon [(5, -3.5), (5, 10)]),
  colored (lighter 0.4 brown) (polygon [(-3, 1), (-3, 10)]),
  colored (lighter 0.4 brown) (polygon [(3, 1), (3, 10)]),
  colored (grey)              (polygon [(0, 1), (0, 10)]),
  translated   0.6  5 (colored (dark grey) (solidRectangle 0.7 0.3)),
  translated (-0.6) 5 (colored (dark grey) (solidRectangle 0.7 0.3)),
  colored  grey (solidPolygon [(-4.44,-2.4), (-3.76, -0.6), (-3.76, 6.8), (-4.44, 5.6)]),
  colored  grey (solidPolygon [(4.44,-2.4), (3.76, -0.6), (3.76, 6.8), (4.44, 5.6)]),
  colored color (solidPolygon [(5,2), (5, 10), (21, 10), (21, 5)]),
  colored color (solidPolygon [(-5,2), (-5, 10), (-3, 10), (-3, 7)]),
  colored (lighter 0.4 grey) (solidPolygon [(-3, 1), (-3, 10), (3, 10), (3, 1)]),
  translated (-17) 0.4 door,
  translated (-11) 0.4 door,
  dormFloor color,
  colored (lighter 0.55 brown) (solidRectangle 42 20)]
-- Colors : light (bright green)
-- darker 0.1 ( dull( lighter 0.1 purple))
-- mixed [red, brown]

tree :: Picture
tree = translated 0 3.26 $ translated 0 (-2) (solidRectangle 3 0.5)
                        <> scaled 0.5 0.5 sketchedTree
  
  
smallWindow :: Picture
smallWindow = translated 0 5.8 $ colored blue (solidRectangle 0.3 0.5)
                              <> solidRectangle 0.4 0.6
  
  
smallWindows :: Picture
smallWindows = pictures [
                       smallWindow ,
  translated   0.6  0 (smallWindow),
  translated   1.2  0 (smallWindow),
  translated (-0.6) 0 (smallWindow),
  translated (-1.2) 0 (smallWindow)
  ]
  
  
tormasovka :: Picture
tormasovka = pictures [
  smallWindows,
  translated 0 (-1) smallWindows,
  translated 0 (-2) smallWindows,
  translated 0 (-3) smallWindows,
  translated 0 4.32 $ colored (bright yellow) (thickPolyline 0.1 [(-1.57,0),(1.57,0)]),
  translated 0 5.32 $ colored (bright yellow) (thickPolyline 0.1 [(-1.55,0),(1.55,0)]),
  translated 0 6.32 $ colored (bright yellow) (thickPolyline 0.1 [(-1.55,0),(1.55,0)]),
  translated 0 2.32 $ colored (bright yellow) (thickPolyline 0.1 [(-1.55,0),(1.55,0)]),
  translated 0 4.3  $ colored (white)         (thickPolyline 0.2 [(-1.55,0),(1.55,0)]),
  translated 0 5.3  $ colored (white)         (thickPolyline 0.2 [(-1.55,0),(1.55,0)]),
  translated 0 6.3  $ colored (white)         (thickPolyline 0.2 [(-1.55,0),(1.55,0)]),
  translated 0 2.3  $ colored (white)         (thickPolyline 0.2 [(-1.55,0),(1.55,0)]),
  translated 0 4    $ colored (brown)         (solidRectangle 3 5)]
  
  
succi :: Picture
succi = pictures [
  smallWindows,
  translated 0 (-1) smallWindows,
  translated 0 (-2) smallWindows,
  translated 0 (-3) smallWindows,
  translated 0 4.32 $ colored (bright yellow) (thickPolyline 0.1 [(-1.57,0),(1.57,0)]),
  translated 0 5.32 $ colored (bright yellow) (thickPolyline 0.1 [(-1.55,0),(1.55,0)]),
  translated 0 6.32 $ colored (bright yellow) (thickPolyline 0.1 [(-1.55,0),(1.55,0)]),
  translated 0 2.32 $ colored (bright yellow) (thickPolyline 0.1 [(-1.55,0),(1.55,0)]),
  translated 0 4.3  $ colored (brown)         (thickPolyline 0.2 [(-1.55,0),(1.55,0)]),
  translated 0 5.3  $ colored (brown)         (thickPolyline 0.2 [(-1.55,0),(1.55,0)]),
  translated 0 6.3  $ colored (brown)         (thickPolyline 0.2 [(-1.55,0),(1.55,0)]),
  translated 0 2.3  $ colored (brown)         (thickPolyline 0.2 [(-1.55,0),(1.55,0)]),
  translated 0 4    $ colored (light brown)   (solidRectangle 3 5)]
  
  
hallfloor :: Picture
hallfloor = translated 0 (-7) $
  pictures [                colored (dark brown) (polyline [(0,3.5),(0,-3.5)]),
  translated    7   0     $ colored (dark brown) (polyline [(0,3.5),(0,-3.5)]),
  translated   14   0     $ colored (dark brown) (polyline [(0,3.5),(0,-3.5)]),
  translated  (-7)  0     $ colored (dark brown) (polyline [(0,3.5),(0,-3.5)]),
  translated (-14)  0     $ colored (dark brown) (polyline [(0,3.5),(0,-3.5)]),
                            colored (dark brown) (polyline [(-21,0),(21,0)]),
  translated    0   3.5   $ colored (dark brown) (thickPolyline 0.2 [(-21,0),(21,0)]),
                            colored (dark brown) (polyline [(-21,0),(21,0)]),
  translated    0   1.75  $ colored (dark brown) (polyline [(-21,0),(21,0)]),
  translated    0 (-1.75) $ colored (dark brown) (polyline [(-21,0),(21,0)]),
                            colored (dull brown) (solidRectangle 42 7)]
  
  
window :: Picture
window = translated 0 5 $ colored (light blue) (solidRectangle 5 7)
                       <> (solidRectangle 6 8)


warmHall :: Picture
warmHall = pictures 
  [ hallfloor
  , translated   12  0 tree
  , translated (-18) 0 tree
  ,                    tormasovka
  , translated (-12) 0 succi
  ,                    window
  , translated    6  0 window
  , translated   12  0 window
  , translated   18  0 window
  , translated  (-6) 0 window
  , translated (-12) 0 window
  , translated (-18) 0 window
  , translated    0  9 $ colored
      (darker 0.4 (bright (translucent orange))) (solidRectangle 42 3)
  , colored (dark (bright (translucent orange))) (solidRectangle 42 20)
  ]


initialState :: State
initialState = State 3 0 [] 0 0 Start


drawHealth :: Int -> Picture
drawHealth n
  | n <= 0    = blank 
  | otherwise = translated (8 - (fromIntegral n) * 2.2) 7
                           heart <> drawHealth (n - 1)


drawObstacles :: [Drawbrick] -> Picture
drawObstacles                   [] = blank
drawObstacles ((Drawbrick x y):xs) = translated x y excursion
                                  <> drawObstacles xs


pseudorandom :: Double -> Int -> Double
pseudorandom x modulo = fromIntegral
  (floor (190711997864373 * x + 373) `mod` modulo)


genObstacle :: [Drawbrick] -> Double -> [Drawbrick]
genObstacle x time
  | floor (10000 * time) `mod` 80 == 0 =
      (Drawbrick (10 + (pseudorandom (200000 * time) 150))
      (4 - (pseudorandom time 13))) : x
  | otherwise = x
  
  
collision :: (Double, Double) -> [Drawbrick] -> Bool
collision _coords [] = False
collision (playeerX, playeerY) ((Drawbrick x y):xs)
  | abs (playeerX - x) <= 2 && abs (playeerY - y) <= 2 = True
  | otherwise = collision (playeerX, playeerY) xs


updateObstacles :: State -> Double -> State
updateObstacles (State lives pos obs dt shield screen) time = State livesRemaining pos
    (Prelude.filter (\(Drawbrick x _y) -> (x > (-10)))
      (Prelude.map (\(Drawbrick x y) -> (Drawbrick (x - (10 * time)) y))
        (genObstacle obs dt))) (time + dt) shieldRemaining screen
  where
    isCollision = shield <= 0 && collision (-7, pos) obs
    livesRemaining
      | isCollision = lives - 1
      | otherwise   = lives
    shieldRemaining
      | isCollision = 800
      | otherwise   = shield - dt


moveTo :: Dir -> State -> State
moveTo   Up    (State lives  pos         drawbricks dt shield screen)
  | pos < 4   = State lives (pos + 0.75) drawbricks dt shield screen
  | otherwise = State lives  pos         drawbricks dt shield screen
moveTo Down    (State lives  pos         drawbricks dt shield screen)
  | pos > -9  = State lives (pos - 0.75) drawbricks dt shield screen
  | otherwise = State lives  pos         drawbricks dt shield screen


onGame :: State -> State
onGame (State lives pos drawbricks dt shield screen)
  | lives > 0 = State lives pos drawbricks dt shield screen
  | otherwise = State     0   0         []  0      0 GameOver


background :: Double -> Picture
background time = translated (fromIntegral (-shift)) 0
    (warmHall <> (translated 42 0 (dormitory dormColor))
              <> (translated 84 0  warmHall))
  where
    shift = (floor (time * 15) `mod` 95)
    dormColor
      | time <= 5                        = light (bright green)
      | floor (1.1 * time) `mod` 21 <= 6 = light (bright green)
      | floor (1.1 * time) `mod` 21 < 13 = darker 0.1 ( dull (lighter 0.1 purple))
      | otherwise                        = mixed [red, brown]


handleWorld :: Event -> State -> State
handleWorld (KeyPress "Up")   state = onGame (moveTo Up   state)
handleWorld (KeyPress "Down") state = onGame (moveTo Down state)
handleWorld (KeyPress " ")    state = update              state
    where
      update (State _lives _pos _drawbricks _dt _shield GameOver) =
        State 3 0 [] 0 0 Game
      update (State _lives _pos _drawbricks _dt _shield Start) =
        State 3 0 [] 0 0 Game
      update sc = sc
handleWorld (TimePassing dt) state = onGame (updateObstacles state dt)
handleWorld                _ state = onGame state


render :: State -> Picture
render (State lives pos drawbricks dt _shield screen) = renderScreen screen
  where
    renderScreen Game = translated (-7) pos runner <>
      (drawHealth lives) <> drawObstacles drawbricks <>translated (-7) 7
        (colored red (styledLettering Plain Monospace (Data.Text.pack ("08:" ++ (show (floor (53 + dt)))))))
        <> translated (-7) 7 (solidRectangle (fromIntegral (Prelude.length ("08:" ++ (show (floor (53 + dt))))) - 1.5) 1)
          <> background dt
    renderScreen Start = startGame
    renderScreen GameOver = gameOver

main :: IO()
main = activityOf initialState handleWorld render
