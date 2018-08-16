module Main where

import Graphics.Gloss

-- Creating a simple ping-pong game

width, height, offset :: Int
width = 300
height = 300
offset = 100

data PongGame = Game 
    { ballLoc :: (Float, Float)
    , ballVel :: (Float, Float)
    , player1 :: Float
    , player2 :: Float
    } deriving Show
    
-- | Desctibes the initial state of the Pong game
initialState :: PongGame
initialState = Game
    { ballLoc = (-10, 30)
    , ballVel = (1, -3)
    , player1 = 40
    , player2 = -80
    }
    
-- | Game State -> Picture
render :: PongGame -> Picture
render game = 
    pictures 
    [ ball, borders, 
      createPaddle green 120 $ player1 game,
      createPaddle orange (-120) $ player2 game
    ]
    where
        -- The ball:
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 8
        ballColor = dark red
        
        -- Borders:
        border :: Float -> Picture
        border offset = 
            translate 0 offset $ color borderColor $ rectangleSolid 270 10
        borderColor = greyN 0.5
        borders = pictures [border 150, border (-150)]
        
        -- Paddles:
        createPaddle :: Color -> Float -> Float -> Picture
        createPaddle col x y = pictures 
            [ translate x y $ color col $ rectangleSolid 26 86,
              translate x y $ color paddleColor $ rectangleSolid 20 80
            ]
        
        paddleColor = applyN 2 light blue

-- Ball Position Update:
moveBall :: Float -- number of seconds since last update
         -> PongGame -- initial game state
         -> PongGame -- resulting game state
         
moveBall seconds game = game { ballLoc = (x', y')}
    where
        (x, y) = ballLoc game
        (vx, vy) = ballVel game
        
        x' = x + vx * seconds
        y' = y + vy * seconds
        

window :: Display
window = InWindow "pong window" (width, height) (offset, offset) -- initializing window

background :: Color
background = white
        
        
applyN :: (Num n, Eq n) => n -> (a->a) -> a -> a
applyN 1 f x = f x
applyN n f x = f (applyN (n-1) f x)
        

main :: IO()
main = animate window background frame
    where
        frame :: Float -> Picture
        frame seconds = render $ moveBall seconds initialState
        