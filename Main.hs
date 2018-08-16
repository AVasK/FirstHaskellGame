module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Creating a simple ping-pong game

width, height, offset :: Int
width = 300
height = 300
offset = 100
paddle_offset = 120

data GameState = Game 
    { ballLoc :: (Float, Float)
    , ballVel :: (Float, Float)
    , player1 :: Float
    , player2 :: Float
    , paddleSpeed :: Int
    , endgame :: Bool
    , paused :: Bool
    } deriving Show
    
-- | Desctibes the initial state of the Pong game
initialState :: GameState
initialState = Game
    { ballLoc = (-10, 30)
    , ballVel = (80, 30)
    , player1 = 40
    , player2 = -80
    , paddleSpeed = 10
    , endgame = False
    , paused = False
    }
    
-- | Game State -> Picture
render :: GameState -> Picture
render game = 
    pictures 
    [ uncurry createBall (ballLoc game)
    , borders 
    , createPaddle green paddle_offset $ player1 game
    , createPaddle orange (-paddle_offset) $ player2 game
    ]
    where
        -- The ball:
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
        ballColor = if endgame game then black else dark red
        
        -- New ball:
        createBall :: Float -> Float -> Picture
        createBall x y = pictures
            [ translate x y $ color black $ circleSolid 11
            , translate x y $ color ballColor $ circleSolid 10
            , translate (x+3) (y+3) $ color white $ circleSolid 2
            ]
        
        -- Borders:
        border :: Float -> Picture
        border offset = 
            translate 0 offset $ color borderColor $ rectangleSolid 270 10
        borderColor = greyN 0.5
        borders = pictures [border 150, border (-150)]
        
        -- Paddles:
        createPaddle :: Color -> Float -> Float -> Picture
        createPaddle col x y = pictures 
            [ translate x y $ color paddleColor $ rectangleSolid 10 60,
              translate x y $ color col $ rectangleSolid 5 55
            ]
        
        paddleColor = applyN 2 light blue

-- Ball Position Update:
moveBall :: Float -- number of seconds since last update
         -> GameState -- initial game state
         -> GameState -- resulting game state
         
moveBall seconds game = game { ballLoc = (x', y')}
    where
        (x, y) = ballLoc game
        (vx, vy) = ballVel game
        (x', y') = if paused game then (x, y) else (x + vx * seconds, y + vy * seconds)
        --x' = x + vx * seconds
        --y' = y + vy * seconds
        

fps :: Int
fps = 60

window :: Display
window = InWindow "pong window" (width, height) (offset, offset) -- initializing window

background :: Color
background = white
        
        
applyN :: (Num n, Eq n) => n -> (a->a) -> a -> a
applyN 1 f x = f x
applyN n f x = f (applyN (n-1) f x)
        
update seconds = speedIncrease . outOfPlayzone . paddleBounce . borderBounce . moveBall seconds

speedIncrease :: GameState -> GameState
speedIncrease game = game { ballVel = (vx', vy'), paddleSpeed = paddleSpeed' }
    where
        (vx, vy) = ballVel game
        vx' = vx * 1.001
        vy' = vy * 1.001
        paddleSpeed' = 8 + round (velocity (vx', vy') / 10)
        

type Radius = Float
type Position = (Float, Float)

velocity :: (Float, Float) -> Float
velocity (vx, vy) = sqrt $ (vx^2 + vy^2) :: Float

outOfPlayzone :: GameState -> GameState
outOfPlayzone game = game { ballVel = (vx', vy'), endgame = e' }
    where
        (x, y) = ballLoc game
        ((vx', vy'), e') = if abs x >= fromIntegral width / 2 || abs y >= fromIntegral height / 2
            then 
                ((0, 0), True)
            else
                (ballVel game, False)
        
            
                


paddleBounce :: GameState -> GameState
paddleBounce game = game { ballVel = (vx', vy) }
    where
        radius = 10
        (vx, vy) = ballVel game
        paddle1 = player1 game
        paddle2 = player2 game
        vx' = if paddleCollision (ballLoc game) radius paddle1 paddle2
            then
                -vx
            else
                vx

paddleCollision :: Position -> Radius -> Float -> Float -> Bool
paddleCollision (x, y) radius paddle1 paddle2 = leftCollision || rightCollision
    where
        leftCollision = x - radius <= - (paddle_offset - 5) && x - radius >= - paddle_offset && abs (y - paddle2) <= 40
        rightCollision = x + radius >= (paddle_offset - 5) && x + radius <= paddle_offset && abs (y - paddle1) <= 40

borderBounce :: GameState -> GameState
borderBounce game = game { ballVel = (vx, vy')}
    where
        radius = 10
        (vx, vy) = ballVel game
        vy' = if borderCollision (ballLoc game) radius 
            then
                -vy
            else
                vy

borderCollision :: Position -> Radius -> Bool
borderCollision (_, y) radius = topCollision || bottomCollision
    where topCollision    = y - radius <= - fromIntegral height / 2
          bottomCollision = y + radius >= fromIntegral height / 2
          
          
handler :: Event -> GameState -> GameState
handler (EventKey (Char 'r') _ _ _) game = game { ballLoc = (0, 0), ballVel = (80, 30), endgame = False }

handler (EventKey (Char 'p') _ _ _) game = game { paused = option }
    where
        option = not $ paused game

-- left player input
handler (EventKey (Char 'w') _ _ _) game = if player2 game <= fromIntegral height / 2 - 43 && (not $ paused game)
                                           then
                                               game { player2 = player2 game + fromIntegral (paddleSpeed game)}
                                           else
                                               game
                                              
handler (EventKey (Char 's') _ _ _) game = if player2 game >= -fromIntegral height / 2 + 43 && (not $ paused game)
                                           then
                                               game { player2 = player2 game - fromIntegral (paddleSpeed game) }
                                           else
                                               game 
        
-- right player:
handler (EventKey (Char 'o') _ _ _) game = if player1 game <= fromIntegral height / 2 - 43 && (not $ paused game)
                                           then
                                               game { player1 = player1 game + fromIntegral (paddleSpeed game) }
                                           else
                                               game
                                              
handler (EventKey (Char 'l') _ _ _) game = if player1 game >= -fromIntegral height / 2 + 43 && (not $ paused game)
                                           then
                                               game { player1 = player1 game - fromIntegral (paddleSpeed game) }
                                           else
                                               game 
    
handler _ game = game



main :: IO()
main = play window background fps initialState render handler update
    where
        frame :: Float -> Picture
        frame seconds = render $ moveBall seconds initialState