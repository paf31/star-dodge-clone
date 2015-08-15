module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random

import Data.Maybe
import Data.Functor (($>))
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.List (List(..), mapMaybe, (..))

import Signal
import Signal.DOM
import Signal.Time

import Graphics.Canvas

data Direction = Up | Down

type Point = 
  { x :: Number
  , y :: Number
  }

type MineData = 
  { pos :: Point
  , created :: Time
  , maxRadius :: Number
  }
  
type Mine = 
  { pos :: Point
  , radius :: Number
  }

type Inputs = 
  { direction :: Direction
  }

type GameState = 
  { pos :: Point
  }

initialState :: GameState
initialState = 
  { pos: { x: 0.0
         , y: 0.5
         }
  }
  
newMine :: Number -> Number -> Number -> Number -> MineData
newMine x y m t = 
  { pos: { x: x
         , y: y
         }
  , created: t
  , maxRadius: m
  }

clamp :: Number -> Number
clamp x
  | x < 0.0 = clamp (x + 1.0)
  | x > 1.0 = clamp (x - 1.0)
  | otherwise = x

toMines :: Time -> List MineData -> List Mine
toMines t = mapMaybe toMine
  where
  toMine d | t - d.created < 0.0 = Nothing
           | t - d.created < 2000.0 = Just { pos: d.pos
                                           , radius: (t - d.created) / 2000.0 * d.maxRadius
                                           }
           | t - d.created < 2500.0 = Just { pos: d.pos
                                           , radius: (2500.0 - (t - d.created)) / 500.0 * d.maxRadius
                                           }
           | otherwise = Nothing

update :: Inputs -> GameState -> GameState
update inputs state = state { pos = { x: clamp (state.pos.x + 0.001)
                                    , y: clamp (state.pos.y + dy * 0.001)
                                    } 
                            }
  where
  dy = case inputs.direction of
         Up -> 1.0
         Down -> -1.0
  
notTooClose :: GameState -> MineData -> Maybe MineData
notTooClose st md 
  | dist st.pos md.pos < 0.05 = Nothing
  | otherwise = Just md

dist :: Point -> Point -> Number
dist p1 p2 = dx * dx + dy * dy
  where
  dx = p1.x - p2.x
  dy = p1.y - p2.y
  
main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  
  stars <- for (1 .. 100) \_ -> { x: _, y: _ } <$> random <*> random
  
  scale { scaleX: 600.0, scaleY: 600.0 } ctx

  frame <- animationFrame
  space <- keyPressed 32
  
  mineData <- unwrap $ every 500.0 <#> \t -> 
                do x <- random
                   y <- random
                   r <- map (* 0.15) random
                   return (newMine x y r t)

  let direction :: Signal Direction
      direction = map (\b -> if b then Up else Down) space
  
      filteredMines :: Signal (Maybe MineData)
      filteredMines = notTooClose <$> state <*> mineData
  
      mines :: Signal (List Mine)
      mines = toMines <$> frame <*> foldp (\md mds -> maybe mds (`Cons` mds) md) Nil filteredMines
  
      inputs :: Signal Inputs
      inputs = { direction: _ } <$> direction
  
      state :: Signal GameState
      state = foldp update initialState (sampleOn frame inputs)

      render :: GameState -> List Mine -> Eff _ Unit
      render state mines = void do
        setFillStyle "black" ctx
        fillRect ctx { x: 0.0, y: 0.0, w: 1.0, h: 1.0 }

        setFillStyle "white" ctx
        for_ stars \star ->
          fillPath ctx $
            arc ctx { x: star.x
                    , y: star.y
                    , r: 0.001
                    , start: 0.0
                    , end: Math.pi * 2.0
                    }

        setFillStyle "white" ctx
        fillPath ctx $ do
          moveTo ctx state.pos.x state.pos.y
          lineTo ctx (state.pos.x - 0.01) (state.pos.y + 0.005)
          lineTo ctx (state.pos.x - 0.01) (state.pos.y - 0.005)
          closePath ctx
          
        setFillStyle "goldenrod" ctx
        for_ mines \mine ->
          fillPath ctx $
            arc ctx { x: mine.pos.x
                    , y: mine.pos.y
                    , r: mine.radius
                    , start: 0.0
                    , end: Math.pi * 2.0
                    }    
 
  runSignal (render <$> state <*> mines) 

