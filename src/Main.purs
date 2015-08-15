module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random

import Data.Int (toNumber)
import Data.Maybe
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.List (List(..), (..), singleton, toList)

import Signal
import Signal.DOM
import Signal.Time

import Graphics.Canvas

data Direction = Up | Down
  
instance eqDirection :: Eq Direction where
  eq Up Up = true
  eq Down Down = true
  eq _ _ = false
  
type Mine = 
  { x :: Number
  , y :: Number
  , r :: Number
  }

type Inputs = 
  { direction :: Direction
  }

type Point = 
  { x :: Number
  , y :: Number
  }
  
type GameState = 
  { paths :: List (List Point)
  , direction :: Direction
  }

initialState :: GameState
initialState = 
  { paths: singleton (toList [p, p])
  , direction: Down
  }
  where
  p = { x: 0.0
      , y: 0.5
      }

clamp :: Number -> Number
clamp x
  | x < 0.0 = clamp (x + 1.0)
  | x > 1.0 = clamp (x - 1.0)
  | otherwise = x

update :: Inputs -> GameState -> GameState
update inputs state@{ paths: Cons (Cons hd tl) paths }
  | state.direction == inputs.direction 
    = case move hd inputs.direction of
        Right p -> { paths: Cons (Cons p tl) paths, direction: inputs.direction }
        Left (Tuple old new) -> { paths: Cons (toList [new, new]) (Cons (Cons old tl) paths), direction: inputs.direction }
  | otherwise
    = { paths: Cons (Cons hd (Cons hd tl)) paths, direction: inputs.direction }

move :: Point -> Direction -> Either (Tuple Point Point) Point
move pt dir 
  | pt.x < 0.998
    = let dy Up   = 1.0
          dy Down = -1.0
      in Right { x: pt.x + 0.002
               , y: clamp (pt.y + dy dir * 0.002)
               }
  | otherwise
    = Left (Tuple { x: 1.0, y: pt.y } { x: 0.0, y: pt.y })
  
main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  
  stars <- for (1 .. 100) \_ -> { x: _, y: _ } <$> random <*> random
  
  frame <- animationFrame
  space <- keyPressed 32  

  scale { scaleX: 600.0, scaleY: 600.0 } ctx
  setLineWidth 0.001 ctx

  let direction :: Signal Direction
      direction = map (\b -> if b then Up else Down) space
  
      inputs :: Signal Inputs
      inputs = { direction: _ } <$> direction
  
      state :: Signal GameState
      state = foldp update initialState (sampleOn frame inputs)

      render :: GameState -> Eff _ Unit
      render state = void do          
        setFillStyle "black" ctx
        fillRect ctx { x: 0.0, y: 0.0, w: 1.0, h: 1.0 }

        setFillStyle "white" ctx
        for_ stars \star ->
          fillPath ctx $ do
            arc ctx { x: star.x
                    , y: star.y
                    , r: 0.001
                    , start: 0.0
                    , end: Math.pi * 2.0
                    }
            closePath ctx

        setStrokeStyle "lightblue" ctx
        for_ state.paths \path -> 
          case path of
            Cons hd tl -> do
              beginPath ctx
              moveTo ctx hd.x hd.y
              for_ tl \p -> lineTo ctx p.x p.y
              stroke ctx
 
  runSignal (render <$> state) 

