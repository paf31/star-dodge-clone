module Main where

import Prelude

import Control.Bind (join)
import Control.Monad.Eff
import Control.Monad.Eff.Random

import Data.Int (toNumber)
import Data.Maybe
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Foldable (for_, any)
import Data.Traversable (for)
import Data.List (List(..), (..), singleton, toList, take)

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
  { space :: Boolean
  }

type Point = 
  { x :: Number
  , y :: Number
  }
  
data GameState 
  = Playing { paths :: List (List Point)
            , direction :: Direction
            }
  | GameOver (List (List Point))

newGame :: GameState
newGame = Playing
  { paths: singleton (toList [p, p])
  , direction: Down
  }
  where
  p = { x: 0.05
      , y: 0.5
      }
  
dist2 :: Point -> Point -> Number
dist2 p1 p2 = len2 (p2 `subtract` p1)  
  
len2 :: Point -> Number
len2 p = p.x * p.x + p.y * p.y
  
subtract :: Point -> Point -> Point
subtract p1 p2 = { x: p1.x - p2.x, y: p1.y - p2.y }
  
main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  
  stars <- join <$> for (0 .. 8) \x -> 
                      for (2 .. 6) \y -> do
                        dx <- randomRange (-0.025) 0.025
                        dy <- randomRange (-0.025) 0.025
                        return { x: 0.15 + toNumber x * 0.7 / 8.0 + dx
                               , y: 0.15 + toNumber y * 0.7 / 8.0 + dy
                               }
  
  frame <- animationFrame
  space <- keyPressed 32  

  scale { scaleX: 800.0, scaleY: 800.0 } ctx
  setLineWidth 0.001 ctx

  let inputs :: Signal Inputs
      inputs = { space: _ } <$> space
  
      state :: Signal GameState
      state = foldp update (GameOver Nil) (sampleOn frame inputs)
        where
        update :: Inputs -> GameState -> GameState
        update inputs (Playing state@{ paths: Cons (Cons hd tl) paths })
          | testCollision state.paths = GameOver state.paths
          | (state.direction == Up) == inputs.space 
            = case move hd inputs.space of
                Right p -> playing (Cons (Cons p tl) paths) inputs
                Left (Tuple old new) -> playing (take 3 (Cons (toList [new, new]) (Cons (Cons old tl) paths))) inputs
          | otherwise
            = playing (Cons (Cons hd (Cons hd tl)) paths) inputs
        update inputs (GameOver paths) = if inputs.space then newGame else GameOver paths

      testCollision :: List (List Point) -> Boolean
      testCollision (Cons (Cons p1 (Cons p2 _)) pss) 
        | p1.y <= 0.25 = true
        | p1.y >= 0.75 = true
        | any ((< 0.000225) <<< dist2 p1) stars = true
        | any (testLineSegments p1 p2) pss = true
        | otherwise = false

      testLineSegments :: Point -> Point -> List Point -> Boolean
      testLineSegments q1 q2 = go
        where 
        go (Cons p1 ps@(Cons p2 _))
          | linesIntersect p1 p2 q1 q2 = true
          | otherwise = go ps
        go _ = false

      linesIntersect :: Point -> Point -> Point -> Point -> Boolean
      linesIntersect p1 p2 q1 q2 = 
        let d1  = (q1.x - p1.x) * (p2.x - p1.x) + (q1.y - p1.y) * (p2.y - p1.y)
            d2  = (p1.x - q1.x) * (q2.x - q1.x) + (p1.y - q1.y) * (q2.y - q1.y)
            lp2 = (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)
            lq2 = (q1.x - q2.x) * (q1.x - q2.x) + (q1.y - q2.y) * (q1.y - q2.y)
        in d1 > 0.0 && d1 < lp2 && d2 > 0.0 && d2 < lq2

      playing :: List (List Point) -> Inputs -> GameState
      playing paths inputs = Playing { paths: paths, direction: if inputs.space then Up else Down }

      move :: Point -> Boolean -> Either (Tuple Point Point) Point
      move pt space 
        | pt.x < 0.948
          = let dy = if space then 1.0 else -1.0
            in Right { x: pt.x + 0.002
                     , y: pt.y + dy * 0.002
                     }
        | otherwise
          = Left (Tuple { x: 0.95, y: pt.y } { x: 0.05, y: pt.y })

      background :: Eff _ Unit
      background = void do
        setFillStyle "black" ctx
        fillRect ctx { x: 0.0, y: 0.0, w: 1.0, h: 1.0 }
            
        setStrokeStyle "lightgreen" ctx
        strokeRect ctx { x: 0.05, y: 0.25, w: 0.9, h: 0.5 }
        strokeRect ctx { x: 0.045, y: 0.245, w: 0.91, h: 0.51 }
        
        setFillStyle "#222" ctx
        setStrokeStyle "lightgreen" ctx
        for_ stars \star -> do
          let path = do arc ctx { x: star.x
                                , y: star.y
                                , r: 0.015
                                , start: 0.0
                                , end: Math.pi * 2.0
                                }
                        closePath ctx
          fillPath ctx path
          strokePath ctx path

      renderPaths :: List (List Point) -> Eff _ Unit
      renderPaths paths = do
        setStrokeStyle "lightgreen" ctx
        for_ paths \path -> 
          case path of
            Cons hd tl -> do
              beginPath ctx
              moveTo ctx hd.x hd.y
              for_ tl \p -> lineTo ctx p.x p.y
              stroke ctx

      render :: GameState -> Eff _ Unit
      render (GameOver paths) = void do
        background
        
        renderPaths paths
      render (Playing state) = void do          
        background

        renderPaths state.paths
 
  runSignal (render <$> state) 

