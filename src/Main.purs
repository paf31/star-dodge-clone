module Main where

import Prelude

import Control.Bind (join)
import Control.Monad.Eff
import Control.Monad.Eff.Random

import Data.Int (toNumber)
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Foldable (for_, any)
import Data.Traversable (for)
import Data.List (List(..), (..), (!!),  singleton, toList, take)

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
  
type Star = 
  { x :: Number
  , y :: Number
  , r :: Number
  }
  
data GameState 
  = Playing { path :: List Point
            , direction :: Direction
            , level :: Int
            }
  | Waiting { path :: List Point
            , nextLevel :: Int
            }
  
dist2 :: forall r1 r2. { x :: Number, y :: Number | r1 } -> { x :: Number, y :: Number | r2 } -> Number
dist2 p1 p2 = len2 (p2 `subtract` p1)  
  
len2 :: forall r. { x :: Number, y :: Number | r } -> Number
len2 p = p.x * p.x + p.y * p.y
  
subtract :: forall r1 r2. { x :: Number, y :: Number | r1 } -> { x :: Number, y :: Number | r2 } -> Point
subtract p1 p2 = { x: p1.x - p2.x, y: p1.y - p2.y }
  
main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  
  levels <- for (0 .. 19) \level -> do
              for (Tuple <$> 0 .. 8 <*> 2 .. 6) \(Tuple x y) -> do
                dx <- randomRange (-0.025) 0.025
                dy <- randomRange (-0.025) 0.025
                return { x: 0.15 + toNumber x * 0.7 / 8.0 + dx
                       , y: 0.15 + toNumber y * 0.7 / 8.0 + dy
                       , r: 0.015
                       }
  
  frame <- animationFrame
  space <- keyPressed 32  

  scale { scaleX: 800.0, scaleY: 800.0 } ctx
  setLineWidth 0.001 ctx

  let inputs :: Signal Inputs
      inputs = { space: _ } <$> space
  
      initialState :: Int -> GameState
      initialState level = Waiting { path: Nil, nextLevel: level }
  
      state :: Signal GameState
      state = foldp update (initialState 0) (sampleOn frame inputs)
        where
        update :: Inputs -> GameState -> GameState
        update inputs (Playing state@{ path: Cons hd tl })
          | testCollision (fromJust (levels !! state.level)) state.path = Waiting { path: state.path, nextLevel: state.level }
          | (state.direction == Up) == inputs.space 
            = case move hd inputs.space of
                Just p -> playing (Cons p tl) state.level inputs
                Nothing -> initialState ((state.level + 1) `mod` 20)
          | otherwise
            = playing (Cons hd (Cons hd tl)) state.level inputs
        update inputs w@(Waiting { nextLevel: nextLevel }) = if inputs.space then newGame nextLevel else w

        testCollision :: List Star -> List Point -> Boolean
        testCollision stars (Cons p1 (Cons p2 _))
          | p1.y <= 0.25 = true
          | p1.y >= 0.75 = true
          | p1.x > 0.95 && (p1.y < 0.45 || p1.y > 0.55) = true
          | any (\star -> dist2 p1 star < star.r * star.r) stars = true
          | otherwise = false

        newGame :: Int -> GameState
        newGame level = Playing
          { path: toList [p, p]
          , direction: Down
          , level: level
          }
          where
          p = { x: 0.05
              , y: 0.5
              }

        playing :: List Point -> Int -> Inputs -> GameState
        playing path level inputs = Playing { path: path, level: level, direction: if inputs.space then Up else Down }

        move :: Point -> Boolean -> Maybe Point
        move pt space 
          | pt.x >= 0.948 && pt.y >= 0.45 && pt.y <= 0.55
            = Nothing
          | otherwise
            = let dy = if space then 1.0 else -1.0
              in Just { x: pt.x + 0.002
                      , y: pt.y + dy * 0.002
                      }

      background :: Int -> Eff _ Unit
      background level = void do
        setFillStyle "black" ctx
        fillRect ctx { x: 0.0, y: 0.0, w: 1.0, h: 1.0 }
            
        setStrokeStyle "lightgreen" ctx
        strokeRect ctx { x: 0.05, y: 0.25, w: 0.9, h: 0.5 }
        strokeRect ctx { x: 0.045, y: 0.245, w: 0.91, h: 0.51 }
        
        setFillStyle "lightgreen" ctx
        fillRect ctx { x: 0.045, y: 0.45, w: 0.005, h: 0.1 }
        fillRect ctx { x: 0.95, y: 0.45, w: 0.005, h: 0.1 }
        
        setFillStyle "#222" ctx
        setStrokeStyle "lightgreen" ctx
        for_ (fromJust (levels !! level)) \star -> do
          let path = do arc ctx { x: star.x
                                , y: star.y
                                , r: star.r
                                , start: 0.0
                                , end: Math.pi * 2.0
                                }
                        closePath ctx
          fillPath ctx path
          strokePath ctx path

      renderPath :: List Point -> Eff _ Unit
      renderPath path = do
        setStrokeStyle "lightgreen" ctx
        case path of
          Cons hd tl -> void do
            beginPath ctx
            moveTo ctx hd.x hd.y
            for_ tl \p -> lineTo ctx p.x p.y
            stroke ctx
          _ -> return unit

      render :: GameState -> Eff _ Unit
      render (Waiting w) = void do
        background w.nextLevel
        renderPath w.path
      render (Playing state) = void do          
        background state.level
        renderPath state.path
 
  runSignal (render <$> state) 

