module Main where

import Prelude

import Control.Bind (join)
import Control.MonadPlus (guard)

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Ref

import Data.Int (toNumber)
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Foldable (for_, any)
import Data.Traversable (for)
import Data.List (List(..), (..), toList)
import qualified Data.List.Lazy as Lazy
import Data.Date (nowEpochMilliseconds)
import Data.Time (Milliseconds(..))

import DOM
import DOM.RequestAnimationFrame (requestAnimationFrame)

import Graphics.Canvas

data Direction = Up | Down
  
instance eqDirection :: Eq Direction where
  eq Up Up = true
  eq Down Down = true
  eq _ _ = false

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
  
type Level = 
  { stars :: List Star
  , door  :: Number
  , speed :: Number
  }
  
data GameState 
  = Playing { path :: List Point
            , direction :: Direction
            , level :: Lazy.List Level
            }
  | Waiting { path :: List Point
            , nextLevel :: Lazy.List Level
            }

foreign import onSpaceBar :: forall eff. (Boolean -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
  
dist2 :: forall r1 r2. { x :: Number, y :: Number | r1 } -> { x :: Number, y :: Number | r2 } -> Number
dist2 p1 p2 = len2 (p2 `subtract` p1)  
  
len2 :: forall r. { x :: Number, y :: Number | r } -> Number
len2 p = p.x * p.x + p.y * p.y
  
subtract :: forall r1 r2. { x :: Number, y :: Number | r1 } -> { x :: Number, y :: Number | r2 } -> Point
subtract p1 p2 = { x: p1.x - p2.x, y: p1.y - p2.y }
  
decay :: Number -> Number -> Int -> Number
decay at0 at7 n = (1.0 - Math.exp (- (toNumber n) / 2.5)) * (at7 - at0) + at0
  
clamp :: forall a. (Ord a) => a -> a -> a
clamp a max | a > max = max
            | otherwise = a
  
unsafeLevel :: Int -> Level
unsafeLevel level = unsafePure do
  let door  = decay 0.14    0.04    level
      speed = decay 0.00009 0.00012 level
      r     = decay 0.005   0.02    level
  stars <- for (Tuple <$> 0 .. 8 <*> 2 .. 6) \(Tuple x y) -> do
    dx <- randomRange (-0.025) 0.025
    dy <- randomRange (-0.025) 0.025
    return { x: 0.15 + toNumber x * 0.7 / 8.0 + dx
           , y: 0.15 + toNumber y * 0.7 / 8.0 + dy
           , r: r
           }
  return { stars: stars, door: door, speed: speed }
  where
  unsafePure :: forall eff a. Eff eff a -> a
  unsafePure = runPure <<< Control.Monad.Eff.Unsafe.unsafeInterleaveEff
  
levels :: Lazy.List Level
levels = unsafeLevel <$> Lazy.iterate (1 +) 0
  
main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  
  inputsRef <- newRef { space: false }
  
  onSpaceBar (\b -> modifyRef inputsRef (_ { space = b }))

  translate { translateX: 0.0, translateY: -170.0 } ctx
  scale { scaleX: 800.0, scaleY: 800.0 } ctx
  setLineWidth 0.001 ctx

  let initialState :: Lazy.List Level -> GameState
      initialState levels = Waiting { path: Nil, nextLevel: levels }
  
      update :: Inputs -> Milliseconds -> GameState -> Maybe GameState
      update inputs elapsed (Playing state@{ path: Cons hd tl })
        | testCollision (fromJust (Lazy.head state.level)) state.path 
          = Just $ Waiting { path: state.path, nextLevel: state.level }
        | (state.direction == Up) == inputs.space 
          = case move (fromJust (Lazy.head state.level)) hd inputs.space elapsed of
              Just p -> Just $ playing (Cons p tl) state.level inputs
              Nothing -> Just $ initialState (fromJust (Lazy.tail state.level))
        | otherwise
          = Just $ playing (Cons hd (Cons hd tl)) state.level inputs
      update inputs _ w@(Waiting { nextLevel: nextLevel }) = if inputs.space then Just (newGame nextLevel) else Nothing

      testCollision :: Level -> List Point -> Boolean
      testCollision level (Cons p1 (Cons p2 _))
        | p1.y <= 0.25 = true
        | p1.y >= 0.75 = true
        | p1.x > 0.95 && (p1.y < 0.5 - level.door / 2.0 || p1.y > 0.5 + level.door / 2.0) = true
        | any (\star -> dist2 p1 star < star.r * star.r) level.stars = true
        | otherwise = false
          
      newGame :: Lazy.List Level -> GameState
      newGame levels = Playing
        { path: toList [p, p]
        , direction: Down
        , level: levels
        }
        where
        p = { x: 0.05
            , y: 0.5
            }
          
      playing :: List Point -> Lazy.List Level -> Inputs -> GameState
      playing path levels inputs = Playing { path: path, level: levels, direction: if inputs.space then Up else Down }
          
      move :: Level -> Point -> Boolean -> Milliseconds -> Maybe Point
      move level pt space (Milliseconds elapsed) = do
        let dt  = clamp elapsed 50.0 -- Cap the equivalent of at 20fps
            dy  = if space then 1.0 else -1.0
            new = { x: pt.x + level.speed * dt
                  , y: pt.y + dy * level.speed * dt
                  }
        guard (new.x < 0.95 || new.y < 0.5 - level.door / 2.0 || new.y > 0.5 + level.door / 2.0)
        return new

      background :: Lazy.List Level -> Eff _ Unit
      background levels = void do
        let level = fromJust (Lazy.head levels)
          
        setFillStyle "black" ctx
        fillRect ctx { x: 0.0, y: 0.0, w: 1.0, h: 1.0 }
            
        setStrokeStyle "lightgreen" ctx
        strokeRect ctx { x: 0.05, y: 0.25, w: 0.9, h: 0.5 }
        strokeRect ctx { x: 0.045, y: 0.245, w: 0.91, h: 0.51 }
        
        setFillStyle "lightgreen" ctx
        fillRect ctx { x: 0.045, y: 0.5 - level.door / 2.0, w: 0.005, h: level.door }
        fillRect ctx { x: 0.95, y: 0.5 - level.door / 2.0, w: 0.005, h: level.door }
        
        setFillStyle "#222" ctx
        setStrokeStyle "lightgreen" ctx
        for_ level.stars \star -> do
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
 
      loop :: GameState -> Eff _ Unit
      loop state = do
        render state
        go state
        where
        go state = do
          t0 <- nowEpochMilliseconds
          inputs <- readRef inputsRef
          requestAnimationFrame do
            t1 <- nowEpochMilliseconds
            case update inputs (t1 - t0) state of
              Just newState -> do
                render newState 
                go newState
              Nothing -> go state 
 
  loop (initialState levels)

