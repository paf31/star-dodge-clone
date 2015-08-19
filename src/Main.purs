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
import Data.Monoid (mempty) 
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Foldable (for_, any, fold, foldMap)
import Data.Traversable (for)
import Data.List (List(..), (..), toList, fromList, null)
import qualified Data.List.Lazy as Lazy
import Data.Date (nowEpochMilliseconds)
import Data.Time (Milliseconds(..))

import DOM
import DOM.RequestAnimationFrame (requestAnimationFrame)

import qualified Graphics.Canvas as C
import qualified Graphics.Drawing as D

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
  { level :: Int
  , stars :: List Star
  , door  :: Number
  , speed :: Number
  , entry :: Number
  , exit  :: Number
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

foreign import onSpaceBarOnce :: forall eff. Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit
  
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
  let door  = decay 100.0    20.0   level
      speed = decay 0.09     0.12   level
      r     = decay 3.0      15.0   level
      d     = decay 15.0     25.0   level
  stars <- for (Tuple <$> 0 .. 8 <*> 0 .. 4) \(Tuple x y) -> do
    dx <- randomRange (-d) d
    dy <- randomRange (-d) d
    return { x: 100.0 + toNumber x * 75.0 + dx
           , y: 100.0 + toNumber y * 70.0 + dy
           , r: r
           }
  entry <- randomRange (50.0 + door / 2.0) (450.0 - door / 2.0)
  exit  <- randomRange (50.0 + door / 2.0) (450.0 - door / 2.0)
  return { level: level + 1, stars: stars, door: door, speed: speed, entry: entry, exit: exit }
  where
  unsafePure :: forall eff a. Eff eff a -> a
  unsafePure = runPure <<< Control.Monad.Eff.Unsafe.unsafeInterleaveEff
  
levels :: Lazy.List Level
levels = unsafeLevel <$> Lazy.iterate (1 +) 0
  
main = do
  Just canvas <- C.getCanvasElementById "canvas"
  ctx <- C.getContext2D canvas
  
  inputsRef <- newRef { space: false }
  
  onSpaceBar (\b -> modifyRef inputsRef (_ { space = b }))

  let intro = "Press Space to Start"
  C.setFont "20px VT323" ctx
  introWidth <- C.measureText ctx intro

  let initialState :: Lazy.List Level -> GameState
      initialState levels = Waiting { path: Nil, nextLevel: levels }
  
      update :: Inputs -> Milliseconds -> GameState -> Either (Tuple GameState GameState) GameState
      update inputs elapsed (Playing state@{ path: Cons hd tl })
        | testCollision (fromJust (Lazy.head state.level)) state.path 
          = Left (Tuple (Waiting { path: state.path, nextLevel: state.level }) (newGame state.level))
        | (state.direction == Up) == inputs.space 
          = case move (fromJust (Lazy.head state.level)) hd inputs.space elapsed of
              Just p -> Right $ playing (Cons p tl) state.level inputs
              Nothing -> let next = fromJust (Lazy.tail state.level) 
                         in Left (Tuple (initialState next) (newGame next))
        | otherwise
          = Right $ playing (Cons hd (Cons hd tl)) state.level inputs

      testCollision :: Level -> List Point -> Boolean
      testCollision level (Cons p1 (Cons p2 _))
        | p1.y <= 50.0 = true
        | p1.y >= 450.0 = true
        | p1.x > 750.0 && (p1.y < level.exit - level.door / 2.0 || p1.y > level.exit + level.door / 2.0) = true
        | any (\star -> dist2 p1 star < star.r * star.r) level.stars = true
        | otherwise = false
          
      newGame :: Lazy.List Level -> GameState
      newGame levels = Playing
        { path: toList [p, p]
        , direction: Down
        , level: levels
        }
        where
        p = { x: 50.0
            , y: (fromJust (Lazy.head levels)).entry
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
        guard (new.x < 750.0 || new.y < level.exit - level.door / 2.0 || new.y > level.exit + level.door / 2.0)
        return new

      clearCanvas :: Eff _ Unit
      clearCanvas = void do
        C.setFillStyle "#1B1C1B" ctx
        C.fillRect ctx { x: 0.0, y: 0.0, w: 800.0, h: 500.0 }

      lightGreen :: D.Color
      lightGreen = D.lighten 0.7 D.green

      vt323 :: D.Font
      vt323 = D.font (D.customFont "VT323") 20 mempty

      background :: Lazy.List Level -> D.Drawing
      background levels =
        let level = fromJust (Lazy.head levels)
        in fold $ 
             [ D.outlined (D.outlineColor lightGreen) 
                          (D.rectangle 50.0 50.0 700.0 400.0 <> 
                           D.rectangle 45.0 45.0 710.0 410.0)
             , D.filled (D.fillColor lightGreen)
                        (D.rectangle 45.0  (level.entry - level.door / 2.0) 5.0 level.door <>
                         D.rectangle 750.0 (level.exit  - level.door / 2.0) 5.0 level.door)
             , D.shadow (D.shadowColor D.white <> D.shadowBlur 4.0) $
                 foldMap (\star -> D.filled (D.fillColor lightGreen) 
                                              (D.circle star.x star.y star.r))
                         level.stars
             , D.text vt323 50.0 40.0 (D.fillColor lightGreen) ("Level: " <> show level.level)
             ]
        
      renderPath :: List Point -> D.Drawing
      renderPath path | not null path =
        D.outlined (D.outlineColor lightGreen) 
                   (D.path path)
      renderPath _ = mempty

      scene :: GameState -> D.Drawing
      scene (Waiting w) = fold 
        [ background w.nextLevel
        , renderPath w.path
        , D.shadow (D.shadowColor D.black <> D.shadowBlur 8.0) $
            D.text vt323 (400.0 - introWidth.width / 2.0) 275.0 (D.fillColor lightGreen) intro
        ]
      scene (Playing state) = fold   
        [ background state.level
        , renderPath state.path
        ]
 
      loop :: Lazy.List Level -> Eff _ Unit
      loop levels = do
        clearCanvas
        D.render ctx $ scene (initialState levels)
        onSpaceBarOnce (go (newGame levels))
        where
        go state = do
          t0 <- nowEpochMilliseconds
          inputs <- readRef inputsRef
          requestAnimationFrame do
            t1 <- nowEpochMilliseconds
            case update inputs (t1 - t0) state of
              Right newState -> do
                clearCanvas
                D.render ctx $ scene newState 
                go newState
              Left (Tuple now next) -> do
                clearCanvas
                D.render ctx $ scene now 
                onSpaceBarOnce (go next)
 
  loop levels

