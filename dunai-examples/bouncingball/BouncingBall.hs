{-# LANGUAGE Arrows            #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- An example of an animation that bounces up and down.
module Main where

#ifdef BEARRIVER
import FRP.BearRiver as Yampa
#else
import FRP.Yampa     as Yampa
#endif

import Control.Concurrent
import Foreign.C.Types
import qualified SDL
import qualified SDL.Primitive as SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

main :: IO ()
main = do
   SDL.initialize [SDL.InitVideo]
   window <- SDL.createWindow
             "Bouncing ball"
             SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
   SDL.showWindow window
   renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

   reactimate (return ())
              (\_ -> return (0.005, Just ()))
              (\_ e -> render e renderer)
              sf

sf = bouncingBall (100.0 :: Float) 0.0

bouncingBall p0 v0 =
  switch (proc (_) -> do
            (p,v)  <- fallingBall p0 v0  -< ()
            bounce <- edge               -< (p <= 0 && v < 0)
            returnA -< ((p,v), bounce `tag` (p,v))
         )
         (\(p,v) -> bouncingBall p (-v))

fallingBall p0 v0 = proc () -> do
  v <- (v0 +) ^<< integral          -< (-99.8)
  p <- (p0 +) ^<< integralLinear v0 -< v
  returnA -< (p, v)

-- bouncingBall p0 v0 =
--   switch
--     (fallingBall p0 v0 >>> (arr id &&& whenS (\(p,v) -> p <= 0 && v < 0)))
--     (\(p,v) -> bouncingBall p (-v))
--
-- fallingBall p0 v0 = proc () -> do
--   v <- (v0 +) ^<< integral -< (-9.8)
--   p <- (p0 +) ^<< integral -< v
--   returnA -< (p, v)
--
-- whenS :: (a -> Bool) -> SF a (Yampa.Event a)
-- whenS p = (((arr p >>> edge) &&& arr id) >>> (arr (uncurry tag)))

render :: RealFrac a => (a, b) -> SDL.Renderer -> IO Bool
render (p,_) renderer = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

  -- Set background.
  let white = SDL.V4 maxBound maxBound maxBound maxBound
  SDL.rendererDrawColor renderer SDL.$= white
  SDL.clear renderer

  let red = SDL.V4 maxBound 0 0 maxBound
  SDL.rendererDrawColor renderer SDL.$= red

  SDL.fillCircle renderer (SDL.V2 100 (600 - 100 - round p)) 30 red

  SDL.present renderer

  threadDelay 1000

  return quit

-- * Auxiliary functions

-- | Integrate a linearly changing input.
--
-- Alternative to 'integrate' that is numerically more accurate when
-- integrating linearly changing inputs. Uses the average between the last
-- input and the current input as the value to integrate.
#ifdef BEARRIVER
integralLinear :: (Monad m, Fractional s, VectorSpace a s) => a -> SF m a a
#else
integralLinear :: (Fractional s, VectorSpace a s) => a -> SF a a
#endif
integralLinear initial = average >>> integral
  where
    -- Average of the current and the last values
    average = (arr id &&& iPre initial) >>^ (\(x, y) -> (x ^+^ y) ^/ 2)
