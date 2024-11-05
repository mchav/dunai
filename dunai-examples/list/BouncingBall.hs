{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- An example of using a list monad in a game to represent that new game
-- objects can be created and destroyed.
module Main where

import           Control.Concurrent
import           Data.MonadicStreamFunction hiding (reactimate, trace)
import           Data.MonadicStreamFunction.InternalCore (MSF(..))
import           Foreign.C.Types
import           FRP.Yampa                             as Yampa
import qualified SDL
import qualified SDL.Primitive              as SDL
import           ListT                      as L

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

main :: IO ()
main = do
   SDL.initialize [SDL.InitVideo]
   window <- SDL.createWindow
             "Bouncing balls"
             SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
   SDL.showWindow window
   renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
   reactimate getMouse
              (\_ -> getMouse >>= (\p -> return (0.005, Just p)))
              (\_ e -> render e renderer)
              bouncingBalls

bouncingBalls = proc (mp@(mx, my, ml, mr)) -> do
  -- Just to be sure the game is running
  b <- bouncingBall (100.0 :: Float) (0.0) -< ()

  -- More balls, started on clicks
  ml' <- isEvent ^<< edge -< ml
  bs  <- fireballs        -< (ml', (my, 0))
  returnA -< (b : bs)

fireballs = switch
  (    arr (const [])
   &&& arr (\(mp, pos) -> if mp then Event pos else Yampa.NoEvent)
   -- use notYet to delay the switching event to the next point in
   -- time, so that the switching (and hence the recursive call to
   -- fly) occurs when the "old" tipping event is gone.
   >>> second notYet
  ) 

  (\(p, v) -> let oldfb = voidI $ runListMSF (liftTransS (bouncingBall p v))
                  newfb = fireballs
              in (oldfb &&& newfb) >>> arr2 (++)
  )

bouncingBall p0 v0 =
  switch (proc (_) -> do
            (p,v)  <- fallingBall p0 v0  -< ()
            bounce <- edge               -< (p <= 0 && v < 0)
            returnA -< ((p,v), bounce `tag` (p, v))
         )
         (\(p,v) -> bouncingBall 0 (flippedVel p0 v0 (-99.8)))

-- Calculates the flipped velocity in terms of total energy instead of flipping
-- the current velocity. This produces collisions without energy loss. I do not
-- know how to generalise this for all collisions.
flippedVel p0 v0 acc = sqrt (2 * (ike + ipe))
  where ike = abs (acc * p0)
        ipe = (v0**2)/2

fallingBall p0 v0 = proc () -> do
  v <- (v0 +) ^<< integral -< (-99.8)
  p <- (p0 +) ^<< integral -< v
  returnA -< (p, v)

-- Input
getMouse :: IO (Float, Float, Bool, Bool)
getMouse = do
  SDL.P (SDL.V2 x y) <- SDL.getAbsoluteMouseLocation
  isMouseDown <- SDL.getMouseButtons

  let leftButton  = isMouseDown SDL.ButtonLeft
      rightButton = isMouseDown SDL.ButtonRight
  return (fromIntegral x, fromIntegral y, leftButton, rightButton)

-- Output
render ps renderer = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

  -- Set background.
  let white = SDL.V4 maxBound maxBound maxBound maxBound
  SDL.rendererDrawColor renderer SDL.$= white
  SDL.clear renderer

  let red = SDL.V4 maxBound 0 0 maxBound
  SDL.rendererDrawColor renderer SDL.$= red

  mapM_ (\((p,_),xi) -> SDL.fillCircle
                          renderer
                          (SDL.V2 (100 + xi * 100) (600 - 100 - round p))
                          30
                          red)
     (zip ps [1..])

  SDL.present renderer

  threadDelay 1000

  return quit

-- Auxiliary MSF functions
applyMSF :: Monad m => (a -> MSF m b c) -> MSF m (a, b) c
applyMSF f = MSF $ \(a,b) -> do
  (c, msf') <- unMSF (f a) b
  return (c, arr snd >>> msf')

runListMSF :: (Functor m, Monad m) => MSF (ListT m) a b -> MSF m a [b]
runListMSF msf = runListMSF' [msf]
  where
    runListMSF' msfs = MSF $ \a -> do
        (bs, msfs') <- unzip . concat <$> mapM (toList . (`unMSF` a)) msfs
        return (bs, runListMSF' msfs')

-- Auxiliary Arrow functions
voidI :: Arrow a => a () c -> a b c
voidI =  (>>>) (arr (const ()))
