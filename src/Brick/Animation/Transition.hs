{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Brick.Animation.Transition
  ( Transition
  , startTransition
  , stopTransition
  , renderTransition

  , slideUp
  )
where

import Graphics.Vty (imageHeight)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (Lens', _Just)
import Lens.Micro.Mtl ((.=), use)

import Brick.Widgets.Core (cropBottomBy, translateBy)
import Brick.Animation
import Brick.Types

data Transition s n =
    Transition { _transitionAnimation :: Maybe (Animation s n)
               , _transitionWidget :: s -> Widget n
               }

makeLenses ''Transition

startTransition :: AnimationManager s e n
                -> (s -> Widget n)
                -> Integer
                -> Integer
                -> (Integer -> Integer -> Widget n -> Widget n)
                -> Lens' s (Maybe (Transition s n))
                -> EventM n s ()
startTransition mgr renderW frameCount frameMs renderFrame updater = do
    let t = Transition { _transitionAnimation = Nothing
                       , _transitionWidget = renderW
                       }
        clip = mkTransitionClip renderW renderFrame frameCount
    updater .= Just t
    startAnimation mgr clip frameMs Once (updater._Just.transitionAnimation)

stopTransition :: AnimationManager s e n
               -> Lens' s (Maybe (Transition s n))
               -> EventM n s ()
stopTransition mgr target = do
    mT <- use target
    case mT >>= _transitionAnimation of
        Nothing -> return ()
        Just a -> do
            stopAnimation mgr a
            target .= Nothing

mkTransitionClip :: (s -> Widget n)
                 -> (Integer -> Integer -> Widget n -> Widget n)
                 -> Integer
                 -> Clip s n
mkTransitionClip renderW renderFrame frameCount =
    newClip frames
    where
        frames = [ renderFrame frameCount i . renderW | i <- [0..frameCount-1] ]

slideUp :: Integer
        -> Integer
        -> Widget n
        -> Widget n
slideUp frameCount i w =
    -- Sliding up means that this slides into view from the bottom, so
    -- we want to crop the bottom less with each frame.
    Widget (hSize w) (vSize w) $ do
        ctx <- getContext
        result <- render w

        let heightPerFrame :: Float
            heightPerFrame = (fromIntegral $ imageHeight $ image result) / (fromInteger frameCount)

            cropAmt = truncate $
                      fromInteger (frameCount - (i + 1)) * heightPerFrame

            vOffset = availHeight ctx - (imageHeight $ image result) + cropAmt

        render $ translateBy (Location (0, vOffset)) $
                 cropBottomBy cropAmt $
                 Widget Fixed Fixed $ return result

renderTransition :: (s -> Widget n)
                 -- ^ The fallback function to use for drawing if the
                 -- transition has not been started
                 -> s
                 -- ^ The state to provide when rendering the animation's
                 -- current frame
                 -> Maybe (Transition s n)
                 -- ^ The transition state itself
                 -> Widget n
renderTransition fallback s Nothing =
    fallback s
renderTransition _ s (Just t) =
    renderAnimation (_transitionWidget t) s (_transitionAnimation t)
