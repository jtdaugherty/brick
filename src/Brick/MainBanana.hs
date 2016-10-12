{-# LANGUAGE ScopedTypeVariables #-}

module Brick.MainBanana
  ( brickNetwork
  , continue
  , halt
  )
where



import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana

import           Brick.Types                      ( Widget
                                                  , rowL
                                                  , columnL
                                                  , CursorLocation(..)
                                                  )
import           Brick.Types.Internal             ( RenderState(..)
                                                  , Next(..)
                                                  , EventState(..)
                                                  )
import           Brick.Widgets.Internal           ( renderFinal
                                                  )
import           Brick.AttrMap

import qualified Data.Map as M
import qualified Data.Set as S

import           Data.Default
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad
import           Control.Concurrent
import           Control.Exception                (finally)
import           Lens.Micro                       ((^.))
import           Data.IORef

import           Graphics.Vty
                                                  ( Vty
                                                  , Picture(..)
                                                  , Cursor(..)
                                                  , Event(..)
                                                  , update
                                                  , outputIface
                                                  , inputIface
                                                  , displayBounds
                                                  , shutdown
                                                  , mkVty
                                                  )
import           Graphics.Vty.Input               ( _eventChannel
                                                  )
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM



brickNetwork
  :: forall n
   . Ord n
  => (Banana.AddHandler (), Banana.Handler ())
  -> (  Banana.Event (Maybe Event)
     -> Banana.Event ()
     -> Banana.MomentIO
          ( Banana.Event (Next ())
          , Banana.Behavior [Widget n]
          , Banana.Behavior
              ([CursorLocation n] -> Maybe (CursorLocation n))
          , Banana.Behavior AttrMap
          )
     )
  -> Banana.MomentIO ()
brickNetwork (startupAH, startupH) interfaceF = do
  let emptyES   = ES [] []
      initialRS = RS M.empty (esScrollRequests emptyES) S.empty mempty

  (eventEvent   , eventH   )          <- Banana.newEvent
  (shutdownEvent, shutdownH)          <- Banana.newEvent
  startupEvent                        <- Banana.fromAddHandler startupAH
  (nextE, nextH)                      <- Banana.newEvent

  initState                           <-
    (=<<) (Banana.switchB (pure Nothing) . fmap (fmap Just))
      $ Banana.execute
      $ flip fmap startupEvent
      $ \() -> liftIO $ do
          vty           <- liftIO $ do
            x <- mkVty def
            return x
          shutdownIORef <- liftIO $ newIORef False
          let pumpAction = do
                loop `finally` shutdown vty
              loop       = do
                ev <- atomically (readTChan $ _eventChannel $ inputIface vty)
                case ev of
                  (EvResize _ _) ->
                    eventH
                      .   Just
                      .   (\(w, h) -> EvResize w h)
                      =<< (displayBounds $ outputIface vty)
                  _ -> eventH $ Just ev
                sd <- readIORef shutdownIORef
                unless sd loop
          liftIO $ do
            void $ forkIO $ pumpAction
            void $ forkIO $ eventH $ Nothing
          let stopper = writeIORef shutdownIORef True
          return $ pure (vty, stopper)

  (triggerE, widgetB, cursorB, attrB) <- interfaceF eventEvent shutdownEvent

  Banana.reactimate $ nextH <$> triggerE

  rsRef <- liftIO $ newIORef initialRS

  let
    hand
      :: Maybe (Vty, IO ())
      -> [Widget n]
      -> ([CursorLocation n] -> Maybe (CursorLocation n))
      -> AttrMap
      -> Next ()
      -> IO ()
    hand mState widgetStack chooseCursor attrs next = do
      case next of
        Continue         () -> do
          case mState of
            Nothing       -> pure ()
            Just (vty, _) -> do
              renderState  <- readIORef rsRef
              renderState' <- render vty
                                     widgetStack
                                     chooseCursor
                                     attrs
                                     renderState
              writeIORef rsRef renderState'
        SuspendAndResume io -> do
          mState `forM_` snd
          io
          void $ forkIO $ startupH ()
        Halt             () -> do
          mState `forM_` snd
          shutdownH ()

  Banana.reactimate
    $          hand
    <$>        initState
    <*>        widgetB
    <*>        cursorB
    <*>        attrB
    Banana.<@> nextE
  --   $   flip Banana.apply resultEvent
  --   $   initState
  --   <&> \mState (widgetStack, next, chooseCursor, attrMapCur) -> do
  --         case next of
  --           Continue         () -> do
  --             case mState of
  --               Nothing       -> pure ()
  --               Just (vty, _) -> do
  --                 renderState  <- readIORef rsRef
  --                 renderState' <- render vty
  --                                        widgetStack
  --                                        chooseCursor
  --                                        attrMapCur
  --                                        renderState
  --                 writeIORef rsRef renderState'
  --           SuspendAndResume io -> do
  --             mState `forM_` snd
  --             io
  --             void $ forkIO $ startupH ()
  --           Halt             () -> do
  --             mState `forM_` snd
  --             shutdownH ()

render
  :: Vty
  -> [Widget n]
  -> ([CursorLocation n] -> Maybe (CursorLocation n))
  -> AttrMap
  -> RenderState n
  -> IO (RenderState n)
render vty widgetStack chooseCursor attrMapCur rs = do
  sz <- displayBounds $ outputIface vty
  let (newRS, pic, theCursor) =
        renderFinal attrMapCur widgetStack sz chooseCursor rs
      picWithCursor = case theCursor of
        Nothing  -> pic { picCursor = NoCursor }
        Just loc -> pic { picCursor = Cursor (loc ^. columnL) (loc ^. rowL) }

  update vty picWithCursor

  return newRS

continue :: Next ()
continue = Continue ()

halt :: Next ()
halt = Halt ()
