{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( vBox
  , padTopBottom
  , withDefAttr
  , cached
  , padBottom
  , str
  )
import Brick (on)
import Brick.Widgets.Center
  ( hCenter
  )
import Brick.AttrMap
  ( AttrName
  , attrMap
  )

data Name = ExpensiveWidget
          deriving (Ord, Show, Eq)

drawUi :: Int -> [Widget Name]
drawUi i = [ui]
    where
        ui = C.vCenter $
             vBox $ hCenter <$>
             [ str "This demo shows how cached widgets behave. The top widget below"
             , str "is cacheable, so once it's rendered, brick re-uses the rendering"
             , str "each time it is drawn. The bottom widget is not cacheable so it is"
             , str "drawn on every request. Brick supports cache invalidation to force"
             , str "a redraw of cached widgets; we can trigger that here with 'i'. Notice"
             , str "how state changes with '+' aren't reflected in the cached widget"
             , str "until the cache is invalidated with 'i'."
             , padTopBottom 1 $
               cached ExpensiveWidget $
               withDefAttr emphAttr $ str $ "This widget is cached (state = " <> show i <> ")"
             , padBottom (T.Pad 1) $
               withDefAttr emphAttr $ str $ "This widget is not cached (state = " <> show i <> ")"
             , hCenter $ str "Press 'i' to invalidate the cache,"
             , str "'+' to change the state value, and"
             , str "'Esc' to quit."
             ]

appEvent :: Int -> BrickEvent Name e -> T.EventM Name (T.Next Int)
appEvent i (VtyEvent (V.EvKey (V.KChar '+') [])) = M.continue $ i + 1
appEvent i (VtyEvent (V.EvKey (V.KChar 'i') [])) = M.invalidateCacheEntry ExpensiveWidget >> M.continue i
appEvent i (VtyEvent (V.EvKey V.KEsc [])) = M.halt i
appEvent i _ = M.continue i

emphAttr :: AttrName
emphAttr = "emphasis"

app :: M.App Int e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr [(emphAttr, V.white `on` V.blue)]
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app 0
