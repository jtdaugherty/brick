{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.), (&), (.~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core

data Name = Info | Button1 | Button2 | Button3 deriving (Show, Ord, Eq)

data St =
    St { _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ buttonLayer st
    , infoLayer st
    ]

buttonLayer :: St -> Widget Name
buttonLayer st =
    C.centerLayer $ hBox $ padAll 1 <$> buttons
    where
        buttons = mkButton <$> buttonData
        buttonData = [ (Button1, "Button 1", "button1")
                     , (Button2, "Button 2", "button2")
                     , (Button3, "Button 3", "button3")
                     ]
        mkButton (name, label, attr) =
            let wasClicked = (fst <$> st^.lastReportedClick) == Just name
            in clickable name $
               withDefAttr attr $
               B.border $
               padTopBottom 1 $
               padLeftRight (if wasClicked then 2 else 3) $
               str (if wasClicked then "<" <> label <> ">" else label)

infoLayer :: St -> Widget Name
infoLayer st = T.Widget T.Fixed T.Fixed $ do
    c <- T.getContext
    let h = c^.T.availHeightL
        msg = case st^.lastReportedClick of
                Nothing -> "nothing"
                Just (name, T.Location l) -> show name <> " at " <> show l
    T.render $ translateBy (T.Location (0, h-1)) $ clickable Info $
               withDefAttr "info" $
               C.hCenter (str ("Last reported click: " <> msg))

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.Clicked n _ _ loc) = M.continue $ st & lastReportedClick .~ Just (n, loc)
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ ("info",      V.white `on` V.magenta)
    , ("button1",   V.white `on` V.cyan)
    , ("button2",   V.white `on` V.green)
    , ("button3",   V.white `on` V.blue)
    ]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app $ St [] Nothing
