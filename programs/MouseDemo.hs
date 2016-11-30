{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ((<$>))
import Lens.Micro ((^.), (&), (.~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core

data Name = Info | Button1 | Button2 | Button3 | Prose
          deriving (Show, Ord, Eq)

data St =
    St { _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       , _prose :: String
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ buttonLayer st
    , proseLayer st
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

proseLayer :: St -> Widget Name
proseLayer st =
  B.border $
  C.hCenterLayer $
  vLimit 8 $
  -- n.b. if clickable and viewport are inverted here, click event
  -- coordinates will only identify the viewable range, not the actual
  -- editor widget coordinates.
  viewport Prose Vertical $
  clickable Prose $
  vBox $ map str $ lines (st^.prose)

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
appEvent st (T.MouseDown n _ _ loc) = M.continue $ st & lastReportedClick .~ Just (n, loc)
appEvent st (T.MouseUp _ _ _) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (T.VtyEvent (V.EvMouseUp _ _ _)) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (T.VtyEvent (V.EvKey V.KUp [])) = M.vScrollBy (M.viewportScroll Prose) (-1) >> M.continue st
appEvent st (T.VtyEvent (V.EvKey V.KDown [])) = M.vScrollBy (M.viewportScroll Prose) 1 >> M.continue st
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
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    void $ M.customMain buildVty Nothing app $ St [] Nothing
         $ "Press up and down arrow keys to scroll, ESC to quit.\n\
           \Observe the click coordinates identify the\n\
           \underlying widget coordinates.\n\
           \\n\
           \Lorem ipsum dolor sit amet,\n\
           \consectetur adipiscing elit,\n\
           \sed do eiusmod tempor incididunt ut labore\n\
           \et dolore magna aliqua.\n\
           \ \n\
           \Ut enim ad minim veniam\n\
           \quis nostrud exercitation ullamco laboris\n\
           \nisi ut aliquip ex ea commodo consequat.\n\
           \\n\
           \Duis aute irure dolor in reprehenderit\n\
           \in voluptate velit esse cillum dolore eu fugiat nulla pariatur.\n\
           \\n\
           \Excepteur sint occaecat cupidatat not proident,\n\
           \sunt in culpa qui officia deserunt mollit\n\
           \anim id est laborum.\n"
