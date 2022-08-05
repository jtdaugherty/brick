{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core

data Name = Info | Button1 | Button2 | Button3 | Prose | TextBox
          deriving (Show, Ord, Eq)

data St =
    St { _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       , _prose :: String
       , _edit :: E.Editor String Name
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
    C.vCenterLayer $
      C.hCenterLayer (padBottom (Pad 1) $ str "Click a button:") <=>
      C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons) <=>
      C.hCenterLayer (padTopBottom 1 $ str "Or enter text and then click in this editor:") <=>
      C.hCenterLayer (vLimit 3 $ hLimit 50 $ E.renderEditor (str . unlines) True (st^.edit))
    where
        buttons = mkButton <$> buttonData
        buttonData = [ (Button1, "Button 1", attrName "button1")
                     , (Button2, "Button 2", attrName "button2")
                     , (Button3, "Button 3", attrName "button3")
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
  viewport Prose Vertical $
  vBox $ map str $ lines (st^.prose)

infoLayer :: St -> Widget Name
infoLayer st = T.Widget T.Fixed T.Fixed $ do
    c <- T.getContext
    let h = c^.T.availHeightL
        msg = case st^.lastReportedClick of
                Nothing ->
                    "Click and hold/drag to report a mouse click"
                Just (name, T.Location l) ->
                    "Mouse down at " <> show name <> " @ " <> show l
    T.render $ translateBy (T.Location (0, h-1)) $ clickable Info $
               withDefAttr (attrName "info") $
               C.hCenter $ str msg

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent ev@(T.MouseDown n _ _ loc) = do
    lastReportedClick .= Just (n, loc)
    zoom edit $ E.handleEditorEvent ev
appEvent (T.MouseUp {}) =
    lastReportedClick .= Nothing
appEvent (T.VtyEvent (V.EvMouseUp {})) =
    lastReportedClick .= Nothing
appEvent (T.VtyEvent (V.EvKey V.KUp [V.MCtrl])) =
    M.vScrollBy (M.viewportScroll Prose) (-1)
appEvent (T.VtyEvent (V.EvKey V.KDown [V.MCtrl])) =
    M.vScrollBy (M.viewportScroll Prose) 1
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    M.halt
appEvent ev =
    zoom edit $ E.handleEditorEvent ev

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ (attrName "info",      V.white `on` V.magenta)
    , (attrName "button1",   V.white `on` V.cyan)
    , (attrName "button2",   V.white `on` V.green)
    , (attrName "button3",   V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    ]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = do
              vty <- M.getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.showFirstCursor
          }

main :: IO ()
main = do
    void $ M.defaultMain app $ St [] Nothing
           (unlines [ "Try clicking on various UI elements."
                    , "Observe that the click coordinates identify the"
                    , "underlying widget coordinates."
                    , ""
                    , "Lorem ipsum dolor sit amet,"
                    , "consectetur adipiscing elit,"
                    , "sed do eiusmod tempor incididunt ut labore"
                    , "et dolore magna aliqua."
                    , ""
                    , "Ut enim ad minim veniam"
                    , "quis nostrud exercitation ullamco laboris"
                    , "isi ut aliquip ex ea commodo consequat."
                    , ""
                    , "Duis aute irure dolor in reprehenderit"
                    , "in voluptate velit esse cillum dolore eu fugiat nulla pariatur."
                    , ""
                    , "Excepteur sint occaecat cupidatat not proident,"
                    , "sunt in culpa qui officia deserunt mollit"
                    , "anim id est laborum."
                    ])
           (E.editor TextBox Nothing "")
