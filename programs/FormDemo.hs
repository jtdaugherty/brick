{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Lens.Micro.TH

import Graphics.Vty
import Brick
import Brick.Forms
import Brick.Focus
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Center

data Name = Edit1
          | Edit2
          | Password
          deriving (Eq, Ord, Show)

data FormState =
    FormState { _field1        :: Int
              , _field2        :: Int
              , _fieldPassword :: T.Text
              }
              deriving (Show)

makeLenses ''FormState

mkForm :: FormState -> Form FormState e Name
mkForm =
    newForm [ editShowableField field1 Edit1
              `withHelper` (\w -> str "Edit 1: " <+> w)
            , editShowableField field2 Edit2
              `withHelper` (\w -> str "Edit 2: " <+> w)
            , editPasswordField fieldPassword Password
              `withHelper` (\w -> str "Password: " <+> w)
            ]

theMap :: AttrMap
theMap = attrMap defAttr
  [ (editAttr, white `on` black)
  , (editFocusedAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)
  ]

app :: App (Form FormState e Name) e Name
app =
    App { appDraw = \s -> [center $ border $ hLimit 50 $ renderForm s]
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (EvResize {})     -> continue s
                VtyEvent (EvKey KEsc [])   -> halt s
                VtyEvent (EvKey KEnter []) -> halt s
                _                          -> continue =<< handleFormEvent ev s
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

main :: IO ()
main = do
    let f = mkForm $ FormState 10 20 ""
    f' <- defaultMain app f
    print $ formState f'
