{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Lens.Micro.TH

import Graphics.Vty
import Brick
import Brick.Forms
import Brick.Focus
import Brick.Widgets.Edit

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

formRenderer :: Form FormState e Name -> Name -> Widget Name -> Widget Name
formRenderer _ n w =
    let label = case n of
          Edit1 -> "Editor 1"
          Edit2 -> "Editor 2"
          Password -> "Password"
    in hLimit 50 $
       (vLimit 1 $ hLimit 14 (str (label <> ":") <+> fill ' ')) <+> w

mkForm :: FormState -> Form FormState e Name
mkForm =
    newForm formRenderer
      [ editShowableField field1 Edit1
      , editShowableField field2 Edit2
      , editPasswordField fieldPassword Password
      ]

theMap :: AttrMap
theMap = attrMap defAttr
  [ (editAttr, white `on` black)
  , (editFocusedAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)
  ]

app :: App (Form FormState e Name) e Name
app =
    App { appDraw = \s -> [renderForm s]
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
