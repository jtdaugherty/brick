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

data Name = NameField
          | AgeField
          | BikeField
          | HandedField
          | PasswordField
          | LeftHandField
          | RightHandField
          | AmbiField
          deriving (Eq, Ord, Show)

data Handedness = LeftHanded | RightHanded | Ambidextrous
                deriving (Show, Eq)

data FormState =
    FormState { _name        :: T.Text
              , _age         :: Int
              , _ridesBike   :: Bool
              , _handed      :: Handedness
              , _password    :: T.Text
              }
              deriving (Show)

makeLenses ''FormState

mkForm :: FormState -> Form FormState e Name
mkForm =
    newForm [ ((str "Name: ") <+>) @@=
                editTextField name NameField (Just 1)
            , (str "Age: " <+>) @@=
                editShowableField age AgeField
            , (str "Password: " <+>) @@=
                editPasswordField password PasswordField
            , (str "Dominant hand: " <+>) @@=
                radioField handed [ (LeftHanded, LeftHandField, "Left")
                                  , (RightHanded, RightHandField, "Right")
                                  , (Ambidextrous, AmbiField, "Both")
                                  ]
            , (<+> str " Do you ride a bicycle?") @@=
                checkboxField ridesBike BikeField
            ]

theMap :: AttrMap
theMap = attrMap defAttr
  [ (editAttr, white `on` black)
  , (editFocusedAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)
  , (focusedFormInputAttr, black `on` yellow)
  ]

app :: App (Form FormState e Name) e Name
app =
    App { appDraw = \s -> [center $ border $ padAll 1 $ hLimit 50 $ renderForm s]
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
    let initialForm = FormState { _name = ""
                                , _age = 0
                                , _handed = RightHanded
                                , _ridesBike = False
                                , _password = ""
                                }
        f = mkForm initialForm
    f' <- defaultMain app f
    print $ formState f'
