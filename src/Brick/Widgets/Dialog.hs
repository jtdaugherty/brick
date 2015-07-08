{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.Dialog
  ( Dialog
  , dialog
  , renderDialog
  , dialogSelection
  , dialogAttr
  , buttonAttr
  , buttonSelectedAttr
  )
where

import Control.Lens
import Control.Applicative
import Data.Monoid
import Data.List (intersperse)
import Graphics.Vty.Input (Event(..), Key(..))

import Brick.Util (clamp)
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.AttrMap

data Dialog a =
    Dialog { dialogName :: Name
           , dialogTitle :: Maybe String
           , dialogButtons :: [(String, a)]
           , dialogSelectedIndex :: Maybe Int
           , dialogWidth :: Int
           }

suffixLenses ''Dialog

instance HandleEvent (Dialog a) where
    handleEvent ev d =
        case ev of
            EvKey (KChar '\t') [] -> nextButton d
            EvKey KBackTab [] -> prevButton d
            _ -> d

dialog :: Name -> Maybe String -> Maybe (Int, [(String, a)]) -> Int -> Dialog a
dialog name title buttonData w =
    let (buttons, idx) = case buttonData of
          Nothing -> ([], Nothing)
          Just (_, []) -> ([], Nothing)
          Just (i, bs) -> (bs, Just $ clamp 0 (length bs - 1) i)
    in Dialog name title buttons idx w

dialogAttr :: AttrName
dialogAttr = "dialog"

buttonAttr :: AttrName
buttonAttr = "button"

buttonSelectedAttr :: AttrName
buttonSelectedAttr = buttonAttr <> "selected"

renderDialog :: Dialog a -> Widget -> Widget
renderDialog d body =
    let buttonPadding = "  "
        mkButton (i, (s, _)) = let att = if Just i == d^.dialogSelectedIndexL
                                         then buttonSelectedAttr
                                         else buttonAttr
                               in withAttr att $ str $ "  " <> s <> "  "
        buttons = hBox $ intersperse buttonPadding $
                         mkButton <$> (zip [0..] (d^.dialogButtonsL))

        doBorder = maybe border borderWithLabel (str <$> d^.dialogTitleL)
    in center $
       withDefaultAttr dialogAttr $
       doBorder $
       hLimit (d^.dialogWidthL) $
       vBox [ body
            , hCenter buttons
            ]

nextButton :: Dialog a -> Dialog a
nextButton d =
    if null (d^.dialogButtonsL)
    then d
    else case d^.dialogSelectedIndexL of
        Nothing -> d & dialogSelectedIndexL .~ (Just 0)
        Just i -> d & dialogSelectedIndexL .~ (Just $ (i + 1) `mod` (length (d^.dialogButtonsL)))

prevButton :: Dialog a -> Dialog a
prevButton d =
    if null (d^.dialogButtonsL)
    then d
    else case d^.dialogSelectedIndexL of
        Nothing -> d & dialogSelectedIndexL .~ (Just 0)
        Just i -> d & dialogSelectedIndexL .~ (Just $ (i - 1) `mod` (length (d^.dialogButtonsL)))

dialogSelection :: Dialog a -> Maybe a
dialogSelection d =
    case d^.dialogSelectedIndexL of
        Nothing -> Nothing
        Just i -> Just $ ((d^.dialogButtonsL) !! i)^._2
