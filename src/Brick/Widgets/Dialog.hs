{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module provides a simple dialog widget. You get to pick the
-- dialog title, if any, as well as its body and buttons.
module Brick.Widgets.Dialog
  ( Dialog
  , dialogTitle
  , dialogName
  , dialogButtons
  , dialogSelectedIndex
  , dialogWidth
  -- * Construction and rendering
  , dialog
  , renderDialog
  -- * Getting a dialog's current value
  , dialogSelection
  -- * Attributes
  , dialogAttr
  , buttonAttr
  , buttonSelectedAttr
  -- * Lenses
  , dialogNameL
  , dialogButtonsL
  , dialogSelectedIndexL
  , dialogWidthL
  , dialogTitleL
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

-- | Dialogs present a window with a title (optional), a body, and
-- buttons (optional). They provide a 'HandleEvent' instance that knows
-- about Tab and Shift-Tab for changing which button is active. Dialog
-- buttons are labeled with strings and map to values of type 'a', which
-- you choose.
--
-- Dialogs handle the following events by default:
--
-- * Tab: selecte the next button
-- * Shift-tab: select the previous button
data Dialog a =
    Dialog { dialogName :: Name
           -- ^ The dialog name
           , dialogTitle :: Maybe String
           -- ^ The dialog title
           , dialogButtons :: [(String, a)]
           -- ^ The dialog button labels and values
           , dialogSelectedIndex :: Maybe Int
           -- ^ The currently selected dialog button index (if any)
           , dialogWidth :: Int
           -- ^ The maximum width of the dialog
           }

suffixLenses ''Dialog

instance HandleEvent (Dialog a) where
    handleEvent ev d =
        case ev of
            EvKey (KChar '\t') [] -> nextButtonBy 1 d
            EvKey KBackTab [] -> nextButtonBy (-1) d
            _ -> d

-- | Create a dialog.
dialog :: Name
       -- ^ The dialog name, provided so that you can use this as a
       -- basis for viewport names in the dialog if desired
       -> Maybe String
       -- ^ The dialog title
       -> Maybe (Int, [(String, a)])
       -- ^ The currently-selected button index (starting at zero) and
       -- the button labels and values to use
       -> Int
       -- ^ The maximum width of the dialog
       -> Dialog a
dialog name title buttonData w =
    let (buttons, idx) = case buttonData of
          Nothing -> ([], Nothing)
          Just (_, []) -> ([], Nothing)
          Just (i, bs) -> (bs, Just $ clamp 0 (length bs - 1) i)
    in Dialog name title buttons idx w

-- | The default attribute of the dialog
dialogAttr :: AttrName
dialogAttr = "dialog"

-- | The default attribute for all dialog buttons
buttonAttr :: AttrName
buttonAttr = "button"

-- | The attribute for the selected dialog button (extends 'dialogAttr')
buttonSelectedAttr :: AttrName
buttonSelectedAttr = buttonAttr <> "selected"

-- | Render a dialog with the specified body widget.
renderDialog :: Dialog a -> Widget -> Widget
renderDialog d body =
    let buttonPadding = str "   "
        mkButton (i, (s, _)) = let att = if Just i == d^.dialogSelectedIndexL
                                         then buttonSelectedAttr
                                         else buttonAttr
                               in withAttr att $ str $ "  " <> s <> "  "
        buttons = hBox $ intersperse buttonPadding $
                         mkButton <$> (zip [0..] (d^.dialogButtonsL))

        doBorder = maybe border borderWithLabel (str <$> d^.dialogTitleL)
    in center $
       withDefAttr dialogAttr $
       hLimit (d^.dialogWidthL) $
       doBorder $
       vBox [ body
            , hCenter buttons
            ]

nextButtonBy :: Int -> Dialog a -> Dialog a
nextButtonBy amt d =
    let numButtons = length $ d^.dialogButtonsL
    in if numButtons == 0 then d
       else case d^.dialogSelectedIndexL of
           Nothing -> d & dialogSelectedIndexL .~ (Just 0)
           Just i -> d & dialogSelectedIndexL .~ (Just $ (i + amt) `mod` numButtons)

-- | Obtain the value associated with the dialog's currently-selected
-- button, if any. This function is probably what you want when someone
-- presses 'Enter' in a dialog.
dialogSelection :: Dialog a -> Maybe a
dialogSelection d =
    case d^.dialogSelectedIndexL of
        Nothing -> Nothing
        Just i -> Just $ ((d^.dialogButtonsL) !! i)^._2
