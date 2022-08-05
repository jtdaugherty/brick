{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | This module provides a simple dialog widget. You get to pick the
-- dialog title, if any, as well as its body and buttons.
--
-- Note that this dialog is really for simple use cases where you want
-- to get the user's answer to a question, such as "Would you like
-- to save changes before quitting?" If you require something more
-- sophisticated, you'll need to build it yourself. You might also
-- consider seeing the 'Brick.Forms' module for help with input
-- management, and see the implementation of this module to see how to
-- reproduce a dialog-style UI.
module Brick.Widgets.Dialog
  ( Dialog
  , dialogTitle
  , dialogButtons
  , dialogSelectedIndex
  , dialogWidth
  -- * Construction and rendering
  , dialog
  , renderDialog
  -- * Handling events
  , handleDialogEvent
  -- * Getting a dialog's current value
  , dialogSelection
  -- * Attributes
  , dialogAttr
  , buttonAttr
  , buttonSelectedAttr
  -- * Lenses
  , dialogButtonsL
  , dialogSelectedIndexL
  , dialogWidthL
  , dialogTitleL
  )
where

import Lens.Micro
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.List (intersperse)
import Graphics.Vty.Input (Event(..), Key(..))

import Brick.Util (clamp)
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.AttrMap

-- | Dialogs present a window with a title (optional), a body, and
-- buttons (optional). Dialog buttons are labeled with strings and map
-- to values of type 'a', which you choose.
--
-- Dialogs handle the following events by default with
-- handleDialogEvent:
--
-- * Tab or Right Arrow: select the next button
-- * Shift-tab or Left Arrow: select the previous button
data Dialog a =
    Dialog { dialogTitle :: Maybe String
           -- ^ The dialog title
           , dialogButtons :: [(String, a)]
           -- ^ The dialog button labels and values
           , dialogSelectedIndex :: Maybe Int
           -- ^ The currently selected dialog button index (if any)
           , dialogWidth :: Int
           -- ^ The maximum width of the dialog
           }

suffixLenses ''Dialog

handleDialogEvent :: Event -> EventM n (Dialog a) ()
handleDialogEvent ev = do
    modify $ \d -> case ev of
        EvKey (KChar '\t') [] -> nextButtonBy 1 True d
        EvKey KBackTab [] -> nextButtonBy (-1) True d
        EvKey KRight [] -> nextButtonBy 1 False d
        EvKey KLeft [] -> nextButtonBy (-1) False d
        _ -> d

-- | Create a dialog.
dialog :: Maybe String
       -- ^ The dialog title
       -> Maybe (Int, [(String, a)])
       -- ^ The currently-selected button index (starting at zero) and
       -- the button labels and values to use
       -> Int
       -- ^ The maximum width of the dialog
       -> Dialog a
dialog title buttonData w =
    let (buttons, idx) = case buttonData of
          Nothing -> ([], Nothing)
          Just (_, []) -> ([], Nothing)
          Just (i, bs) -> (bs, Just $ clamp 0 (length bs - 1) i)
    in Dialog title buttons idx w

-- | The default attribute of the dialog
dialogAttr :: AttrName
dialogAttr = attrName "dialog"

-- | The default attribute for all dialog buttons
buttonAttr :: AttrName
buttonAttr = attrName "button"

-- | The attribute for the selected dialog button (extends 'dialogAttr')
buttonSelectedAttr :: AttrName
buttonSelectedAttr = buttonAttr <> attrName "selected"

-- | Render a dialog with the specified body widget. This renders the
-- dialog as a layer, which makes this suitable as a top-level layer in
-- your rendering function to be rendered on top of the rest of your
-- interface.
renderDialog :: Dialog a -> Widget n -> Widget n
renderDialog d body =
    let buttonPadding = str "   "
        mkButton (i, (s, _)) = let att = if Just i == d^.dialogSelectedIndexL
                                         then buttonSelectedAttr
                                         else buttonAttr
                               in withAttr att $ str $ "  " <> s <> "  "
        buttons = hBox $ intersperse buttonPadding $
                         mkButton <$> (zip [0..] (d^.dialogButtonsL))

        doBorder = maybe border borderWithLabel (str <$> d^.dialogTitleL)
    in centerLayer $
       withDefAttr dialogAttr $
       hLimit (d^.dialogWidthL) $
       doBorder $
       vBox [ body
            , hCenter buttons
            ]

nextButtonBy :: Int -> Bool -> Dialog a -> Dialog a
nextButtonBy amt wrapCycle d =
    let numButtons = length $ d^.dialogButtonsL
    in if numButtons == 0 then d
       else case d^.dialogSelectedIndexL of
           Nothing -> d & dialogSelectedIndexL .~ (Just 0)
           Just i -> d & dialogSelectedIndexL .~ (Just newIndex)
               where
                   addedIndex = i + amt
                   newIndex = if wrapCycle
                              then addedIndex `mod` numButtons
                              else max 0 $ min addedIndex $ numButtons - 1

-- | Obtain the value associated with the dialog's currently-selected
-- button, if any. This function is probably what you want when someone
-- presses 'Enter' in a dialog.
dialogSelection :: Dialog a -> Maybe a
dialogSelection d =
    case d^.dialogSelectedIndexL of
        Nothing -> Nothing
        Just i -> Just $ ((d^.dialogButtonsL) !! i)^._2
