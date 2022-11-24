{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | This module provides a simple dialog widget. You get to pick the
-- dialog title, if any, as well as its body and buttons.
--
-- Note that this dialog is really for simple use cases where you want
-- to get the user's answer to a question, such as "Would you like to
-- save changes before quitting?" As is typical in such cases, we assume
-- that this dialog box is used modally, meaning that while it is open
-- it is has exclusive input focus until it is closed.
--
-- If you require something more sophisticated, you'll need to build it
-- yourself. You might also consider seeing the 'Brick.Forms' module for
-- help with input management and see the implementation of this module
-- to see how to reproduce a dialog-style UI.
module Brick.Widgets.Dialog
  ( Dialog
  , dialogTitle
  , dialogButtons
  , dialogWidth
  -- * Construction and rendering
  , dialog
  , renderDialog
  , getDialogFocus
  , setDialogFocus
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
  , dialogWidthL
  , dialogTitleL
  )
where

import Lens.Micro
import Lens.Micro.Mtl ((%=))
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.List (intersperse, find)
import Graphics.Vty.Input (Event(..), Key(..))

import Brick.Focus
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
data Dialog a n =
    Dialog { dialogTitle :: Maybe (Widget n)
           -- ^ The dialog title
           , dialogButtons :: [(String, n, a)]
           -- ^ The dialog buttons' labels, resource names, and values
           , dialogWidth :: Int
           -- ^ The maximum width of the dialog
           , dialogFocus :: FocusRing n
           -- ^ The focus ring for the dialog's buttons
           }

suffixLenses ''Dialog

handleDialogEvent :: Event -> EventM n (Dialog a n) ()
handleDialogEvent ev = do
    case ev of
        EvKey (KChar '\t') [] -> dialogFocusL %= focusNext
        EvKey KRight []       -> dialogFocusL %= focusNext
        EvKey KBackTab []     -> dialogFocusL %= focusPrev
        EvKey KLeft []        -> dialogFocusL %= focusPrev
        _ -> return ()

-- | Set the focused button of a dialog.
setDialogFocus :: (Eq n) => n -> Dialog a n -> Dialog a n
setDialogFocus n d = d { dialogFocus = focusSetCurrent n $ dialogFocus d }

-- | Get the focused button of a dialog.
getDialogFocus :: Dialog a n -> Maybe n
getDialogFocus = focusGetCurrent . dialogFocus

-- | Create a dialog.
dialog :: (Eq n)
       => Maybe (Widget n)
       -- ^ The dialog title
       -> Maybe (n, [(String, n, a)])
       -- ^ The currently-selected button resource name and the button
       -- labels, resource names, and values to use for each button,
       -- respectively
       -> Int
       -- ^ The maximum width of the dialog
       -> Dialog a n
dialog title buttonData w =
    let (r, buttons) = case buttonData of
            Nothing ->
                (focusRing [], [])
            Just (focName, entries) ->
                let ns = (\(_, n, _) -> n) <$> entries
                in (focusSetCurrent focName $ focusRing ns, entries)
    in Dialog title buttons w r

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
renderDialog :: (Ord n) => Dialog a n -> Widget n -> Widget n
renderDialog d body =
    let buttonPadding = str "   "
        foc = focusGetCurrent $ dialogFocus d
        mkButton (s, n, _) =
            let att = if Just n == foc
                      then buttonSelectedAttr
                      else buttonAttr
                csr = if Just n == foc
                      then putCursor n (Location (1,0))
                      else id
            in csr $
               clickable n $
               withAttr att $
               str $ "  " <> s <> "  "
        buttons = hBox $ intersperse buttonPadding $
                         mkButton <$> (d^.dialogButtonsL)

        doBorder = maybe border borderWithLabel (d^.dialogTitleL)
    in centerLayer $
       withDefAttr dialogAttr $
       hLimit (d^.dialogWidthL) $
       doBorder $
       vBox [ body
            , hCenter buttons
            ]

-- | Obtain the resource name and value associated with the dialog's
-- currently-selected button, if any. The result of this function is
-- probably what you want when someone presses 'Enter' in a dialog.
dialogSelection :: (Eq n) => Dialog a n -> Maybe (n, a)
dialogSelection d = do
    n' <- focusGetCurrent $ dialogFocus d
    let matches (_, n, _) = n == n'
    (_, n, a) <- find matches (d^.dialogButtonsL)
    return (n, a)
