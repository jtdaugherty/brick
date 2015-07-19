{-# LANGUAGE OverloadedStrings #-}
-- | This module provides a progress bar widget.
module Brick.Widgets.ProgressBar
  ( progressBar
  -- * Attributes
  , progressCompleteAttr
  , progressIncompleteAttr
  )
where

import Control.Lens ((^.))
import Data.Monoid

import Brick.AttrMap
import Brick.Widgets.Core

-- | The attribute of the completed portion of the progress bar.
progressCompleteAttr :: AttrName
progressCompleteAttr = "progressComplete"

-- | The attribute of the incomplete portion of the progress bar.
progressIncompleteAttr :: AttrName
progressIncompleteAttr = "progressIncomplete"

-- | Draw a progress bar with the specified (optional) label and
-- progress value. This fills available horizontal space and is one row
-- high.
progressBar :: Maybe String
            -- ^ The label. If specified, this is shown in the center of
            -- the progress bar.
            -> Float
            -- ^ The progress value. Should be between 0 and 1 inclusive.
            -> Widget
progressBar mLabel progress =
    Widget Unlimited Fixed $ do
        c <- getContext
        let barWidth = c^.availWidthL
            label = maybe "" id mLabel
            labelWidth = length label
            spacesWidth = barWidth - labelWidth
            leftPart = replicate (spacesWidth `div` 2) ' '
            rightPart = replicate (barWidth - (labelWidth + length leftPart)) ' '
            fullBar = leftPart <> label <> rightPart
            completeWidth = round $ progress * toEnum barWidth
            completePart = take completeWidth fullBar
            incompletePart = drop completeWidth fullBar
        render $ (withAttr progressCompleteAttr $ str completePart) <+>
                 (withAttr progressIncompleteAttr $ str incompletePart)
