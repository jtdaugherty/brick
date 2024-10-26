{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | This module provides a progress bar widget.
module Brick.Widgets.ProgressBar
  ( progressBar
  , customProgressBar
  -- * Attributes
  , progressCompleteAttr
  , progressIncompleteAttr
  )
where

import Lens.Micro ((^.))
import Data.Maybe (fromMaybe)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Graphics.Vty (safeWcswidth)

import Brick.Types
import Brick.AttrMap
import Brick.Widgets.Core

-- | The attribute of the completed portion of the progress bar.
progressCompleteAttr :: AttrName
progressCompleteAttr = attrName "progressComplete"

-- | The attribute of the incomplete portion of the progress bar.
progressIncompleteAttr :: AttrName
progressIncompleteAttr = attrName "progressIncomplete"

-- | Draw a progress bar with the specified (optional) label and
-- progress value. This fills available horizontal space and is one row
-- high.
progressBar :: Maybe String
            -- ^ The label. If specified, this is shown in the center of
            -- the progress bar.
            -> Float
            -- ^ The progress value. Should be between 0 and 1 inclusive.
            -> Widget n
progressBar = customProgressBar ' ' ' '

-- | Draw a progress bar with the specified (optional) label,
-- progress value and custom characters to fill the progress.
-- This fills available horizontal space and is one row high.
-- Please be aware of using wide characters in Brick,
-- see [Wide Character Support and the TextWidth class](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#wide-character-support-and-the-textwidth-class)
customProgressBar :: Char
                  -- ^ Character to fill the completed part.
                  -> Char
                  -- ^ Character to fill the incomplete part.
                  -> Maybe String
                  -- ^ The label. If specified, this is shown in the center of
                  -- the progress bar.
                  -> Float
                  -- ^ The progress value. Should be between 0 and 1 inclusive.
                  -> Widget n
customProgressBar completeChar incompleteChar mLabel progress =
    Widget Greedy Fixed $ do
        c <- getContext
        let barWidth = c^.availWidthL
            label = fromMaybe "" mLabel
            labelWidth = safeWcswidth label
            spacesWidth = barWidth - labelWidth
            leftWidth = spacesWidth `div` 2
            rightWidth = barWidth - labelWidth - leftWidth
            completeWidth = round $ progress * toEnum barWidth

            leftCompleteWidth = min leftWidth completeWidth
            leftIncompleteWidth = leftWidth - leftCompleteWidth
            leftPart = replicate leftCompleteWidth completeChar ++ replicate leftIncompleteWidth incompleteChar
            rightCompleteWidth = max 0 (completeWidth - labelWidth - leftWidth)
            rightIncompleteWidth = rightWidth - rightCompleteWidth
            rightPart = replicate rightCompleteWidth completeChar ++ replicate rightIncompleteWidth incompleteChar

            fullBar = leftPart <> label <> rightPart
            adjustedCompleteWidth = if completeWidth == length fullBar && progress < 1.0
                                    then completeWidth - 1
                                    else if completeWidth == 0 && progress > 0.0
                                         then 1
                                         else completeWidth
            (completePart, incompletePart) = splitAt adjustedCompleteWidth fullBar
        render $ (withAttr progressCompleteAttr $ str completePart) <+>
                 (withAttr progressIncompleteAttr $ str incompletePart)
