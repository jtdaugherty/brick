{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module provides a progress bar widget.
module Brick.Widgets.ProgressBar
  ( progressBar
  , progressBar'
  -- * Attributes
  , progressCompleteAttr
  , progressIncompleteAttr
  )
where

import Data.Maybe (fromMaybe)
import Lens.Micro ((^.))
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty (safeWcswidth)

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
progressBar mLabel progress =
    progressBar' mLabel progress (' ', ' ')

-- | Draw a progress bar with the specified (optional) label,
-- progress value and a pair of characters to fill the progress. 
-- This fills available horizontal space and is one row high.
progressBar' :: Maybe String
             -- ^ The label. If specified, this is shown in the center of
             -- the progress bar.
             -> Float
             -- ^ The progress value. Should be between 0 and 1 inclusive.
             -> (Char, Char)
             -- ^ Pair of characters to fill the bar. First character is used 
             -- to fill the completed part, second character draws the uncompleted part.
             -- Please be aware of using wide characters in Brick, 
             -- see [Wide Character Support and the TextWidth class](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#wide-character-support-and-the-textwidth-class)
             -> Widget n
progressBar' mLabel progress (completeChar, uncompleteChar) =
    Widget Greedy Fixed $ do
        c <- getContext
        let barWidth = c ^. availWidthL
            label = fromMaybe "" mLabel
            labelWidth = safeWcswidth label
            spacesWidth = barWidth - labelWidth
            leftWidth = spacesWidth `div` 2
            rightWidth = barWidth - labelWidth - leftWidth
            completeWidth = round $ progress * toEnum barWidth
            
            leftCompleteWidth = min leftWidth completeWidth
            leftIncompleteWidth = leftWidth - leftCompleteWidth         
            leftPart = replicate leftCompleteWidth completeChar ++ replicate leftIncompleteWidth uncompleteChar
            rightCompleteWidth = max 0 (completeWidth - labelWidth - leftWidth)
            rightIncompleteWidth = rightWidth - rightCompleteWidth
            rightPart = replicate rightCompleteWidth completeChar ++ replicate rightIncompleteWidth uncompleteChar
    
            fullBar = leftPart <> label <> rightPart
            adjustedCompleteWidth = if completeWidth == length fullBar && progress < 1.0
                                    then completeWidth - 1
                                    else if completeWidth == 0 && progress > 0.0
                                         then 1
                                         else completeWidth
            (completePart, incompletePart) = splitAt adjustedCompleteWidth fullBar
        render $ (withAttr progressCompleteAttr $ str completePart) <+>
                 (withAttr progressIncompleteAttr $ str incompletePart)
