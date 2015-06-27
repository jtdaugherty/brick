module Brick.Widgets.Core
  ( Widget(..)
  , Size(..)
  , RenderM

  , Result
  , image
  , attr
  , ctxAttrs
  , cursors
  , lookupAttrName
  , visibilityRequests

  , Context
  , availW
  , availH
  , getActiveBorderStyle
  , getContext
  , Priority(..)
  , (=>>), (<<=), (<=>)
  , (+>>), (<<+), (<+>)
  , ViewportType(..)
  , Direction(..)

  , txt
  , str
  , pad
  , hFill
  , vFill
  , hBox
  , vBox
  , hLimit
  , vLimit
  , withDefaultAttr
  , withAttrName
  , withAttrMappings
  , forceAttr
  , raw
  , withBorderStyle
  , translateBy
  , cropLeftBy
  , cropRightBy
  , cropTopBy
  , cropBottomBy
  , showCursor
  , viewport
  , visible
  , visibleRegion
  )
where

import Brick.Widgets.Internal
