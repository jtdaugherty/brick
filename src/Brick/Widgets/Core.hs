-- | This module provides the core widget combinators and rendering
-- routines. Everything this library does is in terms of these basic
-- primitives.
module Brick.Widgets.Core
  ( Widget(..)
  , Size(..)

  -- * Basic rendering primitives
  , emptyWidget
  , raw
  , txt
  , multilineTxt
  , str
  , multilineStr
  , fill

  -- * Padding
  , Padding(..)
  , padLeft
  , padRight
  , padTop
  , padBottom
  , padLeftRight
  , padTopBottom
  , padAll

  -- * Box layout
  , (<=>)
  , (<+>)
  , hBox
  , vBox

  -- * Limits
  , hLimit
  , vLimit

  -- * Attribute mangement
  , withDefAttr
  , withAttr
  , forceAttr
  , updateAttrMap

  -- * Border style management
  , withBorderStyle

  -- * Cursor placement
  , showCursor

  -- * Translation
  , translateBy

  -- * Cropping
  , cropLeftBy
  , cropRightBy
  , cropTopBy
  , cropBottomBy

  -- * Scrollable viewports
  , ViewportType(..)
  , viewport
  , visible
  , visibleRegion

  -- * Rendering infrastructure
  , RenderM
  , getContext
  , lookupAttrName

  -- ** The rendering context
  , Context(ctxAttrName, availWidth, availHeight, ctxBorderStyle, ctxAttrMap)
  , attrL
  , availWidthL
  , availHeightL
  , ctxAttrMapL
  , ctxAttrNameL
  , ctxBorderStyleL

  -- ** Rendering results
  , Result(..)
  -- ** Result lenses
  , imageL
  , cursorsL
  , visibilityRequestsL
  -- ** Visibility requests
  , VisibilityRequest(..)
  , vrPositionL
  , vrSizeL
  -- ** Adding offsets to cursor positions and visibility requests
  , addResultOffset
  -- ** Cropping results
  , cropToContext

  -- * Misc
  , Direction(..)
  )
where

import Brick.Widgets.Internal
