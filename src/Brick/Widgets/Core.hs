{-# LANGUAGE CPP #-}
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
  , str
  , fill

  -- * Padding
  , padLeft
  , padRight
  , padTop
  , padBottom

  -- * Box layout
  , (<=>)
  , (<+>)
  , hBox
  , vBox

  -- * Limits
  , hLimit
  , vLimit

  -- * Attribute mangement
  , withDefaultAttr
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
  -- ** Rendering results
  , Result(..)
  -- ** Result lenses
  , image
  , cursors
  -- ** Adding offsets to cursor positions and visibility requests
  , addResultOffset

  -- ** The rendering context
  , Context
  , getContext
  , lookupAttrName
  , getActiveBorderStyle
  -- ** Context lenses
  , attr
  , availW
  , availH
  , ctxAttrs

  -- * Misc
  , Direction(..)

#ifdef BENCH
  , renderFinal
  , RenderState(..)
#endif
  )
where

import Brick.Widgets.Internal
