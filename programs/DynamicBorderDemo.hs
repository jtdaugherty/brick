{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

doubleHorizontal :: BS.BorderStyle
doubleHorizontal = BS.BorderStyle
    { BS.bsCornerTL = '╒'
    , BS.bsCornerTR = '╕'
    , BS.bsCornerBR = '╛'
    , BS.bsCornerBL = '╘'
    , BS.bsIntersectL = '╞'
    , BS.bsIntersectR = '╡'
    , BS.bsIntersectT = '╤'
    , BS.bsIntersectB = '╧'
    , BS.bsIntersectFull = '╪'
    , BS.bsHorizontal = '═'
    , BS.bsVertical = '│'
    }

box1 :: Widget ()
box1
    = C.withBorderStyle doubleHorizontal . B.border
    . C.withBorderStyle BS.unicodeRounded . B.border
    $ C.str "25 kg"

weights :: Widget ()
weights = C.withBorderStyle doubleHorizontal $ C.hBox
    [ box1
    , C.str "\n\n" C.<=> B.hBorder
    , box1
    ]

box2 :: Widget ()
box2 = C.freezeBorders $ C.vBox
    [ C.hBox
        [ C.vLimit 3 B.vBorder
        , C.str "Resize horizontally to\nmove across the label\nbelow"
        , C.vLimit 3 B.vBorder
        ]
    , B.borderWithLabel (B.vBorder C.<+> C.str " Label " C.<+> B.vBorder) $ C.hBox
        [ C.str "               "
        , C.vBox [B.vBorder, C.str "L\na\nb\ne\nl", C.vLimit 3 B.vBorder]
        , C.str "\n\n\n Resize vertically to\n move across the label\n to the left\n\n\n\n\n" C.<=> B.hBorder
        ]
    ]

-- BYOB: build your own border
byob :: Widget ()
byob = C.vBox
    [              C.hBox [ corner   , top      , corner   ]
    , C.vLimit 6 $ C.hBox [ B.vBorder, mid      , B.vBorder]
    ,              C.hBox [ corner   , B.hBorder, corner   ]
    ]
    where
    top = B.hBorderWithLabel (C.str "BYOB")
    mid = C.center (C.str "If `border` is too easy,\nyou can build it yourself")
    corner = B.joinableBorder (pure False)

ui :: Widget ()
ui = C.vBox [weights, box2, byob]

main :: IO ()
main = M.simpleMain (C.joinBorders ui)
