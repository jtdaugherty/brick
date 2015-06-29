{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Criterion.Main
import Control.DeepSeq (NFData(..))
import Data.Monoid
import Data.Default
import Graphics.Vty
import qualified Data.Text as T

import Brick.AttrMap
import Brick.Widgets.Core
import Brick.Widgets.Border

instance NFData Widget where
    rnf w = (hSize w) `seq` (vSize w) `seq` (render w) `seq` ()

aMap :: AttrMap
aMap = attrMap def []

sz :: DisplayRegion
sz = (100, 100)

renderBench :: Widget -> Picture
renderBench w = pic
    where
        (_, pic, _) = renderFinal aMap [w] sz (const Nothing) (RS mempty mempty)

mkBench :: String -> Widget -> Benchmark
mkBench s w = bench s (nf renderBench w)

allGroups :: [Benchmark]
allGroups =
    [ bgroup "widgets"
        [ mkBench "str"                 (str "testing")
        , mkBench "txt"                 (txt $ T.pack "testing")
        , mkBench "hBorder"             hBorder
        , mkBench "hBorderWithLabel"    (hBorderWithLabel (str "label"))
        , mkBench "vBorder"             vBorder
        , mkBench "border"              (border $ str "testing")
        , mkBench "fill"                (fill ' ')
        , mkBench "hFill"               (hFill ' ')
        , mkBench "vFill"               (vFill ' ')
        , mkBench "hBox"                (hBox [str "testing", str "testing"])
        , mkBench "vBox"                (vBox [str "testing", str "testing"])
        , mkBench "empty"               emptyWidget
        , mkBench "hLimit"              (hLimit 1 $ str "t")
        , mkBench "vLimit"              (vLimit 1 $ str "testing")

        -- withDefaultAttr
        -- withDefaultAttrName
        -- withAttrName
        -- withAttrMappings
        -- forceAttr
        -- raw
        -- withBorderStyle
        -- translateBy
        -- cropLeftBy
        -- cropRightBy
        -- cropTopBy
        -- cropBottomBy
        -- showCursor
        -- viewport
        -- visible
        -- visibleRegion
        ]
    ]

main :: IO ()
main = defaultMain allGroups
