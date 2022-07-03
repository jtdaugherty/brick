{-# LANGUAGE TypeApplications #-}
module Render
  ( main,
  )

where

import Brick
import qualified Graphics.Vty as V
import Brick.Widgets.Border (hBorder)

region :: V.DisplayRegion
region = (30, 10)

renderDisplay :: Ord n => [Widget n] -> IO ()
renderDisplay ws = do
  outp <- V.outputForConfig V.defaultConfig
  ctx <- V.displayContext outp region
  V.outputPicture ctx (renderWidget ws region)

myWidget :: Widget ()
myWidget = str "Why" <=> hBorder <=> str "not?"

-- Since you can't Read a Picture, we have to compare the result with the Shown one
renderedMyWidget :: String
renderedMyWidget = "Picture ?? [VertJoin {partTop = VertJoin {partTop = HorizText {attr = Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default, attrURL = Default}, displayText = \"Why                           \", outputWidth = 30, charWidth = 30}, partBottom = VertJoin {partTop = HorizText {attr = Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default, attrURL = Default}, displayText = \"\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\", outputWidth = 30, charWidth = 30}, partBottom = HorizText {attr = Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default, attrURL = Default}, displayText = \"not?                          \", outputWidth = 30, charWidth = 30}, outputWidth = 30, outputHeight = 2}, outputWidth = 30, outputHeight = 3}, partBottom = BGFill {outputWidth = 30, outputHeight = 7}, outputWidth = 30, outputHeight = 10}] ??"

main :: IO Bool
main =
  do 
      renderDisplay [myWidget]
      
      print $ show $ renderWidget [myWidget] region

      return $
        show (renderWidget [myWidget] region) == renderedMyWidget
