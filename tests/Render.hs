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

renderToPicture :: Ord n => [Widget n] -> V.Picture
renderToPicture ws = let (p, _, _) = renderWidget (attrMap V.defAttr []) ws region (const Nothing) in p

renderDisplay :: Ord n => [Widget n] -> IO ()
renderDisplay ws = do
  outp <- V.outputForConfig V.defaultConfig
  ctx <- V.displayContext outp region
  V.outputPicture ctx (renderToPicture ws)

ui :: Widget ()
ui = str "Hello, world!"

main :: IO Bool
main =
  do 
      renderDisplay [ui]
      renderDisplay @() [str "why" <=> hBorder <=> str "not"]
      return True
