{-# LANGUAGE CPP #-}
module Render
  ( main
  )
where

import Brick
import Control.Monad (when)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform.Testing as V
import Brick.Widgets.Border (hBorder)
import Control.Exception (SomeException, try)

region :: V.DisplayRegion
region = (30, 10)

renderDisplay :: Ord n => [Widget n] -> IO ()
renderDisplay ws = do
    outp <- V.mkDefaultOutput
    ctx <- V.displayContext outp region
    V.outputPicture ctx (renderWidget Nothing ws region)
    V.releaseDisplay outp

myWidget :: Widget ()
myWidget = str "Why" <=> hBorder <=> str "not?"

-- Since you can't Read a Picture, we have to compare the result with
-- the Shown one
expectedResult :: String
expectedResult = "Picture {picCursor = NoCursor, picLayers = [VertJoin {partTop = VertJoin {partTop = HorizText {attr = Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default, attrURL = Default}, displayText = \"Why                           \", outputWidth = 30, charWidth = 30}, partBottom = VertJoin {partTop = HorizText {attr = Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default, attrURL = Default}, displayText = \"\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\\9472\", outputWidth = 30, charWidth = 30}, partBottom = HorizText {attr = Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default, attrURL = Default}, displayText = \"not?                          \", outputWidth = 30, charWidth = 30}, outputWidth = 30, outputHeight = 2}, outputWidth = 30, outputHeight = 3}, partBottom = BGFill {outputWidth = 30, outputHeight = 7}, outputWidth = 30, outputHeight = 10}], picBackground = Background {backgroundChar = ' ', backgroundAttr = Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default, attrURL = Default}}}"

main :: IO Bool
main = do
    result <- try (renderDisplay [myWidget]) :: IO (Either SomeException ())
    case result of
        Left _ -> do
            putStrLn "Terminal is not available, skipping test"
            -- Even though we could not actually run the test, we return
            -- True here to prevent the absence of a terminal from
            -- causing a test suite failure in an automated context.
            -- This means that this test effectively doesn't get
            -- considered at all in the automated context.
            return True
        Right () -> do
            let matched = actualResult == expectedResult
                actualResult = show (renderWidget Nothing [myWidget] region)
                msg = if matched then "rendering match" else "rendering mismatch"

            putStrLn ""
            putStrLn $ "renderWidget test outcome: " <> msg

            when (not matched) $ do
                putStrLn "Expected result:"
                putStrLn expectedResult

                putStrLn "Actual result:"
                putStrLn actualResult

            return matched
