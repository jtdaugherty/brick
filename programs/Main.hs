{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.String
import Data.Maybe
import Graphics.Vty
import System.Exit

newtype Location = Location (Int, Int)

offset :: Location -> Location -> Location
offset (Location (w1, h1)) (Location (w2, h2)) = Location (w1+w2, h1+h2)

data Widget =
    Widget { render :: Location -> DisplayRegion -> Attr -> (Image, [Location])
           }

txt :: String -> Widget
txt s = Widget { render = \_ _ a -> ( string a s
                                    , []
                                    )
               }

instance IsString Widget where
    fromString = txt

hBorder :: Char -> Widget
hBorder ch =
    Widget { render = \_ (width, _) attr -> ( charFill attr ch width 1
                                            , []
                                            )
           }

vBorder :: Char -> Widget
vBorder ch =
    Widget { render = \_ (_, height) attr -> ( charFill attr ch 1 height
                                             , []
                                             )
           }

vBox :: [Widget] -> Widget
vBox widgets =
    Widget { render = renderVBox
           }
    where
        renderVBox loc (width, height) attr =
            let (imgs, curs) = doIt attr width widgets height loc
            in (vertCat imgs, curs)

        doIt _ _ [] _ _ = ([], [])
        doIt attr width (w:ws) hRemaining loc
          | hRemaining <= 0 = ([], [])
          | otherwise =
            let newHeight = hRemaining - imageHeight img
                (img, curs') = render w loc (width, hRemaining) attr
                newLoc = loc `offset` Location (0, imageHeight img)
                (restImgs, restCurs) = doIt attr width ws newHeight newLoc
            in (img:restImgs, curs'++restCurs)

hBox :: [Widget] -> Widget
hBox widgets =
    Widget { render = renderHBox
           }
    where
        renderHBox loc (width, height) attr =
            let (imgs, curs) = doIt attr height widgets width loc
            in (horizCat imgs, curs)

        doIt _ _ [] _ _ = ([], [])
        doIt attr height (w:ws) wRemaining loc
          | wRemaining <= 0 = ([], [])
          | otherwise =
            let newWidth = wRemaining - imageWidth img
                (img, curs') = render w loc (wRemaining, height) attr
                newLoc = loc `offset` Location (imageWidth img, 0)
                (restImgs, restCurs) = doIt attr height ws newWidth newLoc
            in (img:restImgs, curs'++restCurs)

hLimit :: Int -> Widget -> Widget
hLimit width w =
    Widget { render = \loc (_, height) attr -> render_ w loc (width, height) attr
           }

vLimit :: Int -> Widget -> Widget
vLimit height w =
    Widget { render = \loc (width, _) attr -> render_ w loc (width, height) attr
           }

render_ :: Widget -> Location -> DisplayRegion -> Attr -> (Image, [Location])
render_ w loc sz attr = (uncurry crop sz img, curs)
    where
        (img, curs) = render w loc sz attr

renderFinal :: Widget -> DisplayRegion -> ([Location] -> Maybe Location) -> Picture
renderFinal widget sz chooseCursor = pic
    where
        pic = basePic { picCursor = cursor }
        basePic = picForImage $ uncurry resize sz img
        cursor = case chooseCursor curs of
                   Nothing -> NoCursor
                   Just (Location (w, h)) -> Cursor w h
        (img, curs) = render_ widget (Location (0, 0)) sz defAttr

liftVty :: Image -> Widget
liftVty img =
    Widget { render = const $ const $ const (img, [])
           }

on :: Color -> Color -> Attr
on f b = defAttr `withForeColor` f
                 `withBackColor` b

fg :: Color -> Attr
fg = (defAttr `withForeColor`)

bg :: Color -> Attr
bg = (defAttr `withBackColor`)

withAttr :: Widget -> Attr -> Widget
withAttr w attr =
    Widget { render = \loc sz _ -> render_ w loc sz attr
           }

withCursor :: Widget -> Location -> Widget
withCursor w cursorLoc =
    w { render = \loc sz a -> let (img, _) = render_ w loc sz a
                              in (img, [loc `offset` cursorLoc])
      }

drawUI :: () -> Widget
drawUI _ =
    hBox [ vBox [ "-- header 1 --"
                , "-- header 2 --"
                ] `withAttr` (fg red)
         , vBorder '|'
         , "stuff things" `withCursor` (Location (0, 0))
         ]

handleEvent :: Event -> () -> Either ExitCode ()
handleEvent e _ =
    case e of
        EvKey (KChar 'q') [] -> Left ExitSuccess
        _ -> Right ()

runVty :: (MonadIO m)
       => (a -> Widget)
       -> (a -> [Location] -> Maybe Location)
       -> (Event -> a -> Either ExitCode a)
       -> a
       -> Vty
       -> m ()
runVty draw chooseCursor handleEv state vty = do
    e <- liftIO $ do
        sz <- displayBounds $ outputIface vty
        update vty $ renderFinal (draw state) sz (chooseCursor state)
        nextEvent vty
    case handleEv e state of
        Left status -> liftIO $ do
            shutdown vty
            exitWith status
        Right newState -> runVty draw chooseCursor handleEv newState vty

pickCursor :: () -> [Location] -> Maybe Location
pickCursor = const $ listToMaybe

main :: IO ()
main = standardIOConfig
       >>= mkVty
       >>= runVty drawUI pickCursor handleEvent ()
