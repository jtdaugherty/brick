{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.String
import Graphics.Vty
import System.Exit

data Widget =
    Widget { render :: DisplayRegion -> Attr -> Image
           }

txt :: String -> Widget
txt s = Widget { render = const $ flip string s
               }

instance IsString Widget where
    fromString = txt

hBorder :: Char -> Widget
hBorder ch =
    Widget { render = \(width, _) attr -> charFill attr ch width 1
           }

vBorder :: Char -> Widget
vBorder ch =
    Widget { render = \(_, height) attr -> charFill attr ch 1 height
           }

vBox :: [Widget] -> Widget
vBox ws =
    Widget { render = renderVBox
           }
    where
        renderVBox (width, height) attr =
            vertCat $ snd $ foldr (doIt attr width) (height, []) ws

        doIt attr width w (hRemaining, imgs)
          | hRemaining <= 0 = (0, imgs)
          | otherwise =
            let newHeight = hRemaining - imageHeight img
                img = render w (width, hRemaining) attr
            in (newHeight, img : imgs)

hBox :: [Widget] -> Widget
hBox ws =
    Widget { render = renderHBox
           }
    where
        renderHBox (width, height) attr =
            horizCat $ snd $ foldr (doIt attr height) (width, []) ws

        doIt attr height w (wRemaining, imgs)
          | wRemaining <= 0 = (0, imgs)
          | otherwise =
            let newWidth = wRemaining - imageWidth img
                img = render w (wRemaining, height) attr
            in (newWidth, img : imgs)

hLimit :: Int -> Widget -> Widget
hLimit width w =
    Widget { render = \(_, height) attr -> render_ w (width, height) attr
           }

vLimit :: Int -> Widget -> Widget
vLimit height w =
    Widget { render = \(width, _) attr -> render_ w (width, height) attr
           }

render_ :: Widget -> DisplayRegion -> Attr -> Image
render_ w sz attr = uncurry crop sz $ render w sz attr

renderFinal :: Widget -> DisplayRegion -> Picture
renderFinal w sz = picForImage $ uncurry resize sz $ render_ w sz defAttr

liftVty :: Image -> Widget
liftVty = Widget . const . const

on :: Color -> Color -> Attr
on f b = defAttr `withForeColor` f
                 `withBackColor` b

fg :: Color -> Attr
fg = (defAttr `withForeColor`)

bg :: Color -> Attr
bg = (defAttr `withBackColor`)

withAttr :: Widget -> Attr -> Widget
withAttr w attr =
    Widget { render = \sz _ -> render_ w sz attr
           }

drawUI :: () -> Widget
drawUI _ =
    vBox [ "-- header --" `withAttr` (fg red)
         , vLimit 25 $ hBox [ hLimit 25 $ vBox [ "foo bar stuff things!"
                                               , hBorder '-'
                                               , "more things"
                                               ]
                            , vBorder '|' `withAttr` (yellow `on` black)
                            , liftVty $ string (fg green) "on the right"
                            ]
         ]

handleEvent :: Event -> () -> Either ExitCode ()
handleEvent e _ =
    case e of
        EvKey (KChar 'q') [] -> Left ExitSuccess
        _ -> Right ()

runVty :: (MonadIO m) => (a -> Widget) -> (Event -> a -> Either ExitCode a) -> a -> Vty -> m ()
runVty draw handleEv state vty = do
    e <- liftIO $ do
        sz <- displayBounds $ outputIface vty
        update vty $ renderFinal (draw state) sz
        nextEvent vty
    case handleEv e state of
        Left status -> liftIO $ do
            shutdown vty
            exitWith status
        Right newState -> runVty draw handleEv newState vty

main :: IO ()
main = standardIOConfig
       >>= mkVty
       >>= runVty drawUI handleEvent ()
