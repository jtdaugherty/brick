module Brick where

import Control.Monad.IO.Class
import Data.String
import System.Exit
import Graphics.Vty

newtype Location = Location (Int, Int)

newtype Name = Name String
               deriving (Eq, Show)

data CursorLocation =
    CursorLocation { cursorLocation :: Location
                   , cursorLocationName :: Maybe Name
                   }

locOffset :: Location -> Location -> Location
locOffset (Location (w1, h1)) (Location (w2, h2)) = Location (w1+w2, h1+h2)

clOffset :: CursorLocation -> Location -> CursorLocation
clOffset cl loc = cl { cursorLocation = (cursorLocation cl) `locOffset` loc }

data Widget =
    Widget { render :: Location -> DisplayRegion -> Attr -> (Image, [CursorLocation])
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
                newLoc = loc `locOffset` Location (0, imageHeight img)
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
                newLoc = loc `locOffset` Location (imageWidth img, 0)
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

render_ :: Widget -> Location -> DisplayRegion -> Attr -> (Image, [CursorLocation])
render_ w loc sz attr = (uncurry crop sz img, curs)
    where
        (img, curs) = render w loc sz attr

renderFinal :: Widget -> DisplayRegion -> ([CursorLocation] -> Maybe CursorLocation) -> Picture
renderFinal widget sz chooseCursor = pic
    where
        pic = basePic { picCursor = cursor }
        basePic = picForImage $ uncurry resize sz img
        cursor = case chooseCursor curs of
                   Nothing -> NoCursor
                   Just cl -> let Location (w, h) = cursorLocation cl
                              in Cursor w h
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

withNamedCursor :: Widget -> (Name, Location) -> Widget
withNamedCursor w (name, cursorLoc) =
    w { render = \loc sz a -> let (img, _) = render_ w loc sz a
                              in (img, [CursorLocation (cursorLoc `locOffset` loc) (Just name)])
      }

withCursor :: Widget -> Location -> Widget
withCursor w cursorLoc =
    w { render = \loc sz a -> let (img, _) = render_ w loc sz a
                              in (img, [CursorLocation (cursorLoc `locOffset` loc) Nothing])
      }

runVty :: (MonadIO m)
       => (a -> Widget)
       -> (a -> [CursorLocation] -> Maybe CursorLocation)
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
