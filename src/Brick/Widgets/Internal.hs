{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Widgets.Internal
  ( Result(..)
  , image
  , cursors
  , attr
  , ctxAttrs
  , lookupAttrName
  , visibilityRequests
  , addResultOffset

  , RenderState(..)
  , ScrollRequest(..)
  , Direction(..)

  , renderFinal
  , Widget(..)
  , Size(..)
  , RenderM

  , Context
  , availW
  , availH
  , getActiveBorderStyle
  , getContext

  , withBorderStyle

  , ViewportType(..)

  , txt
  , str
  , fill

  , padLeft
  , padRight
  , padTop
  , padBottom

  , emptyWidget
  , hBox
  , vBox
  , (<=>)
  , (<+>)

  , hLimit
  , vLimit
  , withDefaultAttr
  , withAttr
  , forceAttr
  , updateAttrMap
  , raw
  , translateBy
  , cropLeftBy
  , cropRightBy
  , cropTopBy
  , cropBottomBy
  , showCursor
  , viewport
  , visible
  , visibleRegion
  )
where

import Control.Applicative
import Control.Lens (makeLenses, (^.), (.~), (&), (%~), to, _1, _2, view, each, to, ix)
import Control.Monad (when)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Data.Default
import Data.Functor.Contravariant
import Data.Monoid ((<>), mempty)
import qualified Data.Map as M
import qualified Data.Function as DF
import Data.List (sortBy, partition)
import Control.Lens (Lens')
import Data.String (IsString(..))
import qualified Graphics.Vty as V

import Brick.Types
import Brick.Widgets.Border.Style
import Brick.Util (clOffset)
import Brick.AttrMap
import Brick.Util (clamp)

data VisibilityRequest =
    VR { _vrPosition :: Location
       , _vrSize :: V.DisplayRegion
       }
       deriving Show

data ViewportType = Vertical | Horizontal deriving Show

data Viewport =
    VP { _vpLeft :: Int
       , _vpTop :: Int
       , _vpSize :: V.DisplayRegion
       }
       deriving Show

data Result =
    Result { _image :: V.Image
           , _cursors :: [CursorLocation]
           , _visibilityRequests :: [VisibilityRequest]
           }
           deriving Show

data Context =
    Context { _ctxAttrName :: AttrName
            , _availW :: Int
            , _availH :: Int
            , _activeBorderStyle :: BorderStyle
            , _ctxAttrs :: AttrMap
            }

type RenderM a = ReaderT Context (State RenderState) a

data Size = Fixed
          | Unlimited
          deriving (Show, Eq, Ord)

data Widget =
    Widget { hSize :: Size
           , vSize :: Size
           , render :: RenderM Result
           }

data Direction = Up | Down

data ScrollRequest = ScrollBy Int
                   | ScrollPage Direction
                   | ScrollToBeginning
                   | ScrollToEnd

data RenderState =
    RS { _viewportMap :: M.Map Name Viewport
       , _scrollRequests :: [(Name, ScrollRequest)]
       }

makeLenses ''Result
makeLenses ''Context
makeLenses ''VisibilityRequest
makeLenses ''Viewport
makeLenses ''RenderState

instance IsString Widget where
    fromString = str

instance Default Result where
    def = Result V.emptyImage [] []

getContext :: RenderM Context
getContext = ask

withBorderStyle :: BorderStyle -> Widget -> Widget
withBorderStyle bs p = Widget (hSize p) (vSize p) $ withReaderT (& activeBorderStyle .~ bs) (render p)

getActiveBorderStyle :: RenderM BorderStyle
getActiveBorderStyle = view activeBorderStyle

emptyWidget :: Widget
emptyWidget = raw V.emptyImage

renderFinal :: AttrMap
            -> [Widget]
            -> V.DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> RenderState
            -> (RenderState, V.Picture, Maybe CursorLocation)
renderFinal aMap layerRenders sz chooseCursor rs = (newRS, pic, theCursor)
    where
        (layerResults, newRS) = flip runState rs $ sequence $
            (\p -> runReaderT p ctx) <$>
            (render <$> cropToContext <$> layerRenders)
        ctx = Context def (fst sz) (snd sz) def aMap
        pic = V.picForLayers $ uncurry V.resize sz <$> (^.image) <$> layerResults
        layerCursors = (^.cursors) <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

addResultOffset :: Location -> Result -> Result
addResultOffset off = addCursorOffset off . addVisibilityOffset off

addVisibilityOffset :: Location -> Result -> Result
addVisibilityOffset off r = r & visibilityRequests.each.vrPosition %~ (off <>)

addCursorOffset :: Location -> Result -> Result
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible loc = loc^.column >= 0 && loc^.row >= 0
    in r & cursors %~ (\cs -> onlyVisible $ (`clOffset` off) <$> cs)

unrestricted :: Int
unrestricted = 100000

attr :: (Contravariant f, Functor f) => (V.Attr -> f V.Attr) -> Context -> f Context
attr = to (\c -> attrMapLookup (c^.ctxAttrName) (c^.ctxAttrs))

lookupAttrName :: AttrName -> RenderM V.Attr
lookupAttrName n = do
    c <- getContext
    return $ attrMapLookup n (c^.ctxAttrs)

str :: String -> Widget
str s =
    Widget Fixed Fixed $ do
      c <- getContext
      let theLines = lines s
          fixEmpty [] = " "
          fixEmpty l = l
      case fixEmpty <$> theLines of
          -- The empty string case is important since we often need
          -- empty strings to have non-zero height!  This comes down to Vty's
          -- behavior of empty strings (they have imageHeight 1)
          [] -> return $ def & image .~ (V.string (c^.attr) "")
          [one] -> return $ def & image .~ (V.string (c^.attr) one)
          multiple ->
              let maxLength = maximum $ length <$> multiple
                  lineImgs = lineImg <$> multiple
                  lineImg lStr = V.string (c^.attr) (lStr ++ replicate (maxLength - length lStr) ' ')
              in return $ def & image .~ (V.vertCat lineImgs)

txt :: T.Text -> Widget
txt = str . T.unpack

padLeft :: Widget -> Widget
padLeft p =
    Widget Unlimited (vSize p) $ do
        result <- render p
        render $ (vLimit (result^.image.to V.imageHeight) $ fill ' ') <+> (Widget Fixed Fixed $ return result)

padRight :: Widget -> Widget
padRight p =
    Widget Unlimited (vSize p) $ do
        result <- render p
        render $ (Widget Fixed Fixed $ return result) <+> (vLimit (result^.image.to V.imageHeight) $ fill ' ')

padTop :: Widget -> Widget
padTop p =
    Widget (hSize p) Unlimited $ do
        result <- render p
        render $ (hLimit (result^.image.to V.imageWidth) $ fill ' ') <=> (Widget Fixed Fixed $ return result)

padBottom :: Widget -> Widget
padBottom p =
    Widget (hSize p) Unlimited $ do
        result <- render p
        render $ (Widget Fixed Fixed $ return result) <=> (hLimit (result^.image.to V.imageWidth) $ fill ' ')

fill :: Char -> Widget
fill ch =
    Widget Unlimited Unlimited $ do
      c <- getContext
      return $ def & image .~ (V.charFill (c^.attr) ch (c^.availW) (c^.availH))

vBox :: [Widget] -> Widget
vBox [] = emptyWidget
vBox pairs = renderBox vBoxRenderer pairs

hBox :: [Widget] -> Widget
hBox [] = emptyWidget
hBox pairs = renderBox hBoxRenderer pairs

data BoxRenderer =
    BoxRenderer { contextPrimary :: Lens' Context Int
                , contextSecondary :: Lens' Context Int
                , imagePrimary :: V.Image -> Int
                , limitPrimary :: Int -> Widget -> Widget
                , limitSecondary :: Int -> Widget -> Widget
                , primarySize :: Widget -> Size
                , concatenate :: [V.Image] -> V.Image
                , locationFromOffset :: Int -> Location
                }

vBoxRenderer :: BoxRenderer
vBoxRenderer = BoxRenderer availH availW V.imageHeight vLimit hLimit vSize V.vertCat (Location . (0 ,))

hBoxRenderer :: BoxRenderer
hBoxRenderer = BoxRenderer availW availH V.imageWidth hLimit vLimit hSize V.horizCat (Location . (, 0))

renderBox :: BoxRenderer -> [Widget] -> Widget
renderBox br ws = do
    Widget (maximum $ hSize <$> ws) (maximum $ vSize <$> ws) $ do
      c <- getContext

      let pairsIndexed = zip [(0::Int)..] ws
          (his, lows) = partition (\p -> (primarySize br $ snd p) == Fixed) pairsIndexed

      renderedHis <- mapM (\(i, prim) -> (i,) <$> render prim) his

      renderedLows <- case lows of
          [] -> return []
          ls -> do
              let remainingPrimary = c^.(contextPrimary br) - (sum $ (^._2.image.(to $ imagePrimary br)) <$> renderedHis)
                  primaryPerLow = remainingPrimary `div` length ls
                  padFirst = remainingPrimary - (primaryPerLow * length ls)
                  secondaryPerLow = c^.(contextSecondary br)
                  primaries = replicate (length ls) primaryPerLow & ix 0 %~ (+ padFirst)

              let renderLow ((i, prim), pri) =
                      (i,) <$> (render $ limitPrimary br pri
                                       $ limitSecondary br secondaryPerLow
                                       $ cropToContext prim)

              if remainingPrimary > 0 then mapM renderLow (zip ls primaries) else return []

      let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
          allResults = snd <$> rendered
          allImages = (^.image) <$> allResults
          allPrimaries = imagePrimary br <$> allImages
          allTranslatedResults = (flip map) (zip [0..] allResults) $ \(i, result) ->
              let off = locationFromOffset br offPrimary
                  offPrimary = sum $ take i allPrimaries
              in addResultOffset off result

      cropResultToContext $ Result (concatenate br allImages)
                            (concat $ _cursors <$> allTranslatedResults)
                            (concat $ _visibilityRequests <$> allTranslatedResults)

-- xxx crop cursors and VRs
hLimit :: Int -> Widget -> Widget
hLimit w p =
    Widget Fixed (vSize p) $ do
      withReaderT (& availW .~ w) $ render $ cropToContext p

-- xxx crop cursors and VRs
vLimit :: Int -> Widget -> Widget
vLimit h p =
    Widget (hSize p) Fixed $ do
      withReaderT (& availH .~ h) $ render $ cropToContext p

withAttr :: AttrName -> Widget -> Widget
withAttr an p =
    Widget (hSize p) (vSize p) $ do
      withReaderT (& ctxAttrName .~ an) (render p)

withDefaultAttr :: AttrName -> Widget -> Widget
withDefaultAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (& ctxAttrs %~ (setDefault (attrMapLookup an (c^.ctxAttrs)))) (render p)

updateAttrMap :: (AttrMap -> AttrMap) -> Widget -> Widget
updateAttrMap f p =
    Widget (hSize p) (vSize p) $ do
        withReaderT (& ctxAttrs %~ f) (render p)

forceAttr :: AttrName -> Widget -> Widget
forceAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (& ctxAttrs .~ (forceAttrMap (attrMapLookup an (c^.ctxAttrs)))) (render p)

raw :: V.Image -> Widget
raw img = Widget Fixed Fixed $ return $ def & image .~ img

translateBy :: Location -> Widget -> Widget
translateBy loc p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      return $ addResultOffset loc
             $ result & image %~ (V.translate (loc^.column) (loc^.row))

cropResultToContext :: Result -> RenderM Result
cropResultToContext result = do
    c <- getContext
    return $ result & image %~ (V.crop (c^.availW) (c^.availH))

cropToContext :: Widget -> Widget
cropToContext p =
    -- XXX should this be fixed/fixed? Seems like no, because these are
    -- about how it fills space given regardless of cropping. This same
    -- consideration applies to the other crop functions.
    Widget (hSize p) (vSize p) $ (render p >>= cropResultToContext)

cropLeftBy :: Int -> Widget -> Widget
cropLeftBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.image) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropLeft amt img
      return $ addResultOffset (Location (-1 * cols, 0))
             $ result & image %~ cropped

cropRightBy :: Int -> Widget -> Widget
cropRightBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.image) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropRight amt img
      -- xxx cursors / VRs
      return $ result & image %~ cropped

cropTopBy :: Int -> Widget -> Widget
cropTopBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.image) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropTop amt img
      return $ addResultOffset (Location (0, -1 * rows))
             $ result & image %~ cropped

cropBottomBy :: Int -> Widget -> Widget
cropBottomBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.image) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropBottom amt img
      -- xxx crop cursors / VRs
      return $ result & image %~ cropped

showCursor :: Name -> Location -> Widget -> Widget
showCursor n cloc p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      return $ result & cursors %~ (CursorLocation cloc (Just n):)

hRelease :: Widget -> Maybe Widget
hRelease p =
    case hSize p of
        Fixed -> Just $ Widget Unlimited (vSize p) $ withReaderT (& availW .~ unrestricted) (render p)
        Unlimited -> Nothing

vRelease :: Widget -> Maybe Widget
vRelease p =
    case vSize p of
        Fixed -> Just $ Widget (hSize p) Unlimited $ withReaderT (& availH .~ unrestricted) (render p)
        Unlimited -> Nothing

viewport :: Name -> ViewportType -> Widget -> Widget
viewport vpname typ p =
    Widget Unlimited Unlimited $ do
      -- First, update the viewport size.
      c <- getContext
      let newVp = VP 0 0 newSize
          newSize = (c^.availW, c^.availH)
          doInsert (Just vp) = Just $ vp & vpSize .~ newSize
          doInsert Nothing = Just newVp

      lift $ modify (& viewportMap %~ (M.alter doInsert vpname))

      -- Then render the sub-rendering with the rendering layout
      -- constraint released (but raise an exception if we are asked to
      -- render an infinitely-sized widget in the viewport's scrolling
      -- dimension)
      let Name vpn = vpname
          release = case typ of
            Vertical -> vRelease
            Horizontal -> hRelease
          released = case release p of
            Just w -> w
            Nothing -> case typ of
                Vertical -> error $ "tried to embed an infinite-height widget in vertical viewport " <> (show vpn)
                Horizontal -> error $ "tried to embed an infinite-width widget in horizontal viewport " <> (show vpn)

      initialResult <- render released

      -- If the sub-rendering requested visibility, update the scroll
      -- state accordingly
      when (not $ null $ initialResult^.visibilityRequests) $ do
          Just vp <- lift $ gets $ (^.viewportMap.to (M.lookup vpname))
          -- XXX for now, just permit one request but we could permit
          -- many by computing the bounding rectangle over the submitted
          -- requests.
          let [rq] = initialResult^.visibilityRequests
              updatedVp = scrollToView typ rq vp
          lift $ modify (& viewportMap %~ (M.insert vpname updatedVp))

      -- If the rendering state includes any scrolling requests for this
      -- viewport, apply those
      reqs <- lift $ gets $ (^.scrollRequests)
      let relevantRequests = snd <$> filter (\(n, _) -> n == vpname) reqs
      when (not $ null relevantRequests) $ do
          Just vp <- lift $ gets $ (^.viewportMap.to (M.lookup vpname))
          let [rq] = relevantRequests
              updatedVp = scrollTo typ rq (initialResult^.image) vp
          lift $ modify (& viewportMap %~ (M.insert vpname updatedVp))
          return ()

      -- Get the viewport state now that it has been updated.
      Just vp <- lift $ gets (M.lookup vpname . (^.viewportMap))

      -- Then perform a translation of the sub-rendering to fit into the
      -- viewport
      translated <- render $ translateBy (Location (-1 * vp^.vpLeft, -1 * vp^.vpTop))
                           $ Widget Fixed Fixed $ return initialResult

      -- Return the translated result with the visibility requests
      -- discarded
      let translatedSize = ( translated^.image.to V.imageWidth
                           , translated^.image.to V.imageHeight
                           )
      case translatedSize of
          (0, 0) -> return $ translated & image .~ (V.charFill (c^.attr) ' ' (c^.availW) (c^.availH))
                                        & visibilityRequests .~ mempty
          _ -> render $ cropToContext
                      $ padBottom
                      $ padRight
                      $ Widget Fixed Fixed $ return $ translated & visibilityRequests .~ mempty

scrollTo :: ViewportType -> ScrollRequest -> V.Image -> Viewport -> Viewport
scrollTo typ req img vp = vp & theStart .~ newStart
    where
        theStart :: Lens' Viewport Int
        theStart = case typ of
            Horizontal -> vpLeft
            Vertical -> vpTop
        theSize = case typ of
            Horizontal -> vpSize._1
            Vertical -> vpSize._2
        imgSize = case typ of
            Horizontal -> V.imageWidth img
            Vertical -> V.imageHeight img

        newStart = clamp 0 (imgSize - vp^.theSize) adjustedAmt
        adjustedAmt = case req of
            ScrollBy amt -> (vp^.theStart) + amt
            ScrollPage Up -> (vp^.theStart) - (vp^.theSize)
            ScrollPage Down -> (vp^.theStart) + (vp^.theSize)
            ScrollToBeginning -> 0
            ScrollToEnd -> imgSize - (vp^.theSize)

scrollToView :: ViewportType -> VisibilityRequest -> Viewport -> Viewport
scrollToView typ rq vp = vp & theStart .~ newStart
    where
        theStart :: Lens' Viewport Int
        theStart = case typ of
            Horizontal -> vpLeft
            Vertical -> vpTop
        theSize = case typ of
            Horizontal -> vpSize._1
            Vertical -> vpSize._2
        reqStart = case typ of
            Horizontal -> rq^.vrPosition.column
            Vertical -> rq^.vrPosition.row
        reqSize = case typ of
            Horizontal -> rq^.vrSize._1
            Vertical -> rq^.vrSize._2

        curStart = vp^.theStart
        curEnd = curStart + vp^.theSize

        reqEnd = reqStart + reqSize
        newStart :: Int
        newStart = if reqStart < curStart
                   then reqStart
                   else if reqStart > curEnd || reqEnd > curEnd
                        then reqEnd - vp^.theSize
                        else curStart

visible :: Widget -> Widget
visible p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let imageSize = ( result^.image.to V.imageWidth
                      , result^.image.to V.imageHeight
                      )
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if imageSize^._1 > 0 && imageSize^._2 > 0
               then result & visibilityRequests %~ (VR (Location (0, 0)) imageSize :)
               else result

visibleRegion :: Location -> V.DisplayRegion -> Widget -> Widget
visibleRegion vrloc sz p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if sz^._1 > 0 && sz^._2 > 0
               then result & visibilityRequests %~ (VR vrloc sz :)
               else result

(<+>) :: Widget -> Widget -> Widget
(<+>) a b = hBox [a, b]

(<=>) :: Widget -> Widget -> Widget
(<=>) a b = vBox [a, b]
