module Data.Text.Markup
  ( RLE
  , Markup
  , toList
  , fromList
  , fromText
  , (@@)
  )
where

import Control.Applicative ((<$>))
import Control.Lens ((&), (%~), each, _1)
import Data.Default (Default, def)
import Data.Monoid
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Vector as V

data RLE a = RLE (V.Vector (Int, a))
           deriving Show

rle :: Int -> a -> RLE a
rle 0 _ = RLE V.empty
rle n val = RLE (V.singleton (n, val))

instance Monoid (RLE a) where
    mempty = RLE mempty
    mappend (RLE v1) (RLE v2) =
        -- XXX combine last element of v1 and first element of v2 if the
        -- 'a' values match, or build an "optimize" function that
        -- combines adjacent pairs where this condition is met and call
        -- it here
        RLE $ v1 `mappend` v2

data Markup a = Markup T.Text (RLE a)
              deriving Show

instance Monoid (Markup a) where
    mempty = Markup mempty mempty
    mappend (Markup t1 r1) (Markup t2 r2) =
        Markup (t1 `mappend` t2) (r1 `mappend` r2)

instance (Default a) => IsString (Markup a) where
    fromString = fromText . T.pack

(@@) :: T.Text -> a -> Markup a
t @@ val = Markup t (rle (T.length t) val)

fromText :: (Default a) => T.Text -> Markup a
fromText = (@@ def)

toList :: Markup a -> [(T.Text, a)]
toList (Markup theText (RLE pairs)) = toList' theText pairs
    where
        toList' _ vec | V.null vec = []
        toList' t vec =
            let (n, v) = V.head vec
                rest = V.tail vec
            in (T.take n t, v) : toList' (T.drop n t) rest

fromList :: [(T.Text, a)] -> Markup a
fromList pairs = Markup (T.concat $ fst <$> pairs) (RLE $ V.fromList rlePairs)
    where
        rlePairs = pairs & each._1 %~ T.length
