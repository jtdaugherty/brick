module Data.Text.Markup
  ( Markup
  , markupToList
  , markupSet
  , fromList
  , fromText
  , toText
  , (@@)
  )
where

import Control.Applicative ((<$>))
import Data.Default (Default, def)
import Data.Monoid
import Data.String (IsString(..))
import qualified Data.Text as T

data Markup a = Markup [(Char, a)]
              deriving Show

instance Monoid (Markup a) where
    mempty = Markup mempty
    mappend (Markup t1) (Markup t2) =
        Markup (t1 `mappend` t2)

instance (Default a) => IsString (Markup a) where
    fromString = fromText . T.pack

(@@) :: T.Text -> a -> Markup a
t @@ val = Markup [(c, val) | c <- T.unpack t]

fromText :: (Default a) => T.Text -> Markup a
fromText = (@@ def)

toText :: (Eq a) => Markup a -> T.Text
toText = T.concat . (fst <$>) . markupToList

markupSet :: (Eq a) => (Int, Int) -> a -> Markup a -> Markup a
markupSet (start, len) val m@(Markup l) = if start < 0 || start + len > length l
                                          then m
                                          else newM
    where
        newM = Markup $ theHead ++ theNewEntries ++ theTail
        (theHead, theLongTail) = splitAt start l
        (theOldEntries, theTail) = splitAt len theLongTail
        theNewEntries = zip (fst <$> theOldEntries) (repeat val)

markupToList :: (Eq a) => Markup a -> [(T.Text, a)]
markupToList (Markup thePairs) = toList thePairs
    where
        toList [] = []
        toList ((ch, val):rest) = (T.pack $ ch : (fst <$> matching), val) : toList remaining
            where
                (matching, remaining) = break (\(_, v) -> v /= val) rest

fromList :: [(T.Text, a)] -> Markup a
fromList pairs = Markup $ concatMap (\(t, val) -> [(c, val) | c <- T.unpack t]) pairs
