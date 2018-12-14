-- | This module provides an API for "marking up" text with arbitrary
-- values. A piece of markup can then be converted to a list of pairs
-- representing the sequences of characters assigned the same markup
-- value.
--
-- This interface is experimental. Don't use this for your full-file
-- syntax highlighter just yet!
module Data.Text.Markup
  ( Markup
  , markupToList
  , markupSet
  , fromList
  , fromText
  , toText
  , isEmpty
  , (@@)
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid
#endif

import qualified Data.Semigroup as Sem
import Data.String (IsString(..))
import qualified Data.Text as T

-- | Markup with metadata type 'a' assigned to each character.
data Markup a = Markup [(Char, a)]
              deriving Show

instance Sem.Semigroup (Markup a) where
    (Markup t1) <> (Markup t2) = Markup (t1 `mappend` t2)

instance Monoid (Markup a) where
    mempty = Markup mempty
    mappend = (Sem.<>)

instance (Monoid a) => IsString (Markup a) where
    fromString = fromText . T.pack

-- | Build a piece of markup; assign the specified metadata to every
-- character in the specified text.
(@@) :: T.Text -> a -> Markup a
t @@ val = Markup [(c, val) | c <- T.unpack t]

-- | Build markup from text with the default metadata.
fromText :: (Monoid a) => T.Text -> Markup a
fromText = (@@ mempty)

-- | Extract the text from markup, discarding the markup metadata.
toText :: (Eq a) => Markup a -> T.Text
toText = T.concat . (fst <$>) . concat . markupToList

-- | Test whether the markup is empty.
isEmpty :: Markup a -> Bool
isEmpty (Markup ls) = null ls

-- | Set the metadata for a range of character positions in a piece of
-- markup. This is useful for, e.g., syntax highlighting.
markupSet :: (Eq a) => (Int, Int) -> a -> Markup a -> Markup a
markupSet (start, len) val m@(Markup l) = if start < 0 || start + len > length l
                                          then m
                                          else newM
    where
        newM = Markup $ theHead ++ theNewEntries ++ theTail
        (theHead, theLongTail) = splitAt start l
        (theOldEntries, theTail) = splitAt len theLongTail
        theNewEntries = zip (fst <$> theOldEntries) (repeat val)

-- | Convert markup to a list of lines. Each line is represented by a
-- list of pairs in which each pair contains the longest subsequence of
-- characters having the same metadata.
markupToList :: (Eq a) => Markup a -> [[(T.Text, a)]]
markupToList (Markup thePairs) = toList <$> toLines [] [] thePairs
    where
        toLines ls cur [] = ls ++ [cur]
        toLines ls cur ((ch, val):rest)
            | ch == '\n' = toLines (ls ++ [cur]) [] rest
            | otherwise = toLines ls (cur ++ [(ch, val)]) rest

        toList [] = []
        toList ((ch, val):rest) = (T.pack $ ch : (fst <$> matching), val) : toList remaining
            where
                (matching, remaining) = break (\(_, v) -> v /= val) rest

-- | Convert a list of text and metadata pairs into markup.
fromList :: [(T.Text, a)] -> Markup a
fromList pairs = Markup $ concatMap (\(t, val) -> [(c, val) | c <- T.unpack t]) pairs
