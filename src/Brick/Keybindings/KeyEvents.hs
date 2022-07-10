module Brick.Keybindings.KeyEvents
  ( KeyEvents
  , keyEvents
  , keyEventsList
  , lookupKeyEvent
  , keyEventName
  )
where

import qualified Data.Bimap as B
import qualified Data.Text as T

data KeyEvents e = KeyEvents (B.Bimap T.Text e)
                 deriving (Eq, Show)

keyEvents :: (Ord e) => [(T.Text, e)] -> KeyEvents e
keyEvents pairs = KeyEvents $ B.fromList pairs

keyEventsList :: KeyEvents e -> [(T.Text, e)]
keyEventsList (KeyEvents m) = B.toList m

lookupKeyEvent :: (Ord e) => KeyEvents e -> T.Text -> Maybe e
lookupKeyEvent (KeyEvents m) name = B.lookup name m

keyEventName :: (Ord e) => KeyEvents e -> e -> Maybe T.Text
keyEventName (KeyEvents m) e = B.lookupR e m
