{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Themes
  ( CustomAttr(..)
  , customFgL
  , customBgL
  , customStyleL

  , Theme(..)
  , themeDefaultAttrL
  , themeDefaultMappingL
  , themeCustomMappingL
  , themeCustomDefaultAttrL

  , themeToAttrMap
  , loadCustomizations
  )
where

import GHC.Generics (Generic)
import Graphics.Vty hiding ((<|>))
import Control.Monad (forM)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Bits ((.|.))
import Data.Maybe (fromMaybe, isNothing, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Foldable as F

import Data.Ini.Config

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrNameComponents)
import Brick.Types.TH (suffixLenses)

-- | An attribute customization can specify which aspects of an
-- attribute to customize.
data CustomAttr =
    CustomAttr { customFg    :: Maybe (MaybeDefault Color)
               -- ^ The customized foreground, if any.
               , customBg    :: Maybe (MaybeDefault Color)
               -- ^ The customized background, if any.
               , customStyle :: Maybe Style
               -- ^ The customized style, if any.
               }
               deriving (Eq, Read, Show, Generic)

instance Monoid CustomAttr where
    mempty = CustomAttr Nothing Nothing Nothing
    mappend a b =
        CustomAttr { customFg    = customFg a    <|> customFg b
                   , customBg    = customBg a    <|> customBg b
                   , customStyle = customStyle a <|> customStyle b
                   }

-- | A theme provides a set of default attribute mappings, a default
-- attribute, and a set of customizations for the default mapping
-- and default attribute. The idea here is that the application will
-- always need to provide a complete specification of its attribute
-- mapping, but if the user wants to customize any aspect of that
-- default mapping, it can be contained here and then built into an
-- 'AttrMap' (see 'themeToAttrMap'). We keep the defaults separate
-- from customizations to permit users to serialize themes and their
-- customizations to, say, disk files.
data Theme =
    Theme { themeDefaultAttr :: Attr
          -- ^ The default attribute to use.
          , themeCustomDefaultAttr :: Maybe CustomAttr
          -- ^ Customization for the theme's default attribute.
          , themeDefaultMapping :: M.Map AttrName (Attr, T.Text)
          -- ^ The default attribute mapping to use. This maps attribute
          -- names to default attributes, but it also requires the
          -- author to provide human-readable documentation strings for
          -- each attribute describing what the attribute affects in the
          -- interface. This is to aid the generation of documentation
          -- for user customization of the theme.
          , themeCustomMapping :: M.Map AttrName CustomAttr
          -- ^ Customizations for individual entries of the default
          -- mapping. Note that this will only affect entries in the
          -- default mapping; any attributes named here that are not
          -- present in the default mapping will not be considered.
          }
          deriving (Eq, Read, Show, Generic)

suffixLenses ''CustomAttr
suffixLenses ''Theme

-- | Build an 'AttrMap' from a 'Theme'. This applies all customizations
-- in the returned 'AttrMap'.
themeToAttrMap :: Theme -> AttrMap
themeToAttrMap t =
    attrMap (customizeAttr (themeCustomDefaultAttr t) (themeDefaultAttr t)) customMap
    where
        customMap = F.foldr f [] (M.toList $ themeDefaultMapping t)
        f (aName, (attr, _)) mapping =
            let a' = customizeAttr (M.lookup aName (themeCustomMapping t)) attr
            in (aName, a'):mapping

customizeAttr :: Maybe CustomAttr -> Attr -> Attr
customizeAttr Nothing a = a
customizeAttr (Just c) a =
    let fg = fromMaybe (attrForeColor a) (customFg c)
        bg = fromMaybe (attrBackColor a) (customBg c)
        sty = maybe (attrStyle a) SetTo (customStyle c)
    in a { attrForeColor = fg
         , attrBackColor = bg
         , attrStyle = sty
         }

isNullCustomization :: CustomAttr -> Bool
isNullCustomization c =
    isNothing (customFg c) &&
    isNothing (customBg c) &&
    isNothing (customStyle c)

parseColor :: T.Text -> Either String (MaybeDefault Color)
parseColor s =
    let values = [ ("default", Default)
                 , ("black", SetTo black)
                 , ("red", SetTo red)
                 , ("green", SetTo green)
                 , ("yellow", SetTo yellow)
                 , ("blue", SetTo blue)
                 , ("magenta", SetTo magenta)
                 , ("cyan", SetTo cyan)
                 , ("white", SetTo white)
                 , ("brightBlack", SetTo brightBlack)
                 , ("brightRed", SetTo brightRed)
                 , ("brightGreen", SetTo brightGreen)
                 , ("brightYellow", SetTo brightYellow)
                 , ("brightBlue", SetTo brightBlue)
                 , ("brightMagenta", SetTo brightMagenta)
                 , ("brightCyan", SetTo brightCyan)
                 , ("brightWhite", SetTo brightWhite)
                 ]
        stripped = T.strip $ T.toLower s
    in maybe (Left $ "Invalid color: " <> show s) Right $
             lookup stripped values

parseStyle :: T.Text -> Either String Style
parseStyle s =
    let styles = [ ("standout", standout)
                 , ("underline", underline)
                 , ("reverseVideo", reverseVideo)
                 , ("blink", blink)
                 , ("dim", dim)
                 , ("bold", bold)
                 ]
        lookupStyle n = case lookup n styles of
            Just sty -> Right sty
            Nothing  -> Left $ T.unpack $ "Invalid style: " <> n
        stripped = T.strip $ T.toLower s
        bracketed = "[" `T.isPrefixOf` stripped &&
                    "]" `T.isSuffixOf` stripped
        unbracketed = T.tail $ T.init stripped
        parseStyleList = do
            ss <- mapM lookupStyle $ T.strip <$> T.splitOn "," unbracketed
            return $ foldr (.|.) 0 ss

    in if bracketed
       then parseStyleList
       else lookupStyle stripped

themeParser :: Theme -> IniParser (Maybe CustomAttr, M.Map AttrName CustomAttr)
themeParser t = do
    let parseCustomAttr basename = do
          c <- CustomAttr <$> fieldMbOf (basename <> ".fg")    parseColor
                          <*> fieldMbOf (basename <> ".bg")    parseColor
                          <*> fieldMbOf (basename <> ".style") parseStyle
          return $ if isNullCustomization c then Nothing else Just c

    defCustom <- section "default" $ do
        parseCustomAttr "default"

    customMap <- section "custom" $ do
        catMaybes <$> (forM (M.keys $ themeDefaultMapping t) $ \an ->
            (fmap (an,)) <$> parseCustomAttr (T.pack $ intercalate "." $ attrNameComponents an)
            )

    return (defCustom, M.fromList customMap)

loadCustomizations :: FilePath -> Theme -> IO (Either String Theme)
loadCustomizations path t = do
    content <- T.readFile path
    case parseIniFile content (themeParser t) of
        Left e -> return $ Left e
        Right (customDef, customMap) ->
            return $ Right $ t { themeCustomDefaultAttr = customDef
                               , themeCustomMapping = customMap
                               }
