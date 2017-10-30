{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Support for representing attribute themes and loading and saving
-- theme customizations in INI-style files.
--
-- The file format is as follows:
-- * Customization files are INI-style files with two sections, both
--   optional: "default" and "custom".
-- * The "default" section specifies three optional fields:
--   * "default.fg" - a color specification
--   * "default.bg" - a color specification
--   * "default.style" - a style specification
-- * A color specification can be any of the values (no quotes)
--   "black", "red", "green", "yellow", "blue", "magenta", "cyan",
--   "white", "brightBlack", "brightRed", "brightGreen", "brightYellow",
--   "brightBlue", "brightMagenta", "brightCyan", or "brightWhite".
-- * A style specification can be either one of the following values
--   (without quotes) or a comma-delimited list of one or more of the
--   following values (e.g. "[bold,underline]") indicating that all of
--   the specified styles be used.
--   * "standout"
--   * "underline"
--   * "reverseVideo"
--   * "blink"
--   * "dim"
--   * "bold"
-- * The "custom" section specifies for each attribute name in the theme
--   the same "fg", "bg", and "style" settings as for the default
--   attribute. Furthermore, if an attribute name has multiple
--   components, the fields in the INI file should use periods as
--   delimiters. For example, if a theme has an attribute name ("foo" <>
--   "bar"), then the file may specify three fields:
--   * "foo.bar.fg" - a color specification
--   * "foo.bar.bg" - a color specification
--   * "foo.bar.style" - a style specification
--
-- Any color or style specifications omitted from the file mean that
-- those attribute or style settings will use the theme's default value
-- instead.
module Brick.Themes
  ( CustomAttr(..)
  , customFgL
  , customBgL
  , customStyleL

  , Theme(..)
  , newTheme
  , themeDefaultAttrL
  , themeDefaultMappingL
  , themeCustomMappingL
  , themeCustomDefaultAttrL
  , themeDocumentationL

  , ThemeDocumentation(..)
  , themeDescriptionsL

  , themeToAttrMap
  , loadCustomizations
  , saveCustomizations
  )
where

import GHC.Generics (Generic)
import Graphics.Vty hiding ((<|>))
import Control.Monad (forM, join)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.List (intercalate)
import Data.Bits ((.|.), (.&.))
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

-- | Documentation for a theme's attributes.
data ThemeDocumentation =
    ThemeDocumentation { themeDescriptions :: M.Map AttrName T.Text
                       -- ^ The per-attribute documentation for a theme
                       -- so e.g. documentation for theme customization
                       -- can be generated mechanically.
                       }
                       deriving (Eq, Read, Show, Generic)

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
          , themeDefaultMapping :: M.Map AttrName Attr
          -- ^ The default attribute mapping to use.
          , themeCustomDefaultAttr :: Maybe CustomAttr
          -- ^ Customization for the theme's default attribute.
          , themeCustomMapping :: M.Map AttrName CustomAttr
          -- ^ Customizations for individual entries of the default
          -- mapping. Note that this will only affect entries in the
          -- default mapping; any attributes named here that are not
          -- present in the default mapping will not be considered.
          , themeDocumentation :: ThemeDocumentation
          -- ^ The documentation for the theme's attributes.
          }
          deriving (Eq, Read, Show, Generic)

suffixLenses ''CustomAttr
suffixLenses ''Theme
suffixLenses ''ThemeDocumentation

-- | Create a new theme with the specified default attribute and
-- attribute mapping. The theme will have no documentation or
-- customizations.
newTheme :: Attr -> [(AttrName, Attr)] -> Theme
newTheme def mapping =
    Theme { themeDefaultAttr       = def
          , themeDefaultMapping    = M.fromList mapping
          , themeCustomDefaultAttr = Nothing
          , themeCustomMapping     = mempty
          , themeDocumentation     = ThemeDocumentation mempty
          }

-- | Build an 'AttrMap' from a 'Theme'. This applies all customizations
-- in the returned 'AttrMap'.
themeToAttrMap :: Theme -> AttrMap
themeToAttrMap t =
    attrMap (customizeAttr (themeCustomDefaultAttr t) (themeDefaultAttr t)) customMap
    where
        customMap = F.foldr f [] (M.toList $ themeDefaultMapping t)
        f (aName, attr) mapping =
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
    let stripped = T.strip $ T.toLower s
    in if stripped == "default"
       then Right Default
       else maybe (Left $ "Invalid color: " <> show s) (Right . SetTo) $
                  lookup stripped (swap <$> allColors)

allColors :: [(Color, T.Text)]
allColors =
    [ (black, "black")
    , (red, "red")
    , (green, "green")
    , (yellow, "yellow")
    , (blue, "blue")
    , (magenta, "magenta")
    , (cyan, "cyan")
    , (white, "white")
    , (brightBlack, "brightBlack")
    , (brightRed, "brightRed")
    , (brightGreen, "brightGreen")
    , (brightYellow, "brightYellow")
    , (brightBlue, "brightBlue")
    , (brightMagenta, "brightMagenta")
    , (brightCyan, "brightCyan")
    , (brightWhite, "brightWhite")
    ]

allStyles :: [(T.Text, Style)]
allStyles =
    [ ("standout", standout)
    , ("underline", underline)
    , ("reverseVideo", reverseVideo)
    , ("blink", blink)
    , ("dim", dim)
    , ("bold", bold)
    ]

parseStyle :: T.Text -> Either String Style
parseStyle s =
    let lookupStyle n = case lookup n allStyles of
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

    defCustom <- sectionMb "default" $ do
        parseCustomAttr "default"

    customMap <- sectionMb "custom" $ do
        catMaybes <$> (forM (M.keys $ themeDefaultMapping t) $ \an ->
            (fmap (an,)) <$> parseCustomAttr (makeFieldName $ attrNameComponents an)
            )

    return (join defCustom, M.fromList $ fromMaybe [] customMap)

-- | Load an INI file containing theme customizations. Use the specified
-- theme to determine which customizations to load. Return the specified
-- theme with customizations set. See the module documentation for the
-- theme file format.
loadCustomizations :: FilePath -> Theme -> IO (Either String Theme)
loadCustomizations path t = do
    content <- T.readFile path
    case parseIniFile content (themeParser t) of
        Left e -> return $ Left e
        Right (customDef, customMap) ->
            return $ Right $ t { themeCustomDefaultAttr = customDef
                               , themeCustomMapping = customMap
                               }

vtyColorName :: Color -> T.Text
vtyColorName (Color240 _) = error "Color240 space not supported yet"
vtyColorName c =
    fromMaybe (error $ "Invalid color: " <> show c)
              (lookup c allColors)

makeFieldName :: [String] -> T.Text
makeFieldName cs = T.pack $ intercalate "." cs

serializeCustomColor :: [String] -> MaybeDefault Color -> T.Text
serializeCustomColor cs cc =
    let cName = case cc of
          Default -> "default"
          SetTo c -> vtyColorName c
          KeepCurrent -> error "serializeCustomColor does not support KeepCurrent"
    in makeFieldName cs <> " = " <> cName

serializeCustomStyle :: [String] -> Style -> T.Text
serializeCustomStyle cs s =
    let activeStyles = filter (\(_, a) -> a .&. s == a) allStyles
        styleStr = case activeStyles of
            [(single, _)] -> single
            many -> "[" <> (T.intercalate ", " $ fst <$> many) <> "]"
    in makeFieldName cs <> " = " <> styleStr

serializeCustomAttr :: [String] -> CustomAttr -> [T.Text]
serializeCustomAttr cs c =
    catMaybes [ serializeCustomColor (cs <> ["fg"]) <$> customFg c
              , serializeCustomColor (cs <> ["bg"]) <$> customBg c
              , serializeCustomStyle (cs <> ["style"]) <$> customStyle c
              ]

emitSection :: T.Text -> [T.Text] -> [T.Text]
emitSection _ [] = []
emitSection secName ls = secName : ls

-- | Save an INI file containing theme customizations. Use the specified
-- theme to determine which customizations to save. See the module
-- documentation for the theme file format.
saveCustomizations :: FilePath -> Theme -> IO ()
saveCustomizations path t = do
    let defSection = fromMaybe [] $
                     serializeCustomAttr ["default"] <$> themeCustomDefaultAttr t
        mapSection = concat $ flip map (M.keys $ themeDefaultMapping t) $ \an ->
            maybe [] (serializeCustomAttr (attrNameComponents an)) $
                     M.lookup an $ themeCustomMapping t
        content = T.unlines $ (emitSection "[default]" defSection) <>
                              (emitSection "[custom]" mapSection)
    T.writeFile path content
