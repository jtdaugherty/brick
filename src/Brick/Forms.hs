-- | TODO:
--
-- * make it so that each "field" can add more than one thing to the
--   focus ring, i.e. radio buttons
--
-- * think about how to make API simple but still permit more control
--   over rendering
--
-- * how to deal with variation in layout, e.g. button areas?
--
-- * think about how to let user control how to interpret and commit
--   changes to form state under various validation conditions
--
-- * think about form composition / concatenation

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Brick.Forms
  ( Form
  , formFocus
  , formState
  , FormFieldState(..)
  , FormField(..)

  -- * Working with forms
  , newForm
  , defaultFormRenderer
  , handleFormEvent
  , renderForm

  -- * Simple form field constructors
  , editShowableField
  , editPasswordField

  -- * Advanced form field constructors
  , editField

  -- * Attributes
  , invalidFormInputAttr
  )
where

import Graphics.Vty
import Data.Monoid
import Data.Maybe (isJust)

import Brick
import Brick.Focus
import Brick.Widgets.Edit

import qualified Data.Text as T
import Text.Read (readMaybe)

import Lens.Micro

data FormField a b e n =
    FormField { formFieldValidate    :: b -> Maybe a
              , formFieldRender      :: Bool -> b -> Widget n
              , formFieldHandleEvent :: BrickEvent n e -> b -> EventM n b
              }

data FormFieldState s e n where
    FormFieldState :: (Named b n) =>
                      { formFieldName  :: n
                      , formFieldState :: b
                      , formField      :: FormField a b e n
                      , formFieldLens  :: Lens' s a
                      } -> FormFieldState s e n

data Form s e n =
    Form { formFields       :: [FormFieldState s e n]
         , formFocus        :: FocusRing n
         , formState        :: s
         , formRenderHelper :: Form s e n -> n -> Widget n -> Widget n
         }

defaultFormRenderer :: (Show n) => Form s e n -> n -> Widget n -> Widget n
defaultFormRenderer f n w =
    let allNames = formFieldName <$> formFields f
        maxLength = 1 + (maximum $ length <$> show <$> allNames)
        p = maxLength - length (show n)
        label = padRight (Pad p) $ str $ show n <> ":"
    in label <+> w

newForm :: (Form s e n -> n -> Widget n -> Widget n)
        -> [s -> FormFieldState s e n]
        -> s
        -> Form s e n
newForm helper mkEs s =
    let es = mkEs <*> pure s
    in Form { formFields       = es
            , formFocus        = focusRing $ formFieldName <$> es
            , formState        = s
            , formRenderHelper = helper
            }

editField :: (Ord n, Show n)
          => Lens' s a
          -> n
          -> Maybe Int
          -> (a -> T.Text)
          -> ([T.Text] -> Maybe a)
          -> ([T.Text] -> Widget n)
          -> (Widget n -> Widget n)
          -> s
          -> FormFieldState s e n
editField stLens n limit ini val renderText wrapEditor initialState =
    let initVal = mk $ initialState ^. stLens
        mk = editor n limit . ini
        handleEvent (VtyEvent e) ed = handleEditorEvent e ed
        handleEvent _ ed = return ed

    in FormFieldState { formFieldName  = getName initVal
                      , formFieldState = initVal
                      , formField      = FormField (val . getEditContents)
                                                   (\b e -> wrapEditor $ renderEditor renderText b e)
                                                   handleEvent
                      , formFieldLens  = stLens
                      }

editShowableField :: (Ord n, Show n, Read a, Show a)
                  => Lens' s a
                  -> n
                  -> s
                  -> FormFieldState s e n
editShowableField stLens n =
    let ini = T.pack . show
        val = readMaybe . T.unpack . T.intercalate "\n"
        limit = Just 1
        renderText = txt . T.unlines
    in editField stLens n limit ini val renderText id

editPasswordField :: (Ord n, Show n)
                  => Lens' s T.Text
                  -> n
                  -> s
                  -> FormFieldState s e n
editPasswordField stLens n =
    let ini = id
        val = Just . T.concat
        limit = Just 1
        renderText = toPassword
    in editField stLens n limit ini val renderText id

toPassword :: [T.Text] -> Widget a
toPassword s = txt $ T.replicate (T.length $ T.concat s) "*"

formAttr :: AttrName
formAttr = "brickForm"

invalidFormInputAttr :: AttrName
invalidFormInputAttr = formAttr <> "invalidInput"

renderForm :: (Eq n) => Form s e n -> Widget n
renderForm f@(Form es fr _ helper) =
    vBox $ renderFormField (helper f) fr <$> es

renderFormField :: (Eq n)
                => (n -> Widget n -> Widget n)
                -> FocusRing n
                -> FormFieldState s e n
                -> Widget n
renderFormField helper fr (FormFieldState n st (FormField valid renderFunc _) _) =
    let maybeInvalid = if isJust $ valid st then id else forceAttr invalidFormInputAttr
        w = maybeInvalid (withFocusRing fr renderFunc st)
    in helper n w

handleFormEvent :: (Eq n) => BrickEvent n e -> Form s e n -> EventM n (Form s e n)
handleFormEvent (VtyEvent (EvKey (KChar '\t') [])) f =
    return $ f { formFocus = focusNext $ formFocus f }
handleFormEvent e f =
    case focusGetCurrent (formFocus f) of
        Nothing -> return f
        Just n  -> handleFormFieldEvent n e f

handleFormFieldEvent :: (Eq n) => n -> BrickEvent n e -> Form s e n -> EventM n (Form s e n)
handleFormFieldEvent n ev f = handle [] (formFields f)
    where
        handle _ [] = return f
        handle prev (e:es) =
            case e of
                FormFieldState n' st field@(FormField val _ handleFunc) stLens | n == n' -> do
                    nextSt <- handleFunc ev st
                    let newField = FormFieldState n' nextSt field stLens
                    -- If the new state validates, go ahead and update
                    -- the form state with it.
                    case val nextSt of
                        Nothing -> return $ f { formFields = prev <> [newField] <> es }
                        Just valid -> return $ f { formFields = prev <> [newField] <> es
                                                 , formState = formState f & stLens .~ valid
                                                 }

                _ -> handle (prev <> [e]) es

