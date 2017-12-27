-- | TODO:
--
-- * think about how to make API simple but still permit more control
--   over rendering
--
-- * how to deal with variation in layout, e.g. button areas?
--
-- * think about how to let user control how to interpret and commit
--   changes to form state under various validation conditions
--
-- * Add mouse event generation and handling support!
--
-- * How to signal that event handling in a form is over? (e.g. Enter
--   is necessary for multi-lind editing, so we need something like a
--   button event)

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
  , handleFormEvent
  , renderForm
  , (@@=)

  -- * Simple form field constructors
  , editTextField
  , editShowableField
  , editPasswordField
  , radioField
  , checkboxField

  -- * Advanced form field constructors
  , editField

  -- * Attributes
  , invalidFormInputAttr
  , focusedFormInputAttr
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
    FormField { formFieldName        :: n
              , formFieldValidate    :: b -> Maybe a
              , formFieldRender      :: Bool -> b -> Widget n
              , formFieldHandleEvent :: BrickEvent n e -> b -> EventM n b
              }

data FormFieldState s e n where
    FormFieldState :: { formFieldState :: b
                      , formFieldLens  :: Lens' s a
                      , formFields     :: [FormField a b e n]
                      , formFieldRenderHelper :: Widget n -> Widget n
                      } -> FormFieldState s e n

data Form s e n =
    Form { formFieldStates  :: [FormFieldState s e n]
         , formFocus        :: FocusRing n
         , formState        :: s
         }

(@@=) :: (Widget n -> Widget n) -> (s -> FormFieldState s e n) -> s -> FormFieldState s e n
(@@=) h mkFs s = (mkFs s) { formFieldRenderHelper = h }

newForm :: [s -> FormFieldState s e n]
        -> s
        -> Form s e n
newForm mkEs s =
    let es = mkEs <*> pure s
    in Form { formFieldStates = es
            , formFocus       = focusRing $ concat $ formFieldNames <$> es
            , formState       = s
            }

formFieldNames :: FormFieldState s e n -> [n]
formFieldNames (FormFieldState _ _ fields _) = formFieldName <$> fields

checkboxField :: (Ord n, Show n)
              => Lens' s Bool
              -> n
              -> s
              -> FormFieldState s e n
checkboxField stLens name initialState =
    let initVal = initialState ^. stLens

        handleEvent (VtyEvent (EvKey (KChar ' ') [])) s = return $ not s
        handleEvent _ s = return s

    in FormFieldState { formFieldState        = initVal
                      , formFields            = [ FormField name Just
                                                            renderCheckbox
                                                            handleEvent
                                                ]
                      , formFieldLens         = stLens
                      , formFieldRenderHelper = id
                      }

renderCheckbox :: Bool -> Bool -> Widget n
renderCheckbox foc val =
    let addAttr = if foc then withDefAttr focusedFormInputAttr else id
    in addAttr $ str $ "[" <> (if val then "X" else " ") <> "]"

radioField :: (Ord n, Show n, Eq a)
           => Lens' s a
           -> [(a, n, T.Text)]
           -> s
           -> FormFieldState s e n
radioField stLens options initialState =
    let initVal = initialState ^. stLens

        handleEvent new (VtyEvent (EvKey (KChar ' ') [])) _ = return new
        handleEvent _ _ s = return s

        optionFields = mkOptionField <$> options
        mkOptionField (val, name, label) =
            FormField name
                      Just
                      (renderRadio val label)
                      (handleEvent val)

    in FormFieldState { formFieldState        = initVal
                      , formFields            = optionFields
                      , formFieldLens         = stLens
                      , formFieldRenderHelper = id
                      }

renderRadio :: (Eq a) => a -> T.Text -> Bool -> a -> Widget n
renderRadio val label foc cur =
    let addAttr = if foc
                  then withDefAttr focusedFormInputAttr
                  else id
        isSet = val == cur
    in addAttr $
       hBox [ str "["
            , str $ if isSet then "*" else " "
            , txt $ "] " <> label
            ]

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

    in FormFieldState { formFieldState = initVal
                      , formFields     = [ FormField n
                                                     (val . getEditContents)
                                                     (\b e -> wrapEditor $ renderEditor renderText b e)
                                                     handleEvent
                                         ]
                      , formFieldLens  = stLens
                      , formFieldRenderHelper = id
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

editTextField :: (Ord n, Show n)
              => Lens' s T.Text
              -> n
              -> Maybe Int
              -> s
              -> FormFieldState s e n
editTextField stLens n limit =
    let ini = id
        val = Just . T.intercalate "\n"
        renderText = txt . T.intercalate "\n"
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

focusedFormInputAttr :: AttrName
focusedFormInputAttr = formAttr <> "focusedInput"

renderForm :: (Eq n) => Form s e n -> Widget n
renderForm (Form es fr _) =
    vBox $ renderFormFieldState fr <$> es

renderFormFieldState :: (Eq n)
                     => FocusRing n
                     -> FormFieldState s e n
                     -> Widget n
renderFormFieldState fr (FormFieldState st _ fields helper) =
    let renderFields [] = []
        renderFields ((FormField n validate renderField _):fs) =
            let maybeInvalid = if isJust $ validate st
                               then id
                               else forceAttr invalidFormInputAttr
                foc = Just n == focusGetCurrent fr
            in maybeInvalid (renderField foc st) : renderFields fs
    in helper $ vBox $ renderFields fields

handleFormEvent :: (Eq n) => BrickEvent n e -> Form s e n -> EventM n (Form s e n)
handleFormEvent (VtyEvent (EvKey (KChar '\t') [])) f =
    return $ f { formFocus = focusNext $ formFocus f }
handleFormEvent (VtyEvent (EvKey KBackTab [])) f =
    return $ f { formFocus = focusPrev $ formFocus f }
handleFormEvent e f =
    case focusGetCurrent (formFocus f) of
        Nothing -> return f
        Just n  -> handleFormFieldEvent n e f

handleFormFieldEvent :: (Eq n) => n -> BrickEvent n e -> Form s e n -> EventM n (Form s e n)
handleFormFieldEvent n ev f = findFieldState [] (formFieldStates f)
    where
        findFieldState _ [] = return f
        findFieldState prev (e:es) =
            case e of
                FormFieldState st stLens fields helper -> do
                    let findField [] = return Nothing
                        findField (field:rest) =
                            case field of
                                FormField n' validate _ handleFunc | n == n' -> do
                                    nextSt <- handleFunc ev st
                                    -- If the new state validates, go ahead and update
                                    -- the form state with it.
                                    case validate nextSt of
                                        Nothing -> return $ Just (nextSt, Nothing)
                                        Just newSt -> return $ Just (nextSt, Just newSt)
                                _ -> findField rest

                    result <- findField fields
                    case result of
                        Nothing -> findFieldState (prev <> [e]) es
                        Just (newSt, maybeSt) ->
                            let newFieldState = FormFieldState newSt stLens fields helper
                            in return $ f { formFieldStates = prev <> [newFieldState] <> es
                                          , formState = case maybeSt of
                                              Nothing -> formState f
                                              Just s  -> formState f & stLens .~ s
                                          }
