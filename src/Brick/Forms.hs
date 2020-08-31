{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | NOTE: This API is experimental and will probably change. Please try
-- it out! Feedback is very much appreciated, and your patience in the
-- face of breaking API changes is also appreciated! It's also worth
-- bearing in mind that this API is designed to support a narrow range
-- of use cases. If you find that you need more customization than this
-- offers, then you will need to consider building your own layout and
-- event handling for input fields.
--
-- For a fuller introduction to this API, see the "Input Forms" section
-- of the Brick User Guide. Also see the demonstration programs for
-- examples of forms in action.
--
-- This module provides an input form API. This API allows you to
-- construct an input interface based on a data type of your choice.
-- Each input in the form corresponds to a field in your data type. This
-- API then automatically dispatches keyboard and mouse input events to
-- each form input field, manages rendering of the form, notifies the
-- user when a form field's value is invalid, and stores valid inputs in
-- your data type when possible.
--
-- A form has both a visual representation and a corresponding data
-- structure representing the latest valid values for that form
-- (referred to as the "state" of the form). A 'FormField' is a single
-- input component in the form and a 'FormFieldState' defines the
-- linkage between that visual input and the corresponding portion
-- of the state represented by that visual; there may be multiple
-- 'FormField's combined for a single 'FormFieldState' (e.g. a radio
-- button sequence).
--
-- Also note that, by default, forms and their field inputs are
-- concatenated together in a 'vBox'. This can be customized on a
-- per-field basis and for the entire form by using the functions
-- 'setFieldConcat' and 'setFormConcat', respectively.
--
-- Bear in mind that for most uses, the 'FormField' and 'FormFieldState'
-- types will not be used directly. Instead, the constructors for
-- various field types (such as 'editTextField') will be used instead.
module Brick.Forms
  ( -- * Data types
    Form
  , FormFieldState(..)
  , FormField(..)

  -- * Creating and using forms
  , newForm
  , formFocus
  , formState
  , formStateLens
  , handleFormEvent
  , renderForm
  , renderFormFieldState
  , (@@=)
  , allFieldsValid
  , invalidFields
  , setFieldValid
  , setFormConcat
  , setFieldConcat
  , setFormFocus

  -- * Simple form field constructors
  , editTextField
  , editShowableField
  , editPasswordField
  , radioField
  , checkboxField
  , listField

  -- * Advanced form field constructors
  , editField
  , radioCustomField
  , checkboxCustomField

  -- * Attributes
  , formAttr
  , invalidFormInputAttr
  , focusedFormInputAttr
  )
where

import Graphics.Vty hiding (showCursor)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe (isJust, isNothing)
import Data.List (elemIndex)
import Data.Vector (Vector)

import Brick
import Brick.Focus
import Brick.Widgets.Edit
import Brick.Widgets.List
import qualified Data.Text.Zipper as Z

import qualified Data.Text as T
import Text.Read (readMaybe)

import Lens.Micro

-- | A form field. This represents an interactive input field in the
-- form. Its user input is validated and thus converted into a type of
-- your choosing.
--
-- Type variables are as follows:
--
--  * @a@ - the type of the field in your form state that this field
--    manipulates
--  * @b@ - the form field's internal state type
--  * @e@ - your application's event type
--  * @n@ - your application's resource name type
data FormField a b e n =
    FormField { formFieldName :: n
              -- ^ The name identifying this form field.
              , formFieldValidate :: b -> Maybe a
              -- ^ A validation function converting this field's state
              -- into a value of your choosing. @Nothing@ indicates a
              -- validation failure. For example, this might validate
              -- an 'Editor' state value by parsing its text contents s
              -- aan integer and return 'Maybe' 'Int'. This is for pure
              -- avalue validation; if additional validation is required
              -- a(e.g. via 'IO'), use this field's state value in an
              -- aexternal validation routine and use 'setFieldValid' to
              -- afeed the result back into the form.
              , formFieldExternallyValid :: Bool
              -- ^ Whether the field is valid according to an external
              -- validation source. Defaults to always being 'True' and
              -- can be set with 'setFieldValid'. The value of this
              -- field also affects the behavior of 'allFieldsValid' and
              -- 'getInvalidFields'.
              , formFieldRender :: Bool -> b -> Widget n
              -- ^ A function to render this form field. Parameters are
              -- whether the field is currently focused, followed by the
              -- field state.
              , formFieldHandleEvent :: BrickEvent n e -> b -> EventM n b
              -- ^ An event handler for this field. This receives the
              -- event and the field state and returns a new field
              -- state.
              }

-- | A form field state accompanied by the fields that manipulate that
-- state. The idea is that some record field in your form state has
-- one or more form fields that manipulate that value. This data type
-- maps that state field (using a lens into your state) to the form
-- input fields responsible for managing that state field, along with
-- a current value for that state field and an optional function to
-- control how the form inputs are rendered.
--
-- Most form fields will just have one input, such as text editors, but
-- others, such as radio button collections, will have many, which is
-- why this type supports more than one input corresponding to a state
-- field.
--
-- Type variables are as follows:
--
--  * @s@ - the data type containing the value manipulated by these form
--    fields.
--  * @e@ - your application's event type
--  * @n@ - your application's resource name type
data FormFieldState s e n where
    FormFieldState :: { formFieldState :: b
                      -- ^ The current state value associated with
                      -- the field collection. Note that this type is
                      -- existential. All form fields in the collection
                      -- must validate to this type.
                      , formFieldLens :: Lens' s a
                      -- ^ A lens to extract and store a
                      -- successfully-validated form input back into
                      -- your form state.
                      , formFields :: [FormField a b e n]
                      -- ^ The form fields, in order, that the user will
                      -- interact with to manipulate this state value.
                      , formFieldRenderHelper :: Widget n -> Widget n
                      -- ^ A helper function to augment the rendered
                      -- representation of this collection of form
                      -- fields. It receives the default representation
                      -- and can augment it, for example, by adding a
                      -- label on the left.
                      , formFieldConcat :: [Widget n] -> Widget n
                      -- ^ Concatenation function for this field's input
                      -- renderings.
                      } -> FormFieldState s e n

-- | A form: a sequence of input fields that manipulate the fields of an
-- underlying state that you choose.
--
-- Type variables are as follows:
--
--  * @s@ - the data type of your choosing containing the values
--    manipulated by the fields in this form.
--  * @e@ - your application's event type
--  * @n@ - your application's resource name type
data Form s e n =
    Form { formFieldStates  :: [FormFieldState s e n]
         , formFocus :: FocusRing n
         -- ^ The focus ring for the form, indicating which form field
         -- has input focus.
         , formState :: s
         -- ^ The current state of the form. Forms guarantee that only
         -- valid inputs ever get stored in the state, and that after
         -- each input event on a form field, if that field contains a
         -- valid state value then the value is immediately saved to its
         -- corresponding field in this state value using the form
         -- field's lens over @s@.
         , formConcatAll :: [Widget n] -> Widget n
         -- ^ Concatenation function for this form's field renderings.
         }

-- | A lens into the formState of a form.
--
-- This could be useful if you want to preseve focus properties, etc.
-- of your form but need to modify the underlying application state.

formStateLens :: Lens' (Form s e n) s
formStateLens = lens formState (\f s -> f { formState = s })

-- | Compose a new rendering augmentation function with the one in the
-- form field collection. For example, we might put a label on the left
-- side of a form field:
--
-- > (str "Please check: " <+>) @@= checkboxField alive AliveField "Alive?"
--
-- This can also be used to add multiple augmentations and associates
-- right:
--
-- > (withDefAttr someAttribute) @@=
-- > (str "Please check: " <+>) @@=
-- >   checkboxField alive AliveField "Alive?"
infixr 5 @@=
(@@=) :: (Widget n -> Widget n) -> (s -> FormFieldState s e n) -> s -> FormFieldState s e n
(@@=) h mkFs s =
    let v = mkFs s
    in v { formFieldRenderHelper = h . (formFieldRenderHelper v) }

-- | Set the focused field of a form.
setFormFocus :: (Eq n) => n -> Form s e n -> Form s e n
setFormFocus n f = f { formFocus = focusSetCurrent n $ formFocus f }

-- | Set a form field's concatenation function.
setFieldConcat :: ([Widget n] -> Widget n) -> FormFieldState s e n -> FormFieldState s e n
setFieldConcat f s = s { formFieldConcat = f }

-- | Set a form's concatenation function.
setFormConcat :: ([Widget n] -> Widget n) -> Form s e n -> Form s e n
setFormConcat func f = f { formConcatAll = func }

-- | Create a new form with the specified input fields and an initial
-- form state. The fields are initialized from the state using their
-- state lenses and the first form input is focused initially.
newForm :: [s -> FormFieldState s e n]
        -- ^ The form field constructors. This is intended to be
        -- populated using the various field constructors in this
        -- module.
        -> s
        -- ^ The initial form state used to populate the fields.
        -> Form s e n
newForm mkEs s =
    let es = mkEs <*> pure s
    in Form { formFieldStates = es
            , formFocus       = focusRing $ concat $ formFieldNames <$> es
            , formState       = s
            , formConcatAll   = vBox
            }

formFieldNames :: FormFieldState s e n -> [n]
formFieldNames (FormFieldState _ _ fields _ _) = formFieldName <$> fields

-- | A form field for manipulating a boolean value. This represents
-- 'True' as @[X] label@ and 'False' as @[ ] label@.
--
-- This field responds to `Space` keypresses to toggle the checkbox and
-- to mouse clicks.
checkboxField :: (Ord n, Show n)
              => Lens' s Bool
              -- ^ The state lens for this value.
              -> n
              -- ^ The resource name for the input field.
              -> T.Text
              -- ^ The label for the check box, to appear at its right.
              -> s
              -- ^ The initial form state.
              -> FormFieldState s e n
checkboxField = checkboxCustomField '[' 'X' ']'

-- | A form field for manipulating a boolean value. This represents
-- 'True' as @[X] label@ and 'False' as @[ ] label@. This function
-- permits the customization of the @[X]@ notation characters.
--
-- This field responds to `Space` keypresses to toggle the checkbox and
-- to mouse clicks.
checkboxCustomField :: (Ord n, Show n)
                    => Char
                    -- ^ Left bracket character.
                    -> Char
                    -- ^ Checkmark character.
                    -> Char
                    -- ^ Right bracket character.
                    -> Lens' s Bool
                    -- ^ The state lens for this value.
                    -> n
                    -- ^ The resource name for the input field.
                    -> T.Text
                    -- ^ The label for the check box, to appear at its right.
                    -> s
                    -- ^ The initial form state.
                    -> FormFieldState s e n
checkboxCustomField lb check rb stLens name label initialState =
    let initVal = initialState ^. stLens

        handleEvent (MouseDown n _ _ _) s | n == name = return $ not s
        handleEvent (VtyEvent (EvKey (KChar ' ') [])) s = return $ not s
        handleEvent _ s = return s

    in FormFieldState { formFieldState = initVal
                      , formFields = [ FormField name Just True
                                                 (renderCheckbox lb check rb label name)
                                                 handleEvent
                                     ]
                      , formFieldLens = stLens
                      , formFieldRenderHelper = id
                      , formFieldConcat = vBox
                      }

renderCheckbox :: Char -> Char -> Char -> T.Text -> n -> Bool -> Bool -> Widget n
renderCheckbox lb check rb label n foc val =
    let addAttr = if foc then withDefAttr focusedFormInputAttr else id
        csr = if foc then showCursor n (Location (1,0)) else id
    in clickable n $
       addAttr $ csr $
       (txt $ T.singleton lb <> (if val then T.singleton check else " ") <>
              T.singleton rb <> " ") <+> txt label

-- | A form field for selecting a single choice from a set of possible
-- choices in a scrollable list. This uses a 'List' internally.
--
-- This field responds to the same input events that a 'List' does.
listField :: forall s e n a . (Ord n, Show n, Eq a)
          => (s -> Vector a)
          -- ^ Possible choices.
          -> Lens' s (Maybe a)
          -- ^ The state lens for the initially/finally selected
          -- element.
          -> (Bool -> a -> Widget n)
          -- ^ List item rendering function.
          -> Int
          -- ^ List item height in rows.
          -> n
          -- ^ The resource name for the input field.
          -> s
          -- ^ The initial form state.
          -> FormFieldState s e n
listField options stLens renderItem itemHeight name initialState =
    let optionsVector = options initialState
        initVal = initialState ^. customStLens

        customStLens :: Lens' s (List n a)
        customStLens = lens getList setList
            where
               getList s = let l = list name optionsVector itemHeight
                           in case s ^. stLens of
                               Nothing -> l
                               Just e -> listMoveToElement e l
               setList s l = s & stLens .~ (snd <$> listSelectedElement l)

        handleEvent (VtyEvent e) s = handleListEvent e s
        handleEvent _ s = return s

    in FormFieldState { formFieldState = initVal
                      , formFields = [ FormField name Just True
                                                 (renderList renderItem)
                                                 handleEvent
                                     ]
                      , formFieldLens = customStLens
                      , formFieldRenderHelper = id
                      , formFieldConcat = vBox
                      }
-- | A form field for selecting a single choice from a set of possible
-- choices. Each choice has an associated value and text label.
--
-- This field responds to `Space` keypresses to select a radio button
-- option and to mouse clicks.
radioField :: (Ord n, Show n, Eq a)
           => Lens' s a
           -- ^ The state lens for this value.
           -> [(a, n, T.Text)]
           -- ^ The available choices, in order. Each choice has a value
           -- of type @a@, a resource name, and a text label.
           -> s
           -- ^ The initial form state.
           -> FormFieldState s e n
radioField = radioCustomField '[' '*' ']'

-- | A form field for selecting a single choice from a set of possible
-- choices. Each choice has an associated value and text label. This
-- function permits the customization of the @[*]@ notation characters.
--
-- This field responds to `Space` keypresses to select a radio button
-- option and to mouse clicks.
radioCustomField :: (Ord n, Show n, Eq a)
                 => Char
                 -- ^ Left bracket character.
                 -> Char
                 -- ^ Checkmark character.
                 -> Char
                 -- ^ Right bracket character.
                 -> Lens' s a
                 -- ^ The state lens for this value.
                 -> [(a, n, T.Text)]
                 -- ^ The available choices, in order. Each choice has a value
                 -- of type @a@, a resource name, and a text label.
                 -> s
                 -- ^ The initial form state.
                 -> FormFieldState s e n
radioCustomField lb check rb stLens options initialState =
    let initVal = initialState ^. stLens

        lookupOptionValue n =
            let results = filter (\(_, n', _) -> n' == n) options
            in case results of
                [(val, _, _)] -> Just val
                _ -> Nothing

        handleEvent _ (MouseDown n _ _ _) s =
            case lookupOptionValue n of
                Nothing -> return s
                Just v -> return v
        handleEvent new (VtyEvent (EvKey (KChar ' ') [])) _ = return new
        handleEvent _ _ s = return s

        optionFields = mkOptionField <$> options
        mkOptionField (val, name, label) =
            FormField name
                      Just
                      True
                      (renderRadio lb check rb val name label)
                      (handleEvent val)

    in FormFieldState { formFieldState = initVal
                      , formFields = optionFields
                      , formFieldLens = stLens
                      , formFieldRenderHelper = id
                      , formFieldConcat = vBox
                      }

renderRadio :: (Eq a) => Char -> Char -> Char -> a -> n -> T.Text -> Bool -> a -> Widget n
renderRadio lb check rb val name label foc cur =
    let addAttr = if foc
                  then withDefAttr focusedFormInputAttr
                  else id
        isSet = val == cur
        csr = if foc then showCursor name (Location (1,0)) else id
    in clickable name $
       addAttr $ csr $
       hBox [ txt $ T.singleton lb
            , txt $ if isSet then T.singleton check else " "
            , txt $ T.singleton rb <> " " <> label
            ]

-- | A form field for using an editor to edit the text representation of
-- a value. The other editing fields in this module are special cases of
-- this function.
--
-- This field responds to all events handled by 'editor', including
-- mouse events.
editField :: (Ord n, Show n)
          => Lens' s a
          -- ^ The state lens for this value.
          -> n
          -- ^ The resource name for the input field.
          -> Maybe Int
          -- ^ The optional line limit for the editor (see 'editor').
          -> (a -> T.Text)
          -- ^ The initialization function that turns your value into
          -- the editor's initial contents. The resulting text may
          -- contain newlines.
          -> ([T.Text] -> Maybe a)
          -- ^ The validation function that converts the editors
          -- contents into a valid value of type @a@.
          -> ([T.Text] -> Widget n)
          -- ^ The rendering function for the editor's contents (see
          -- 'renderEditor').
          -> (Widget n -> Widget n)
          -- ^ A rendering augmentation function to adjust the
          -- representation of the rendered editor.
          -> s
          -- ^ The initial form state.
          -> FormFieldState s e n
editField stLens n limit ini val renderText wrapEditor initialState =
    let initVal = applyEdit gotoEnd $
                  editor n limit initialText
        gotoEnd = let ls = T.lines initialText
                      pos = (length ls - 1, T.length (last ls))
                  in if null ls
                     then id
                     else Z.moveCursor pos
        initialText = ini $ initialState ^. stLens
        handleEvent (VtyEvent e) ed = handleEditorEvent e ed
        handleEvent _ ed = return ed

    in FormFieldState { formFieldState = initVal
                      , formFields = [ FormField n
                                                 (val . getEditContents)
                                                 True
                                                 (\b e -> wrapEditor $ renderEditor renderText b e)
                                                 handleEvent
                                     ]
                      , formFieldLens = stLens
                      , formFieldRenderHelper = id
                      , formFieldConcat = vBox
                      }

-- | A form field using a single-line editor to edit the 'Show'
-- representation of a state field value of type @a@. This automatically
-- uses its 'Read' instance to validate the input. This field is mostly
-- useful in cases where the user-facing representation of a value
-- matches the 'Show' representation exactly, such as with 'Int'.
--
-- This field responds to all events handled by 'editor', including
-- mouse events.
editShowableField :: (Ord n, Show n, Read a, Show a)
                  => Lens' s a
                  -- ^ The state lens for this value.
                  -> n
                  -- ^ The resource name for the input field.
                  -> s
                  -- ^ The initial form state.
                  -> FormFieldState s e n
editShowableField stLens n =
    let ini = T.pack . show
        val = readMaybe . T.unpack . T.intercalate "\n"
        limit = Just 1
        renderText = txt . T.unlines
    in editField stLens n limit ini val renderText id

-- | A form field using an editor to edit a text value. Since the value
-- is free-form text, it is always valid.
--
-- This field responds to all events handled by 'editor', including
-- mouse events.
editTextField :: (Ord n, Show n)
              => Lens' s T.Text
              -- ^ The state lens for this value.
              -> n
              -- ^ The resource name for the input field.
              -> Maybe Int
              -- ^ The optional line limit for the editor (see 'editor').
              -> s
              -- ^ The initial form state.
              -> FormFieldState s e n
editTextField stLens n limit =
    let ini = id
        val = Just . T.intercalate "\n"
        renderText = txt . T.intercalate "\n"
    in editField stLens n limit ini val renderText id

-- | A form field using a single-line editor to edit a free-form text
-- value represented as a password. The value is always considered valid
-- and is always represented with one asterisk per password character.
--
-- This field responds to all events handled by 'editor', including
-- mouse events.
editPasswordField :: (Ord n, Show n)
                  => Lens' s T.Text
                  -- ^ The state lens for this value.
                  -> n
                  -- ^ The resource name for the input field.
                  -> s
                  -- ^ The initial form state.
                  -> FormFieldState s e n
editPasswordField stLens n =
    let ini = id
        val = Just . T.concat
        limit = Just 1
        renderText = toPassword
    in editField stLens n limit ini val renderText id

toPassword :: [T.Text] -> Widget a
toPassword s = txt $ T.replicate (T.length $ T.concat s) "*"

-- | The namespace for the other form attributes.
formAttr :: AttrName
formAttr = "brickForm"

-- | The attribute for form input fields with invalid values.
invalidFormInputAttr :: AttrName
invalidFormInputAttr = formAttr <> "invalidInput"

-- | The attribute for form input fields that have the focus.
focusedFormInputAttr :: AttrName
focusedFormInputAttr = formAttr <> "focusedInput"

-- | Returns whether all form fields in the form currently have valid
-- values according to the fields' validation functions. This is useful
-- when we need to decide whether the form state is up to date with
-- respect to the form input fields.
allFieldsValid :: Form s e n -> Bool
allFieldsValid = null . invalidFields

-- | Returns the resource names associated with all form input fields
-- that currently have invalid inputs. This is useful when we need to
-- force the user to repair invalid inputs before moving on from a form
-- editing session.
invalidFields :: Form s e n -> [n]
invalidFields f = concat $ getInvalidFields <$> formFieldStates f

-- | Manually indicate that a field has invalid contents. This can be
-- useful in situations where validation beyond the form element's
-- validator needs to be performed and the result of that validation
-- needs to be fed back into the form state.
setFieldValid :: (Eq n)
              => Bool
              -- ^ Whether the field is considered valid.
              -> n
              -- ^ The name of the form field to set as (in)valid.
              -> Form s e n
              -- ^ The form to modify.
              -> Form s e n
setFieldValid v n form =
    let go1 [] = []
        go1 (s:ss) =
            let s' = case s of
                       FormFieldState st l fs rh concatAll ->
                           let go2 [] = []
                               go2 (f@(FormField fn val _ r h):ff)
                                   | n == fn = FormField fn val v r h : ff
                                   | otherwise = f : go2 ff
                           in FormFieldState st l (go2 fs) rh concatAll
            in s' : go1 ss

    in form { formFieldStates = go1 (formFieldStates form) }

getInvalidFields :: FormFieldState s e n -> [n]
getInvalidFields (FormFieldState st _ fs _ _) =
    let gather (FormField n validate extValid _ _) =
            if (not extValid || (isNothing $ validate st)) then [n] else []
    in concat $ gather <$> fs

-- | Render a form.
--
-- For each form field, each input for the field is rendered using
-- the implementation provided by its 'FormField'. The inputs are
-- then concatenated with the field's concatenation function (see
-- 'setFieldConcat') and are then augmented using the form field's
-- rendering augmentation function (see '@@='). Fields with invalid
-- inputs (either due to built-in validator failure or due to external
-- validation failure via 'setFieldValid') will be displayed using the
-- 'invalidFormInputAttr' attribute.
--
-- Finally, all of the resulting field renderings are concatenated with
-- the form's concatenation function (see 'setFormConcat').
renderForm :: (Eq n) => Form s e n -> Widget n
renderForm (Form es fr _ concatAll) =
    concatAll $ renderFormFieldState fr <$> es

-- | Render a single form field collection. This is called internally by
-- 'renderForm' but is exposed in cases where a form field state needs
-- to be rendered outside of a 'Form', so 'renderForm' is probably what
-- you want.
renderFormFieldState :: (Eq n)
                     => FocusRing n
                     -> FormFieldState s e n
                     -> Widget n
renderFormFieldState fr (FormFieldState st _ fields helper concatFields) =
    let renderFields [] = []
        renderFields ((FormField n validate extValid renderField _):fs) =
            let maybeInvalid = if (isJust $ validate st) && extValid
                               then id
                               else forceAttr invalidFormInputAttr
                foc = Just n == focusGetCurrent fr
            in maybeInvalid (renderField foc st) : renderFields fs
    in helper $ concatFields $ renderFields fields

-- | Dispatch an event to the appropriate form field and return a new
-- form. This handles the following events in this order:
--
-- * On @Tab@ keypresses, this changes the focus to the next field in
--   the form.
-- * On @Shift-Tab@ keypresses, this changes the focus to the previous
--   field in the form.
-- * On mouse button presses (regardless of button or modifier), the
--   focus is changed to the clicked form field and the event is
--   forwarded to the event handler for the clicked form field.
-- * On @Left@ or @Up@, if the currently-focused field is part of a
--   collection (e.g. radio buttons), the previous entry in the
--   collection is focused.
-- * On @Right@ or @Down@, if the currently-focused field is part of a
--   collection (e.g. radio buttons), the next entry in the collection
--   is focused.
-- * All other events are forwarded to the currently focused form field.
--
-- In all cases where an event is forwarded to a form field, validation
-- of the field's input state is performed immediately after the
-- event has been handled. If the form field's input state succeeds
-- validation using the field's validator function, its value is
-- immediately stored in the form state using the form field's state
-- lens. The external validation flag is ignored during this step to
-- ensure that external validators have a chance to get the intermediate
-- validated value.
handleFormEvent :: (Eq n) => BrickEvent n e -> Form s e n -> EventM n (Form s e n)
handleFormEvent (VtyEvent (EvKey (KChar '\t') [])) f =
    return $ f { formFocus = focusNext $ formFocus f }
handleFormEvent (VtyEvent (EvKey KBackTab [])) f =
    return $ f { formFocus = focusPrev $ formFocus f }
handleFormEvent e@(MouseDown n _ _ _) f =
    handleFormFieldEvent n e $ f { formFocus = focusSetCurrent n (formFocus f) }
handleFormEvent e@(MouseUp n _ _) f =
    handleFormFieldEvent n e $ f { formFocus = focusSetCurrent n (formFocus f) }
handleFormEvent e@(VtyEvent (EvKey KUp [])) f =
    case focusGetCurrent (formFocus f) of
        Nothing -> return f
        Just n  ->
            case getFocusGrouping f n of
                Nothing -> forwardToCurrent e f
                Just grp -> return $ f { formFocus = focusSetCurrent (entryBefore grp n) (formFocus f) }
handleFormEvent e@(VtyEvent (EvKey KDown [])) f =
    case focusGetCurrent (formFocus f) of
        Nothing -> return f
        Just n  ->
            case getFocusGrouping f n of
                Nothing -> forwardToCurrent e f
                Just grp -> return $ f { formFocus = focusSetCurrent (entryAfter grp n) (formFocus f) }
handleFormEvent e@(VtyEvent (EvKey KLeft [])) f =
    case focusGetCurrent (formFocus f) of
        Nothing -> return f
        Just n  ->
            case getFocusGrouping f n of
                Nothing -> forwardToCurrent e f
                Just grp -> return $ f { formFocus = focusSetCurrent (entryBefore grp n) (formFocus f) }
handleFormEvent e@(VtyEvent (EvKey KRight [])) f =
    case focusGetCurrent (formFocus f) of
        Nothing -> return f
        Just n  ->
            case getFocusGrouping f n of
                Nothing -> forwardToCurrent e f
                Just grp -> return $ f { formFocus = focusSetCurrent (entryAfter grp n) (formFocus f) }
handleFormEvent e f = forwardToCurrent e f

getFocusGrouping :: (Eq n) => Form s e n -> n -> Maybe [n]
getFocusGrouping f n = findGroup (formFieldStates f)
    where
        findGroup [] = Nothing
        findGroup (e:es) =
            let ns = formFieldNames e
            in if n `elem` ns && length ns > 1
               then Just ns
               else findGroup es

entryAfter :: (Eq a) => [a] -> a -> a
entryAfter as a =
    let Just i = elemIndex a as
        i' = if i == length as - 1 then 0 else i + 1
    in as !! i'

entryBefore :: (Eq a) => [a] -> a -> a
entryBefore as a =
    let Just i = elemIndex a as
        i' = if i == 0 then length as - 1 else i - 1
    in as !! i'

forwardToCurrent :: (Eq n) => BrickEvent n e -> Form s e n -> EventM n (Form s e n)
forwardToCurrent e f =
    case focusGetCurrent (formFocus f) of
        Nothing -> return f
        Just n  -> handleFormFieldEvent n e f

handleFormFieldEvent :: (Eq n) => n -> BrickEvent n e -> Form s e n -> EventM n (Form s e n)
handleFormFieldEvent n ev f = findFieldState [] (formFieldStates f)
    where
        findFieldState _ [] = return f
        findFieldState prev (e:es) =
            case e of
                FormFieldState st stLens fields helper concatAll -> do
                    let findField [] = return Nothing
                        findField (field:rest) =
                            case field of
                                FormField n' validate _ _ handleFunc | n == n' -> do
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
                            let newFieldState = FormFieldState newSt stLens fields helper concatAll
                            in return $ f { formFieldStates = prev <> [newFieldState] <> es
                                          , formState = case maybeSt of
                                              Nothing -> formState f
                                              Just s  -> formState f & stLens .~ s
                                          }
