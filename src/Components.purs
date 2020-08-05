module Components where

import Prelude

import Data.Foldable (foldMap)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, case_, on)
import Data.Variant (match) as Variant
import Debug.Trace (traceM)
import Effect (Effect)
import Polyform.Batteries.Messages (urlEncoded) as Batteries.Messages
import Polyform.Batteries.Messages.UrlEncoded (Messages) as UrlEncoded
import Polyform.Batteries.UrlEncoded (Decoded(..))
import Polyform.Batteries.UrlEncoded.Validators (MissingValue)
import React.Basic.DOM (text) as DOM
import React.Basic.Hooks (ReactComponent, useState)
import React.Basic.Hooks as React
import Record.Builder (build) as Record.Builder
import Run (Run(..), run)
import Run (case_, interpret, on, send) as Run
import Type.Row (type (+))
import WebRow.Applets.Auth (Auth)
import WebRow.Applets.Auth (Messages) as Auth
import WebRow.Applets.Auth.Effects (AuthF(..), AUTH)
import WebRow.Applets.Auth.Forms (loginForm)
import WebRow.Applets.Auth.Testing.Messages (auth) as Auth.Testing.Messages
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Applets.Registration.Messages (Messages) as Registration
import WebRow.Applets.Registration.Testing.Messages (registration) as Registration.Testing.Messages
import WebRow.Forms (TextInput)
import WebRow.Forms (Uni) as Forms
import WebRow.Forms.Payload (UrlDecoded)
import WebRow.Forms.Uni (MessageM, Layout)
import WebRow.Forms.Uni (default, validate) as Uni
import WebRow.Forms.Validators (InvalidEmailFormat)
import WebRow.Forms.Widget (dump) as Widget
import WebRow.Forms.Widgets (TextInputProps(..), _textInput)
import WebRow.Message (MESSAGE, MessageF(..), Message, _message)
import WebRow.Templating.MUI (form) as MUI
import WebRow.Templating.MUI (formBody)
import WebRow.Testing.Messages (validators) as WebRow.Testing.Messages

type Messages = ( Auth.Messages + InvalidEmailFormat + Registration.Messages + UrlEncoded.Messages + ())

runMessage ∷ ∀ eff. Run (message ∷ MESSAGE Messages | eff) ~> Run ( | eff)
runMessage = Run.interpret (Run.on _message handleMessage Run.send)
  where
    handleMessage ∷ ∀ m. Monad m ⇒ MessageF (Variant Messages) ~> m
    handleMessage (MessageF v next) = pure $ next (print v)
      where
        print = Variant.match printers
        printers = Record.Builder.build
          ( Auth.Testing.Messages.auth
          <<< Registration.Testing.Messages.registration
          -- | Rename this to validators?
          <<< WebRow.Testing.Messages.validators
          -- <<< Batteries.Messages.string
          <<< Batteries.Messages.urlEncoded
          )
          {}

runAuth ∷ ∀ eff. Run (auth ∷ AUTH () | eff) ~> Run ( | eff)
runAuth = Run.interpret (Run.on _auth handleAuth Run.send)
  where
    handleAuth :: forall m. Applicative m => AuthF () ~> m
    handleAuth (Authenticate email password next) = do
      pure $ next (Just { email })

type State msgs eff widgets o =
  { inputs ∷ UrlDecoded
  , layout ∷ Layout widgets
  , validate ∷ UrlDecoded → { layout :: Layout widgets, result :: Maybe o }
  }

x :: forall o msgs widgets
  . Forms.Uni (Auth () + ()) msgs widgets o
  -> (Run (Auth () + Message msgs + ()) ~> Run ())
  -> { updates :: Decoded
     , layout :: Layout widgets
     , validate :: Decoded -> { layout :: Layout widgets, result :: Maybe o }
     }
x form interpreter =
  let
    initialForm = Uni.default form
    Identity layout = run Run.case_ (interpreter initialForm)

    validate d =
      let
        Identity (Tuple layout result) =  run Run.case_ (interpreter (Uni.validate form d))
      in
        ({layout, result})
  in
    { updates: mempty
    , layout
    , validate
    }

dumpWidgets = case_
  # on _textInput \(TextInputProps { name, payload }) → Widget.dump (Identity name) (Identity payload)

formComponentBuilder :: ∀ o
  . Forms.Uni (Auth () + ()) Messages (TextInput + ()) o
  -> (Run (Auth () + Message Messages + ()) ~> Run ())
  -> Effect (ReactComponent {})
formComponentBuilder formBuilder runner =
  React.component "Form" \_ → React.do
    let
      { updates, layout, validate } = x formBuilder runner
    formState /\ setFormState ← useState { updates, layout }

    let
      setInputs f =
        setFormState (\{ layout, updates } → { layout, updates: f updates })

      submit = setFormState (\{ layout, updates } → { layout: (validate (updates <> foldMap dumpWidgets layout)).layout, updates })
      handlers =
        { setInputs
        , submit
        }

    pure $ MUI.form handlers formState.updates (const case_) formState.layout



