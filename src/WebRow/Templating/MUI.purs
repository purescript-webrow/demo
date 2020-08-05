module WebRow.Templating.MUI where

import Prelude

import Control.Alt ((<|>))
import Data.Array (head, singleton) as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Data.Map (insert) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (over) as Newtype
import Data.Traversable (for)
import Data.Unfoldable (fromMaybe) as Unfoldable
import Data.Variant (Variant, case_, on)
import Effect (Effect)
import Effect.Console (log)
import Polyform.Batteries.UrlEncoded (Decoded(..))
import Polyform.Batteries.UrlEncoded (lookup) as UrlEncoded
import React.Basic (JSX, fragment)
import React.Basic.DOM (form, h2_, input, p_, text) as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import Type.Row (type (+))
import WebRow.Forms (Layout, LayoutBase(..), TextInput) as Forms
import WebRow.Forms.Widgets (TextInputProps(..), _textInput)

type FormLayout widgets = Forms.Layout (Forms.TextInput + widgets)

type RenderWidgets widgets = Decoded → Variant widgets → JSX

type Handlers =
  { setInputs ∷ (Decoded → Decoded) → Effect Unit
  , submit ∷ Effect Unit
  }

formBody ∷ ∀ widgets. Handlers → Decoded → RenderWidgets widgets → FormLayout widgets → JSX
formBody handlers updates renderWidgets (Forms.Section { closed, errors, layout }) = fragment
  [ flip foldMap errors \msg → DOM.p_ [ DOM.text msg ]
  , case closed of
    Just { title } → DOM.h2_ [ DOM.text title ]
    Nothing → mempty
  , foldMap (formBody handlers updates renderWidgets) layout
  ]
formBody handlers updates renderWidgets (Forms.Widget widget) =
  renderWidget widget
  where
  renderWidget =
    (renderWidgets updates)
    # on _textInput \(TextInputProps { name, payload, result, type_ }) → fragment
      [ flip foldMap result case _ of
          Left errors → flip foldMap errors \msg →
            DOM.p_  [ DOM.text msg ]
          otherwise → mempty
      , DOM.input
        { onChange: handler targetValue (\v → handlers.setInputs (Newtype.over Decoded (Map.insert name (Unfoldable.fromMaybe v))))
            -- setState _{ authenticated = Unknown, inputs { email = fromMaybe "" v }})

        , type: type_
        , name: name
        -- , value: (fromMaybe "" ((payload) >>= Array.head))
        , value: (fromMaybe "" ((UrlEncoded.lookup name updates <|> payload) >>= Array.head))
        }
    ]

form ∷ ∀ widgets. Handlers → Decoded → RenderWidgets widgets → FormLayout widgets → JSX
form handlers updates renderWidgets l = DOM.form $ { method: "post", onSubmit, children: _ }
  [ formBody handlers updates renderWidgets l
  , DOM.input { type: "submit", value: "submit" }
  ]
  where
    onSubmit = handler (preventDefault >>> targetValue) handleSubmit
    handleSubmit _ = handlers.submit

form' ∷ Handlers → Decoded → FormLayout () → JSX
form' handlers query = form handlers query (const case_)

