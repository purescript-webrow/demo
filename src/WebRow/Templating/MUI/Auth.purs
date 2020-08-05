module WebRow.Templating.MUI.Auth where

import Prelude

import Data.Variant (inj)
import MUI.SSR (JSXRender(..))
import MUI.SSR (renderJSX) as SSR
import React.Basic (JSX, element)
import React.Basic.DOM (body, div', head, html, link, meta, script, style', text, title) as DOM
import Run (Run(..))
import Type.Row (type (+))
import WebRow.Applets.Auth (Response, Route) as Auth
import WebRow.Applets.Auth (Route(..))
import WebRow.Applets.Auth.Responses as Auth.Responses
import WebRow.Applets.Auth.Types (_auth)
import WebRow.HTTP (HTTPResponse)
import WebRow.HTTP.Response (found, ok)
import WebRow.Routing (Routing', fromRelativeUrl, printRoute)
import WebRow.Templating.MUI (form')
import WebRow.Templating.React (renderToString)

appElementId ∷ String
appElementId = "app"

styleElementId ∷ String
styleElementId = "app-style"

page { body, description, title } = html
  where
    JSXRender html = SSR.renderJSX $
      let
        j = DOM.script <<< { defer: false , src: _ }
        c = DOM.link <<< { rel: "stylesheet" , href: _ }
        html = renderToString body
      -- { css, html, sheets } ← liftEffect $ do
      --   sheets ← SSR.serverStyleSheets
      in
        DOM.html
          { lang: "en"
          , children:
            [ DOM.head
              { children:
                  [ DOM.meta { charSet: "utf-8" }
                  , DOM.meta { httpEquiv: "X-UA-Compatible", content: "IE=edge,chrome=1" }
                  , DOM.meta { name: "description", content: description }
                  , DOM.meta { name: "viewport", content: "width=device-width" }
                  , DOM.link { rel: "shortcut icon", href: "/favicon.png" }
                  , DOM.title { children: [ DOM.text title ] }
                  , c "https://fonts.googleapis.com/icon?family=Material+Icons"
                  , c "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap"
                  , c "https://fonts.googleapis.com/css?family=Major+Mono+Display"
                  -- , element DOM.style' { dangerouslySetInnerHTML: { __html: docStyle }}
                  -- , element DOM.style' { id: styleElementId, dangerouslySetInnerHTML: { __html: css }}
                  ]
              }
            , DOM.body $ { children: _, id: "body" } -- , style: DOM.css { overflow: Overflow.overflow (Overflow.body route) }}
              [ element DOM.div' { id: appElementId, dangerouslySetInnerHTML: { __html: html }}
              , j "/static/bundle.js"
              ]
            ]
          }

page' body = page { body, title: "Testing MUI rendering", description: "No description yet" }


handlers =
  { setInputs: const $ pure unit
  , submit: pure unit
  }
render :: forall routes t7. Auth.Response -> Run (Routing' (auth ∷ Auth.Route | routes) + t7) (HTTPResponse String)
render = case _ of
  Auth.Responses.LoginResponse loginResponse → case loginResponse of
    Auth.Responses.LoginFormValidationFailed formLayout → ok $ page' $ form' handlers mempty formLayout
    Auth.Responses.EmailPasswordMismatch formLayout → ok $ page' $ form' handlers mempty formLayout
    Auth.Responses.InitialEmailPassordForm formLayout → ok $ page' $ form' handlers mempty formLayout
    Auth.Responses.LoginSuccess → ok $ page' $ DOM.text "TEST"
  Auth.Responses.LogoutResponse → do
    redirectTo ← fromRelativeUrl <$> printRoute (inj _auth Login)
    found redirectTo

  -- const $ ok $ html $ DOM.text "JSX"
