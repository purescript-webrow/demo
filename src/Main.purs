module Main where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (insert, lookup) as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (snd) as Tuple
import Data.Variant (Variant, case_, on)
import Data.Variant (inj, match) as Variant
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref (modify_, new, read) as Ref
import HTTPure (Method(..), Request, ResponseM)
import HTTPure (serve) as HTTPure
import Polyform.Batteries.Messages (string, urlEncoded) as Batteries.Messages
import Polyform.Batteries.Messages.String (Messages) as String
import Polyform.Batteries.Messages.UrlEncoded (Messages) as Batteries.Messages.UrlEncoded
import Polyform.Batteries.Messages.UrlEncoded (Messages) as UrlEncoded
import Record.Builder (build, insert) as Record.Builder
import Routing.Duplex (RouteDuplex', root) as D
import Routing.Duplex.Generic (noArgs, sum) as D
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (Run, runBaseAff')
import Run (interpret, liftEffect, on, run, send) as Run
import Text.Smolder.HTML (a, li, ul) as M
import Text.Smolder.HTML.Attributes (href) as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as M
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import WebRow (method, ok) as W
import WebRow.Applets.Auth (Route(..), RouteRow, Messages, routeBuilder, router) as Auth
import WebRow.Applets.Auth.Effects (AuthF(..))
import WebRow.Applets.Auth.Testing.Messages (auth) as Auth.Testing.Messages
import WebRow.Applets.Auth.Testing.Templates (render) as Auth.Testing.Templates
import WebRow.Applets.Auth.Types (Password, _auth)
import WebRow.Applets.Registration (Route(..), RouteRow, Messages, routeBuilder) as Registration
import WebRow.Applets.Registration (_emailVerification, registerEmail)
import WebRow.Applets.Registration (router) as Registarion
import WebRow.Applets.Registration.Effects (Registration, RegistrationF(..))
import WebRow.Applets.Registration.Testing.Messages (registration) as Registration.Testing.Messages
import WebRow.Applets.Registration.Testing.Templates (render) as Registration.Testing.Templates
import WebRow.Applets.Registration.Types (_registration)
import WebRow.Contrib.Run (EffRow)
import WebRow.Crypto (run) as Crypto
import WebRow.Forms.Validators (Messages) as WebRow.Forms.Validators
import WebRow.HTTP.Cookies (run) as Cookies
import WebRow.HTTP.Response (ok, run) as Response
import WebRow.HTTP.Response (setHeader)
import WebRow.KeyValueStore.Types (Key)
import WebRow.Mailer (Email(..), Mailer, _mailer)
import WebRow.Mailer (MailerF(..)) as Mailer
import WebRow.Message (Message, MessageF(..), _message)
import WebRow.Routing (FullUrl(..), RelativeUrl(..), printRoute, route, runRouting)
import WebRow.Session (runInMemory) as Session
import WebRow.Testing.Interpret (runMailer', runMessage) as Testing.Interpret
import WebRow.Testing.Messages (validators) as WebRow.Testing.Messages
import WebRow.Testing.Templates (html)

data Root = Root
derive instance genericRoot ∷ Generic Root _

type RootRoutes routes = ("" ∷ Root | routes)

rootRouteDuplex = D.sum { "Root": D.noArgs }

type Routes = Variant (RootRoutes + Auth.RouteRow + Registration.RouteRow + ())

_root = SProxy ∷ SProxy ""

routeDuplex ∷ D.RouteDuplex' Routes
routeDuplex = D.root $ RouteDuplex.Variant.variant' $ Record.Builder.build
  ( Record.Builder.insert _root rootRouteDuplex
  <<< Auth.routeBuilder
  <<< Registration.routeBuilder
  ) {}

router = case_
  # on _root
    ( \_ → pure $ Variant.inj _root do
        RelativeUrl loginUrl ← printRoute (Variant.inj _auth Auth.Login)
        RelativeUrl registerUrl ← printRoute (Variant.inj _registration Registration.RegisterEmail)
        Response.ok $ html $ do
          M.ul $ do
            M.li $ M.a ! A.href registerUrl $ M.text "register"
            M.li $ M.a ! A.href loginUrl $ M.text "login"
    )
  # Auth.router
  # Registarion.router

-- runAuth ∷ ∀ eff. Run (Auth () + eff) ~> Run eff
runAuth db = Run.run (Run.on _auth handler Run.send)
  where
    handler (Authenticate email password next) = do
      Run.liftEffect (Ref.read db) >>= Map.lookup email >>> case _ of
        Just dbPassword → if password == dbPassword
          then pure $ next (Just { email })
          else pure $ next Nothing
        Nothing → pure $ next Nothing

runRegistration ∷ ∀ eff. Ref (Map Email Password) → Run (EffRow + Registration + eff) ~> Run (EffRow + eff)
runRegistration db = Run.run (Run.on _registration handler Run.send)
  where
    handler (EmailTakenF email next) = do
      Run.liftEffect (Ref.read db) >>= Map.lookup email >>> isJust >>> next >>> pure
    handler (RegisterF email password next) = do
      Run.liftEffect (Ref.modify_ (Map.insert email password) db)
      pure next

runMailer ∷ ∀ eff. Run (Mailer (emailVerification ∷ FullUrl) + EffRow + eff) ~> Run (EffRow + eff)
runMailer = Run.run (Run.on _mailer handler Run.send)
  where
    sendToConsole { to, body, subject } = do
      Run.liftEffect $ log $ "\nrecipient: " <> to
      Run.liftEffect $ log $ "subject: " <> subject
      Run.liftEffect $ log $ "body:\n" <> body <> "\n\n"

    sendConfirmation to (FullUrl registrationLink) = sendToConsole
      { body: "registration link:" <> registrationLink
      , subject: "registration email"
      , to
      }

    handler (Mailer.SendF { to: Email to, context } next) = do
      case_ # on _emailVerification (sendConfirmation to) $ context
      pure next

type SessionData = { user ∷ Maybe { email ∷ Email }}

type Messages =
  ( Registration.Messages + Auth.Messages
  + UrlEncoded.Messages + WebRow.Forms.Validators.Messages
  + String.Messages
  + ()
  )

handleMessage ∷ ∀ m msgs. Monad m ⇒ MessageF (Variant Messages) ~> m
handleMessage (MessageF v next) = pure $ next (print v)
  where
    print = Variant.match printers
    printers = Record.Builder.build
      ( Registration.Testing.Messages.registration
      <<< Auth.Testing.Messages.auth
      -- | Rename this to validators?
      <<< WebRow.Testing.Messages.validators
      <<< Batteries.Messages.string
      <<< Batteries.Messages.urlEncoded
      )
      {}

runMessage = Run.interpret (Run.on _message handleMessage Run.send)

app
  ∷ { user ∷ Ref (Map Email Password)
    , session ∷ Ref (Map Key SessionData)
    }
  → Request
  → ResponseM
app db req = do
  runBaseAff'
    <<< Response.run
    <<< runRouting "localhost:10000" routeDuplex req
    <<< Crypto.run "seceret"
    <<< Cookies.run
    <<< Session.runInMemory db.session { user: Nothing }
    <<< runMessage
    <<< runMailer
    <<< runRegistration db.user
    <<< runAuth db.user $ do
      bind route $ router >=> (case_
          # on _root identity
          # on _auth Auth.Testing.Templates.render
          # on _registration Registration.Testing.Templates.render)

main ∷ Effect Unit
main = do
  let
    port = 10000
  db ← { session: _, user: _ } <$> Ref.new mempty <*> Ref.new mempty
  void $ HTTPure.serve port (app db) $ log ("Run app on: " <> show port)
  -- void $ HTTPure.serve port (\_ → Effect.Class.liftEffect random >>= show >>> HTTPure.ok) $ log ("HTTPure baseline")
  -- server ← baseline
  -- HTTP.listen server { backlog: Nothing, hostname: "127.0.0.1", port } (log "baseline")
