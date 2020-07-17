# Webrow guide + app

WIP. Some deps not published yet.

## Objectives

* Build a simple proof of concept application based on _webrow_.

* Tag steps to track the progress.

* Sketch a webrow guide along the way.

* Track performance when adding more app layers.


<!--

# Starting up

Installs `nodemon`:

```
$ npm install
```

## Routes

We need to define a type for our rows

type Routes = Variant
  ( "int" :: Int
  , "string" :: String
  )

and a parser / serializer for our rows

route ∷ D.RouteDuplex' Routes
route = D.root $ RouteDuplex.Variant.variant' routes
  where
    routes =
      { "int": D.int D.segment
      , "string": D.string D.segment
      }


-->
