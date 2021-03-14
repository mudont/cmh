# Elm App

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).

## Building

Development build
```
ELM_DEBUGGER=true elm-app build   
```

Production build
```
elm-app build   
```

### elm examples/fractal architecture notes
import deps

Update, View, Subscriptions ->
    Messages
    Model

Model -> 
    Messages
Messages -> None

## Elm SPA notes

Model:
```elm
type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Settings Settings.Model
    | Login Login.Model
    | Register Register.Model
    | Profile Username Profile.Model
    | Article Article.Model
    | Editor (Maybe Slug) Editor.Model
```