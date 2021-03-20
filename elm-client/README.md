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

## Elm SPA notes

### Architecture
public/index.html
  src/index.js
    src/Main.elm 
        main = Api.application Viewer.decoder
            subscriptions model -> Msg
            model, cmd = init 
            view Model -> Msg
            update Msg -> Model -> Model

update

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