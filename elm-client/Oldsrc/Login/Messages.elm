module Login.Messages exposing (..)

import Http


type Msg
    = SetUsername String
    | SetPassword String
    | SetEmail String
    | ClickRegisterUser
    | ClickLogIn
    | GetTokenCompleted (Result Http.Error String)
    | ClickGoogleLogIn
    | LogOut
