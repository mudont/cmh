module Login.Model exposing (..)

import Login.Messages exposing (..)


type alias Model =
    { username : String
    , password : String
    , email : String
    , token : String
    , errorMsg : String
    }
