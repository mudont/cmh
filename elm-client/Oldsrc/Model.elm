module Model exposing (..)

import Browser.Navigation exposing (Key)
import Fun.Model exposing (..)
import Header.Model exposing (..)
import Login.Model exposing (..)
import Url exposing (Url)


type alias Model =
    { header : Header.Model.Model
    , login : Login.Model.Model
    , fun : Fun.Model.Model
    , wsMsg : String
    --, key : Key
    --, url : Url
    }
type Route
    = Register
    | Events
    | Event  Int
    | Players
    | Fun
