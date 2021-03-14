module Messages exposing (..)

import Browser exposing (UrlRequest)
import Fun.Messages
import Header.Messages
import Http
import Login.Messages
import Url exposing (Url)


type Msg
    = HeaderMsg Header.Messages.Msg
    | FunMsg Fun.Messages.Msg
    | LoginMsg Login.Messages.Msg
    | WsSend
    | WsRecv String
    --| LinkClicked UrlRequest
    --| UrlChanged Url
