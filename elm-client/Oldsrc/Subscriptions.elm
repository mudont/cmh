port module Subscriptions exposing (..)

import Header.Subscriptions
import Messages exposing (..)
import Model exposing (..)


port wsMessageReceiver : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        headerSub =
            Header.Subscriptions.subscriptions model.header
    in
    Sub.batch
        [ Sub.map HeaderMsg headerSub
        , wsMessageReceiver WsRecv
        ]
