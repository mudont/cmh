port module Update exposing (..)

import Browser
import Browser.Navigation as Nav exposing (Key)
import Fun.Update
import Header.Update
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Login.Model
import Login.Update
import Messages exposing (..)
import Model exposing (..)
import PortFunnel exposing (sendMessage)
import Url exposing (Url)
import Url.Parser exposing (Parser, int, map, oneOf, s, string, (</>))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --LinkClicked urlRequest ->
        --  case urlRequest of
        --    Browser.Internal url ->
        --      ( model, Nav.pushUrl model.key (Url.toString url) )
        --
        --    Browser.External href ->
        --      ( model, Nav.load href )
        --
        --UrlChanged url ->
        --  ( { model | url = url }
        --  , Cmd.none
        --  )


        WsRecv _ ->
            ( model, Cmd.none )

        WsSend ->
            ( model, sendWsMessage model.wsMsg )

        HeaderMsg headerMsg ->
            let
                ( headerModel, headerCmd ) =
                    Header.Update.update headerMsg model.header
            in
            ( { model | header = headerModel }
            , Cmd.map HeaderMsg headerCmd
            )

        LoginMsg loginMsg ->
            let
                ( loginModel, loginCmd ) =
                    Login.Update.update loginMsg model.login
            in
            ( { model | login = loginModel }
            , Cmd.map LoginMsg loginCmd
            )

        FunMsg funMsg ->
            let
                ( funModel, funCmd ) =
                    Fun.Update.update model.login.token funMsg model.fun
            in
            ( { model | fun = funModel }
            , Cmd.map FunMsg funCmd
            )



-- Helper to update model and set localStorage with the updated model


init : Login.Model.Model  -> ( Model, Cmd Msg )
init model =
    let

        ( headerState, headerCmd ) =
            Header.Update.init

        ( loginState, loginCmd ) =
            Login.Update.init model

        ( funState, funCmd ) =
            Fun.Update.init
    in
    ( { header = headerState
      , login = loginState
      , fun = funState
      , wsMsg = "For ws"
      --, key = key
      --, url = url
      }
    , Cmd.batch
        [ Cmd.map HeaderMsg headerCmd
        , Cmd.map LoginMsg loginCmd
        , Cmd.map FunMsg funCmd
        ]
    )

urlParser : Parser (Route -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map Register (s "register")
        , Url.Parser.map Events (s "events")
        , Url.Parser.map Event (s "event" </>  Url.Parser.int )
        ]
-- PORTS


port sendWsMessage : String -> Cmd msg
