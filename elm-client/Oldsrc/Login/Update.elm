port module Login.Update exposing (..)

import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Login.Messages exposing (..)
import Login.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        ClickRegisterUser ->
            ( model, authUserCmd model registerUrl )

        ClickLogIn ->
            ( model, authUserCmd model loginUrl )

        ClickGoogleLogIn ->
            ( model, googleLoginCmd )

        GetTokenCompleted result ->
            getTokenCompleted model result

        LogOut ->
            ( { model | username = "", token = "" }, removeStorage <| model )



-- POST register / login request


authUserCmd : Model -> String -> Cmd Msg
authUserCmd model apiUrl =
    let
        body =
            model
                |> userEncoder
                |> Http.jsonBody
    in
    Http.post { url = apiUrl, body = body, expect = Http.expectJson GetTokenCompleted (oneOf [ tokenDecoder2, tokenDecoder ]) }


getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newToken ->
            setStorageHelper { model | token = newToken, password = "", errorMsg = "" }

        Err _ ->
            ( { model | errorMsg = " Some Http.Error. Don't know how to display" }, Cmd.none )



-- Decode POST response to get access token
-- Should be called to process reponse of successful login
--


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "access_token" Decode.string


tokenDecoder2 : Decoder String
tokenDecoder2 =
    Decode.field "accessToken" Decode.string



-- Helper to update model and set localStorage with the updated model


setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage <| model )



-- API request URLs


api : String
api =
    "/"


registerUrl : String
registerUrl =
    api ++ "register"


loginUrl : String
loginUrl =
    api ++ "login"


googleLoginUrl : String
googleLoginUrl =
    api ++ "google"



-- Encode user to construct POST request body (for Register and Log In)


userEncoder : Model -> Encode.Value
userEncoder model =
    Encode.object
        [ ( "username", Encode.string model.username )
        , ( "password", Encode.string model.password )
        , ( "email", Encode.string model.email )
        ]


init : Model -> ( Model, Cmd Msg )
init m =
    ( m
    , Cmd.none
    )



-- GET a random quote (unauthenticated)


googleLoginCmd : Cmd Msg
googleLoginCmd =
    Http.get
        { url = googleLoginUrl
        , expect = Http.expectJson GetTokenCompleted <| oneOf [ tokenDecoder2, tokenDecoder ]
        }



-- Ports


port setStorage : Model -> Cmd msg


port removeStorage : Model -> Cmd msg
