port module Api exposing (
    Cred, addServerError, application, decodeErrors, delete, get, login, logout,
    post, put, register, resetPassword, profile, storeCredWith, username, viewerChanges,
    sendWsMessage, wsMessageReceiver, googleLogin, getTask)

{-| This module is responsible for communicating to the Conduit API.

It exposes an opaque Endpoint type which is guaranteed to point to the correct URL.

-}

import Api.Endpoint as Endpoint exposing (Endpoint)
import Avatar exposing (Avatar)
import Browser
import Browser.Navigation as Nav
import Http exposing (Body, Error, Expect, Response)
import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Platform exposing (Task)
import Url exposing (Url)
import Username exposing (Username)
import Email exposing (Email)


-- CRED


{-| The authentication credentials for the Viewer (that is, the currently logged-in user.)

This includes:

  - The cred's Username
  - The cred's authentication token

By design, there is no way to access the token directly as a String.
It can be encoded for persistence, and it can be added to a header
to a HttpBuilder for a request, but that's it.

This token should never be rendered to the end user, and with this API, it
can't be!

-}
type Cred
    = Cred Username String


username : Cred -> Username
username (Cred val _) =
    val


credHeader : Cred -> Http.Header
credHeader (Cred _ str) =
    Http.header "Authorization" ("Bearer " ++ str)


{-| It's important that this is never exposed!

We expose `login` and `application` instead, so we can be certain that if anyone
ever has access to a `Cred` value, it came from either the login API endpoint
or was passed in via flags.

-}
credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "username" Username.decoder
        |> required "token" Decode.string



-- PERSISTENCE


decode : Decoder (Cred -> viewer) -> Value -> Result Decode.Error viewer
decode decoder value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Cred -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Cred -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storeCredWith : Cred -> Avatar -> Cmd msg
storeCredWith (Cred uname token) avatar =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "username", Username.encode uname )
                        , ( "token", Encode.string token )
                        , ( "image", Avatar.encode avatar )
                        ]
                  )
                ]
    in
    storeCache (Just json)


logout : Cmd msg
logout =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg



-- SERIALIZATION
-- APPLICATION


application :
    Decoder (Cred -> viewer)
    ->
        { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application viewerDecoder config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    Decode.field "user" (decoderFromCred viewerDecoder)



-- HTTP


get : Endpoint -> Maybe Cred -> Decoder a -> (Result Error a -> msg) -> Cmd msg
get url maybeCred decoder toMsg =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


getTask : Endpoint -> Maybe Cred -> Decoder a  -> Task Error (Response String)
getTask url maybeCred decoder  =
    Endpoint.task
        { method = "GET"
        , url = url
        -- , expect = Http.expectJson toMsg decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = Http.emptyBody
        , timeout = Nothing
        --, tracker = Nothing
        }


put : Endpoint -> Cred -> Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
put url cred body decoder toMsg =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


post : Endpoint -> Maybe Cred -> Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
post url maybeCred body decoder toMsg =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Endpoint -> Cred -> Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
delete url cred body decoder toMsg =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


login : Http.Body -> Decoder (Cred -> a) -> (Result Error a -> msg) -> Cmd msg
login body decoder toMsg =
    post Endpoint.login Nothing body (decoderFromCred decoder) toMsg

googleLogin : Decoder (Cred -> a) -> (Result Error a -> msg) -> Cmd msg
googleLogin decoder toMsg =
    get Endpoint.googleLogin Nothing  (decoderFromCred decoder) toMsg


register : Http.Body -> Decoder (Cred -> a) -> (Result Error a -> msg) -> Cmd msg
register body decoder toMsg =
    post Endpoint.register Nothing body (decoderFromCred decoder) toMsg

resetPassword : String -> Decoder String -> (Result Error String -> msg) -> Cmd msg
resetPassword email decoder toMsg =
    get (Endpoint.resetPassword  email) Nothing  decoder toMsg

profile : Cred -> Http.Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
profile cred body decoder toMsg =
    put (Endpoint.profile (username cred)) cred body  decoder toMsg


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder



-- ERRORS


addServerError : List String -> List String
addServerError list =
    "Server error" :: list


{-| Many API endpoints include an "errors" field in their BadStatus responses.
-}
decodeErrors : Http.Error -> List String
decodeErrors error =
    case error of
        Http.BadUrl str -> ["Bad URL: " ++ str]
        Http.Timeout -> ["Server Timeout"]
        Http.NetworkError -> ["Network Error"]
        Http.BadBody str -> ["HTTP error Bad Body: " ++ str]
        Http.BadStatus code -> ["Http err status: " ++ String.fromInt code]


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors



-- LOCALSTORAGE KEYS


cacheStorageKey : String
cacheStorageKey =
    "cache"


credStorageKey : String
credStorageKey =
    "cred"

-- WEBSOCKETS
port wsMessageReceiver : (String -> msg) -> Sub msg
port sendWsMessage : String -> Cmd msg