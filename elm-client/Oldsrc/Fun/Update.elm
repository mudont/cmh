module Fun.Update exposing (..)

import Fun.Messages exposing (..)
import Fun.Model exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update token msg model =
    case msg of
        GetQuote ->
            ( model, fetchRandomQuoteCmd )

        FetchRandomQuoteCompleted result ->
            fetchRandomQuoteCompleted model result

        GetProtectedQuote ->
            ( model, fetchProtectedQuoteCmd token model )

        FetchProtectedQuoteCompleted result ->
            fetchProtectedQuoteCompleted model result



-- GET request for random protected quote (authenticated)


fetchProtectedQuoteCmd : String -> Model -> Cmd Msg
fetchProtectedQuoteCmd token model =
    { method = "GET"
    , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
    , url = protectedQuoteUrl
    , body = Http.emptyBody
    , expect = Http.expectString FetchProtectedQuoteCompleted
    , timeout = Nothing
    , tracker = Nothing

    -- , withCredentials = False
    }
        |> Http.request


fetchProtectedQuoteCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
fetchProtectedQuoteCompleted model result =
    case result of
        Ok newPQuote ->
            ( { model | quote = newPQuote }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )



-- API request URLs


api : String
api =
    "/"


protectedQuoteUrl : String
protectedQuoteUrl =
    api ++ "chuck"


randomQuoteUrl : String
randomQuoteUrl =
    api ++ "rajni"


init : ( Model, Cmd Msg )
init =
    ( { quote = ""
      }
    , fetchRandomQuoteCmd
    )



-- GET a random quote (unauthenticated)


fetchRandomQuoteCmd : Cmd Msg
fetchRandomQuoteCmd =
    Http.get
        { url = randomQuoteUrl
        , expect = Http.expectString FetchRandomQuoteCompleted
        }


fetchRandomQuoteCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
fetchRandomQuoteCompleted model result =
    case result of
        Ok newQuote ->
            ( { model | quote = newQuote }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )
