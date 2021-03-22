module Tennis.Match exposing (
    Model, Msg(..), decoder, init, update, viewMatches,
    Match)
import Api exposing (Cred, username)
import Css exposing (width)
import Html exposing (..)
import Html.Attributes exposing (class, colspan, placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, nullable)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Page
import Route exposing (Route)
import Session exposing (Session)
import Time
import Username exposing (toString)
import Bootstrap.Table as Table
import Log
import Api.Endpoint as Endpoint
import Util exposing (dateHhMm, httpErrorToString, iconMore, rsvpHtml)
import Date exposing (..)

-- MODEL

type Model = Model Internals

type alias Match =
    { matchId : Int
    , date : Date
    , league : String
    , homePlayer1: String
    , homePlayer2: String
    , awayPlayer1: String
    , awayPlayer2: String
    , homeWon : Maybe Bool
    , score  :  String
    , comment : String
    , roundNum : Int
    , matchNum : Int
    }
type alias Internals =
    { session: Session
    , errors: List String
    , matches: List Match
    , isLoading: Bool
    }


init : Session -> List Match -> Model
init session matches =
    Model
        { session = session
        , errors = []
        , matches = matches
        , isLoading = False
        }

-- VIEW

viewMatches : Time.Zone -> Model -> List (Html Msg)
viewMatches timeZone (Model { matches, session, errors }) =
    let
        matchesHtml =
            Table.table
                { options = [ Table.striped, Table.hover, Table.small, Table.responsiveMd ]
                , thead =  Table.thead [] [Table.tr []
                    [ Table.td [] [ text "Id" ]
                    , Table.td [] [ text "Date" ]
                    , Table.td [] [ text "Home" ]
                    , Table.td [] [ text "Away" ]
                    , Table.td [] [ text "Winner" ]
                    , Table.td [] [ text "Score" ]
                    , Table.td [] [ text "Round" ]
                    , Table.td [] [ text "Match#" ]
                    ]]
                , tbody =
                    Table.tbody [] <| List.map viewPreview  matches
                }
    in
    Page.viewErrors ClickedDismissErrors errors :: [matchesHtml]


viewPreview : Match -> Table.Row Msg
viewPreview match =
  let
     home = if match.homePlayer2 /= "" then
                match.homePlayer1 ++ " & " ++ match.homePlayer2
            else match.homePlayer1
     away = if match.awayPlayer2 /= "" then
                match.awayPlayer1 ++ " & " ++ match.homePlayer2
            else match.awayPlayer1
     winner = case match.homeWon of
                 Nothing -> ""
                 Just True -> "Home"
                 Just False -> "Away"
  in Table.tr [  ]
        [ Table.td []
            [ text <| String.fromInt match.matchId
            ]
        ,  Table.td [] [text <| toIsoString match.date ]
        ,  Table.td [] [text home ]
        ,  Table.td [] [text away ]
        ,  Table.td [] [text winner ]
        ,  Table.td [] [text match.score ]
        ,  Table.td [] [text match.comment ]
        ,  Table.td [] [text <| String.fromInt match.roundNum ]
        ,  Table.td [] [text <| String.fromInt match.matchNum ]
        ]

-- UPDATE


type Msg
    = ClickedDismissErrors


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )

-- SERIALIZATION

dateDecoder : Decode.Decoder Date.Date
dateDecoder =
  Decode.string
    |> Decode.andThen ( \str ->
          case fromIsoString str of
            Err err -> Decode.fail err
            Ok date -> Decode.succeed date
        )


matchDecoder : Decoder Match
matchDecoder =
    Decode.succeed Match
        |> required "matchId" Decode.int
        |> required "date" dateDecoder
        |> required "league" Decode.string
        |> required "homePlayer1" Decode.string
        |> required "homePlayer2" Decode.string
        |> required "awayPlayer1" Decode.string
        |> required "awayPlayer2" Decode.string
        |> required "homeWon"  (nullable Decode.bool)
        |> required "score" Decode.string
        |> required "comment" Decode.string
        |> required "roundNum" Decode.int
        |> required "matchNum" Decode.int

decoder : Maybe Cred -> Int -> Decoder (List Match)
decoder maybeCred resultsPerPage =
    Decode.list matchDecoder


