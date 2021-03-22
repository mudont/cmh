module Tennis.Match exposing (
    Model, Msg(..), decoder, init, update, viewMatches,
    Match)
import Api exposing (Cred, username)
import Css exposing (width)
import Html exposing (..)
import Html.Attributes exposing (class, colspan, id, placeholder, style, type_, value)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, nullable)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import List exposing (filter, head)
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
    , clickedMatchId: Maybe Int
    , isLoading: Bool
    }


init : Session -> List Match -> Model
init session matches =
    Model
        { session = session
        , errors = []
        , matches = matches
        , isLoading = False
        , clickedMatchId = Nothing
        }

-- VIEW

viewMatches : Time.Zone -> Model -> List (Html Msg)
viewMatches timeZone (Model { matches, session, errors , clickedMatchId}) =
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
                    Table.tbody [] <| List.map (viewMatch clickedMatchId) matches
                }
    in
    Page.viewErrors ClickedDismissErrors errors :: [matchesHtml]

viewMatch : Maybe Int -> Match -> Table.Row Msg
viewMatch mClickedId m =
    if mClickedId == Just m.matchId
    then viewUpdateMatch m
    else viewPreview m

viewPreview : Match -> Table.Row Msg
viewPreview match =
  let
     home = if match.homePlayer2 /= "" then
                match.homePlayer1 ++ " & " ++ match.homePlayer2
            else match.homePlayer1
     away = if match.awayPlayer2 /= "" then
                match.awayPlayer1 ++ " & " ++ match.awayPlayer2
            else match.awayPlayer1
     winner = case match.homeWon of
                 Nothing -> ""
                 Just True -> "Home"
                 Just False -> "Away"
  in Table.tr [ Table.rowAttr (onDoubleClick <| MatchClicked match.matchId)]
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

viewUpdateMatch : Match -> Table.Row Msg
viewUpdateMatch match =
  let
     home = if match.homePlayer2 /= "" then
                match.homePlayer1 ++ " & " ++ match.homePlayer2
            else match.homePlayer1
     away = if match.awayPlayer2 /= "" then
                match.awayPlayer1 ++ " & " ++ match.awayPlayer2
            else match.awayPlayer1
     winner = case match.homeWon of
                 Nothing -> ""
                 Just True -> "Home"
                 Just False -> "Away"
  in
    Table.tr
        [ Table.rowAttr (onDoubleClick <| MatchClicked match.matchId)]
        [ Table.td []
            [ text <| String.fromInt match.matchId
            ]
        ,  Table.td [] [text <| toIsoString match.date ]
        ,  Table.td [] [text home ]
        ,  Table.td [] [text away ]
        ,  Table.td [] [text winner ]
        ,  Table.td [] [input [ type_ "text"
                            , id "score-input"
                            , placeholder "Score"
                            , style "width" "100%"
                            , value match.score
                            , onInput (MatchScore match.matchId)
                            , onBlur (SaveMatch match.matchId)
                            ] []]
        ,  Table.td [] [text match.comment ]
        ,  Table.td [] [text <| String.fromInt match.roundNum ]
        ,  Table.td [] [text <| String.fromInt match.matchNum ]
        ]

-- UPDATE


type Msg
    = ClickedDismissErrors
    | MatchClicked Int
    | MatchScore Int String
    | SaveMatch Int
    | SaveCompleted (Result Http.Error Int)


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )
        MatchClicked matchId ->
            ( Model
                { model | clickedMatchId =
                    if Maybe.map (\cmi -> cmi == matchId) model.clickedMatchId == Just True
                    then Nothing
                    else Just matchId
                }
            , Cmd.none )
        MatchScore matchId score ->
            ( Model
                { model | matches = List.map (setScore matchId score) model.matches
                }
            , Cmd.none)
        SaveMatch matchId ->
          case maybeCred of
            Nothing -> (Model model, Log.dbg <| " Unauthorized Match save " ++ String.fromInt matchId )
            Just cred ->
                ( Model model
                , let
                    mmatch = head <| filter (\m -> m.matchId == matchId) model.matches
                  in Cmd.batch
                    [ Log.dbg <| "Match save " ++ String.fromInt matchId
                    , case mmatch of
                        Nothing -> Log.dbg <| "Tried to save invalid Match id " ++ String.fromInt matchId
                        Just match -> Api.put (Endpoint.match (Just matchId)) cred (Http.jsonBody <|
                            Encode.object [ ("matchId", Encode.int matchId)
                                    , ("date", Encode.string <| Date.toIsoString match.date)
                                    , ("league", Encode.string match.league)
                                    , ("homePlayer1", Encode.string match.homePlayer1)
                                    , ("homePlayer2", Encode.string match.homePlayer2)
                                    , ("awayPlayer1", Encode.string match.awayPlayer1)
                                    , ("awayPlayer2", Encode.string match.awayPlayer2)
                                    , ("homeWon", (Encode.maybe Encode.bool) match.homeWon)
                                    , ("score", Encode.string match.score)
                                    , ("comment", Encode.string match.comment)
                                    , ("roundNum", Encode.int match.roundNum)
                                    , ("matchNum", Encode.int match.matchNum)
                                    ]) (Decode.succeed 0) SaveCompleted
                    ]
                )
        SaveCompleted result ->
            case result of
                Ok _ -> (Model {model| clickedMatchId = Nothing}, Log.dbg "Match Saved OK")
                Err err -> ( Model {model|errors = List.append model.errors ["Error saving match: " ++ httpErrorToString err]}
                           , Log.dbg <| httpErrorToString err)


setScore : Int -> String -> Match -> Match
setScore matchId score match =
    if matchId == match.matchId
    then {match|score=score}
    else match

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


