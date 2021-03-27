module Tennis.Event exposing (
    Model, Msg(..), decoder, init, update, viewEvents,
    Event)
import Api exposing (Cred, username)
import Css exposing (width)
import Html exposing (..)
import Html.Attributes exposing (class, colspan, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
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

-- MODEL

type Model = Model Internals

type alias Event =
    { eventId : Int
    , date : Time.Posix
    , name : String
    , eventType: String
    , comment : String
    , alwaysShow : Bool
    , orgId : Int
    , leagueId : Int
    , myRsvp  :  String
    , editingRsvp : Bool
    , myRsvpComment : String
    }
type alias Internals =
    { session: Session
    , errors: List String
    , events: List Event
    , isLoading: Bool
    }


init : Session -> List Event -> Model
init session events =
    Model
        { session = session
        , errors = []
        , events = events
        , isLoading = False
        }

-- VIEW

viewEvents : Time.Zone -> Model -> List (Html Msg)
viewEvents timeZone (Model { events, session, errors }) =
    let
        eventsHtml =
            Table.table
                { options = [ Table.striped, Table.hover, Table.small, Table.responsiveMd ]
                , thead =  Table.thead [] [Table.tr []
                    [ Table.td [] [ text "Event" ]
                    , Table.td [] [ text "Date" ]
                    , Table.td [] [ text "Curr Rsvp" ]
                    , Table.td [] [ text "Rsvp" ]
                    ]]
                , tbody =
                    Table.tbody [] <| List.concatMap (viewPreview) <| events
                }
    in
    Page.viewErrors ClickedDismissErrors errors :: [eventsHtml]


viewPreview : Event -> List (Table.Row Msg)
viewPreview event =
  Table.tr [  ]
        [ Table.td []
            [ a [ Route.href (Route.EventRsvps event.eventId) ]
                [ text event.name ]
            ]
        ,  Table.td [] [text <| dateHhMm event.date ]
        ,  Table.td [] [rsvpHtml event.myRsvp ]
        , viewRsvpActions event
        ]
    :: viewRsvpDetailedEditor event

viewRsvpActions : Event -> Table.Cell Msg
viewRsvpActions event =

    Table.td [] <|
        if event.editingRsvp
        then
          -- Nothing here, because another row will follow with the action buttons
          []
        else
          [ button [onClick <| Signup event.eventId "A" ""] [rsvpHtml "A"]
          , text " "
          , button [onClick <| Signup event.eventId "N" ""] [rsvpHtml "N"]
          , text " "
          , button [onClick <| EditingRsvp event.eventId] [iconMore]
          ]

viewRsvpDetailedEditor : Event -> List (Table.Row Msg)
viewRsvpDetailedEditor event =
    if event.editingRsvp then
      [ Table.tr [  ]
            [ Table.td [Table.cellAttr (colspan 3)]
                [ input [ type_ "text"
                        , placeholder "Comment"
                        , value event.myRsvpComment
                        , style "width" "100%"
                        , onInput (RsvpComment event.eventId)
                        ] []
                ]
            , Table.td []
              [ button [onClick <| Signup event.eventId "A" event.myRsvpComment] [rsvpHtml "A"]
              , text " "
              , button [onClick <| Signup event.eventId "N" event.myRsvpComment] [rsvpHtml "N"]
              , text " "
              , button [onClick <| Signup event.eventId "1" event.myRsvpComment] [rsvpHtml "1"]
              ]
            ]
      ]
    else
      []
-- UPDATE


type Msg
    = ClickedDismissErrors
    | Signup Int String String
    | SignupCompleted (Result Http.Error Int)
    | EditingRsvp Int
    | RsvpComment Int String


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )
        Signup eventId response comment->
          case maybeCred of
            Nothing -> (Model model, Log.dbg <| " Unauthorized Signup " ++ String.fromInt eventId ++ " " ++ response)
            Just cred ->
              (Model model, Cmd.batch
              [ Log.dbg <| "Signup " ++ String.fromInt eventId ++ " " ++ response ++ " / " ++ comment
              , Api.post (Endpoint.eventRsvp) maybeCred (Http.jsonBody <| Encode.object [ ("eventId", Encode.int eventId)
                              , ("response", Encode.string response)
                              , ("comment", Encode.string comment)
                              , ("username", Encode.string  <| toString <| username cred)
                              ]) (Decode.succeed 0) SignupCompleted
              ])
        SignupCompleted result ->
            case result of
                Ok _ -> (Model model, Log.dbg "Signup OK")
                Err err -> ( Model {model|errors = List.append model.errors [httpErrorToString err]}
                           , Log.dbg <| httpErrorToString err)

        EditingRsvp eventId ->( Model {model| events = List.map (setEditing eventId) model.events}
                              , Log.dbg <| "Editing Rsvp Comment for " ++ String.fromInt eventId)
        RsvpComment eventId comment ->
            ( Model {model| events = List.map (setComment eventId comment) model.events}
            , Log.dbg <| "Got RsvpComment " ++ comment)

setEditing : Int -> Event -> Event
setEditing eventId event = {event|editingRsvp = event.eventId == eventId}

setComment : Int -> String -> Event -> Event
setComment eventId comment event = {event|myRsvpComment =
            if event.eventId == eventId then comment else event.myRsvpComment}

-- SERIALIZATION


eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed Event
        |> required "eventId" Decode.int
        |> required "date" Iso8601.decoder
        |> required "name" Decode.string
        |> required "eventType" Decode.string
        |> optional "comment" Decode.string ""
        |> required "alwaysShow" Decode.bool
        |> required "orgId" Decode.int
        |> optional "leagueId" Decode.int 0
        |> optional "myRsvp" Decode.string ""
        |> optional "editingRsvp" (Decode.succeed False) False
        |> optional "myRsvpComment" Decode.string ""

decoder : Maybe Cred -> Int -> Decoder (List Event)
decoder maybeCred resultsPerPage =
    Decode.list eventDecoder


