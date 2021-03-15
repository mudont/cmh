module Tennis.Event exposing (
    Model, Msg(..), decoder, init, update, viewEvents,
    viewTabs, Event)
import Api exposing (Cred, username)
import Avatar exposing (Avatar)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, maybe)
import Json.Decode.Pipeline exposing (optional, required)
-- import Json.Decode.Field as Field
import Json.Encode as Encode
import Page
import Profile
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Username exposing (toString)
import Bootstrap.Table as Table
import Log
import Api.Endpoint as Endpoint
import Util exposing (httpErrorToString)
-- MODEL

type Model = Model Internals

type alias Event =
    { eventId : Int
    , date : Time.Posix
    , name : String
    ,  eventType: String
    ,  comment : String
    ,  alwaysShow : Bool
    ,  orgId : Int
    ,  leagueId : Int
    ,  myRsvp  :  String
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
                { options = [ Table.striped, Table.hover, Table.small ]
                , thead =  Table.simpleThead
                    [ Table.th [] [ text "Event" ]
                    , Table.th [] [ text "Date" ]
                    , Table.th [] [ text "Curr Rsvp" ]
                    , Table.th [] [ text "Rsvp" ]
                    ]
                , tbody =
                    Table.tbody [] <| List.map (viewPreview) <| events
                }
    in
    Page.viewErrors ClickedDismissErrors errors :: [eventsHtml]


viewPreview : Event -> Table.Row Msg
viewPreview event =
    Table.tr [  ]
        [ Table.td []
            [ a [ Route.href (Route.EventRsvps event.eventId) ]
                [ text event.name ]
            ]
        ,  Table.td [] [text <| String.map (\c -> if c == 'T' then ' ' else c) <| String.slice 0 16 <| Iso8601.fromTime event.date ]
        ,  Table.td [] [rsvpHtml event.myRsvp ]
        , Table.td []
          [ button [onClick <| Signup event.eventId "A"] [i [ class "ion-checkmark-circled" ] []]
          , text " "
          , button [onClick <| Signup event.eventId "N"] [i [ class "ion-minus-circled" ] []]
          ]
        ]

rsvpHtml : String -> Html Msg
rsvpHtml code =
    case code of
        "A" -> i [ class "ion-checkmark-circled" ] []
        "N" -> i [ class "ion-minus-circled" ] []
        _ -> text code

viewTabs :
    List ( String, msg )
    -> ( String, msg )
    -> List ( String, msg )
    -> Html msg
viewTabs before selected after =
    ul [ class "nav nav-pills outline-active" ] <|
        List.concat
            [ List.map (viewTab []) before
            , [ viewTab [ class "active" ] selected ]
            , List.map (viewTab []) after
            ]


viewTab : List (Attribute msg) -> ( String, msg ) -> Html msg
viewTab attrs ( name, msg ) =
    li [ class "nav-item" ]
        [ -- Note: The RealWorld CSS requires an href to work properly.
          a (class "nav-link" :: onClick msg :: href "#" :: attrs)
            [ text name ]
        ]


viewPagination : (Int -> msg) -> Int -> Model -> Html msg
viewPagination toMsg page (Model event) =
    let
        viewPageLink currentPage =
            pageLink toMsg currentPage (currentPage == page)

        totalPages = 1

    in
    if totalPages > 1 then
        List.range 1 totalPages
            |> List.map viewPageLink
            |> ul [ class "pagination" ]

    else
        Html.text ""


pageLink : (Int -> msg) -> Int -> Bool -> Html msg
pageLink toMsg targetPage isActive =
    li [ classList [ ( "page-item", True ), ( "active", isActive ) ] ]
        [ a
            [ class "page-link"
            , onClick (toMsg targetPage)

            -- The RealWorld CSS requires an href to work properly.
            , href "#"
            ]
            [ text (String.fromInt targetPage) ]
        ]


viewTag : String -> Html msg
viewTag tagName =
    li [ class "tag-default tag-pill tag-outline" ] [ text tagName ]



-- UPDATE


type Msg
    = ClickedDismissErrors
    | Signup Int String
    | SignupCompleted (Result Http.Error Int)


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )
        Signup eventId response ->
          case maybeCred of
            Nothing -> (Model model, Log.dbg <| " Unauthorized Signup " ++ String.fromInt eventId ++ " " ++ response)
            Just cred ->
              (Model model, Cmd.batch
              [ Log.dbg <| "Signup " ++ String.fromInt eventId ++ " " ++ response
              , Api.post (Endpoint.eventRsvp) maybeCred (Http.jsonBody <| Encode.object [ ("eventId", Encode.int eventId)
                              , ("response", Encode.string response)
                              , ("comment", Encode.string "")
                              , ("username", Encode.string  <| toString <| username cred)
                              ]) (Decode.succeed 0) SignupCompleted
              ])
        SignupCompleted result ->
            case result of
                Ok _ -> (Model model, Log.dbg "Signup OK")
                Err err -> (Model model, Log.dbg <| httpErrorToString err)


-- SERIALIZATION


eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed Event
        |> required "eventId" Decode.int
        |> required "date" Iso8601.decoder
        |> required "name" Decode.string
        |> required "eventType" Decode.string
        |> required "comment" Decode.string
        |> required "alwaysShow" Decode.bool
        |> required "orgId" Decode.int
        |> optional "leagueId" Decode.int 0
        |> optional "myRsvp" Decode.string ""

decoder : Maybe Cred -> Int -> Decoder (List Event)
decoder maybeCred resultsPerPage =
    Decode.list eventDecoder


