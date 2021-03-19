module Tennis.EventRsvps exposing (
    Model, Msg(..), decoder, init, update, view,
    Event,toSession, subscriptions)
import Api exposing (Cred, username)
import Html exposing (..)
import Html.Attributes exposing ( class)
import Html.Events exposing (onClick)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Loading
import Page
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time
import Username exposing (toString)
import Bootstrap.Table as Table
import Log
import Api.Endpoint as Endpoint
import Util exposing (dateHhMm, httpErrorToString, rsvpHtml)

-- MODEL

type Model = Model Internals
type alias EventRsvp =
    { eventId: Int
    , username: String
    , response: String
    , comment: String
    }
type alias Event =
    { eventId : Int
    , date : Time.Posix
    , name : String
    ,  eventType: String
    ,  comment : String
    ,  alwaysShow : Bool
    ,  orgId : Int
    ,  leagueId : Int
    ,  myRsvp: String
    }
type alias EventWithRsvps =
    { event: Event
    , rsvps: List EventRsvp
    }
type alias Internals =
    { session: Session
    , status: String
    , errors: List String
    , eventWithRsvps: Status EventWithRsvps
    , isLoading: Bool
    }


type Status a
    = Loading Int
    | LoadingSlowly
    | Loaded a
    | Failed

init : Session -> Int -> (Model, Cmd Msg)
init session eventId =
    ( Model
        { session = session
        , errors = []
        , eventWithRsvps = Loading eventId
        , isLoading = False
        , status = ""
        }
    , Cmd.batch
        [ fetchEventRsvps session eventId CompletedLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )


-- HTTP

fetchEventRsvps : Session -> Int ->  (Result Http.Error  EventWithRsvps -> msg) -> Cmd msg
fetchEventRsvps session eventId resToMsg =
    let
        maybeCred =
            Session.cred session
        req =
                    Api.get (Endpoint.eventRsvps  eventId) maybeCred  (decoder maybeCred)
    in
      req resToMsg

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "CM Hackers Event Rsvps"
    , content =
        div [ class "home-page" ]
            [ -- viewBanner,
              div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-12" ] <|
                        [ div [ class "feed-toggle" ] <|
                            viewEventWithRsvps model
                        ]
                    ]
                ]
            ]
    }


viewEventWithRsvps : Model -> List (Html Msg)
viewEventWithRsvps (Model { eventWithRsvps, session, errors, status }) =
  case eventWithRsvps of
      Loaded ewrs ->
        let
            eventHtml =
              [ viewEvent ewrs.event
              , Table.table
                    { options = [ Table.striped, Table.hover, Table.small ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ text "Curr Rsvp" ]
                        , Table.th [] [ text "User" ]
                        , Table.th [] [ text "Comment" ]
                        ]
                    , tbody =
                        Table.tbody [] <| List.map viewRsvp <| ewrs.rsvps
                    }
               ]
        in
        Page.viewErrors ClickedDismissErrors errors :: eventHtml
      Loading _ -> []
      LoadingSlowly -> [Loading.icon]
      Failed -> [ Loading.error  <| "event rspvs. " ++ status ]

viewEvent: Event -> Html Msg
viewEvent event =
    Table.table
      { options=[]
      , thead = Table.simpleThead []
      , tbody = Table.tbody []
        [ Table.tr []
            [ Table.td [] [ text <| String.fromInt event.eventId]
            , Table.td [] [ text <| dateHhMm event.date]
            , Table.td [] [ text event.name]
            , Table.td [] [ text event.comment]
            ]
        ]
      }
viewRsvp : EventRsvp -> Table.Row Msg
viewRsvp rsvp =
    Table.tr [  ]
        [ Table.td [] [rsvpHtml rsvp.response ]
        ,  Table.td []
            [ text rsvp.username
            ]
        ,  Table.td [] [text rsvp.comment ]
        ]

-- UPDATE


type Msg
    = ClickedDismissErrors
    | GotSession Session
    | PassedSlowLoadThreshold
    | CompletedLoad (Result Http.Error EventWithRsvps)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )
        GotSession sess ->
            ( Model { model | session = sess }, Log.dbg "DBG--- eventWithRsvps session changed" )
        CompletedLoad (Err err) -> ( Model {model | status =
                                            httpErrorToString err,
                                            eventWithRsvps = Failed}
                                    , Cmd.none
                                    )
        CompletedLoad (Ok res) ->
              (Model {model | eventWithRsvps = Loaded res}, Cmd.none)
        PassedSlowLoadThreshold ->
          let
            ewrs = case model.eventWithRsvps of
                Loading _ -> LoadingSlowly
                other -> other
          in (Model {model| eventWithRsvps= ewrs}, Cmd.none)


-- SERIALIZATION

rsvpDecoder : Decoder EventRsvp
rsvpDecoder =
    Decode.succeed EventRsvp
        |> required "eventId" Decode.int
        |> required "username" Decode.string
        |> required "response" Decode.string
        |> optional "comment" Decode.string ""

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

decoder : Maybe Cred -> Decoder (EventWithRsvps)
decoder maybeCred =
    Decode.succeed EventWithRsvps
      |> required "event" eventDecoder
      |> required "rsvps" (Decode.list rsvpDecoder)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession (Model {session})  =
    session
