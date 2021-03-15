module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (errorToString)
import Loading
import Log
import Page
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)
import Tennis.Player as Player exposing(..)
import Tennis.Event as Event exposing(..)
import Util exposing (httpErrorToString)


-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , status : String
    , tab : HomeTab

    -- Loaded independently from server
    , players : Status Player.Model
    , events : Status Event.Model
    }


type HomeTab
    = EventTab
    | PlayerTab
    | MatchTab

type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed



init : Session -> ( Model, Cmd Msg )
init session =

    ( { session = session
      , timeZone = Time.utc
      , status = ""
      , tab = case Session.cred session of
                Just _ ->
                    EventTab
                Nothing ->
                    PlayerTab
      , players = Loading
      , events = Loading
      }
    , Cmd.batch
        [ fetchPlayers session PlayerTab CompletedPlayerLoad
        , fetchEvents session EventTab CompletedEventLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW

viewPlayers: Model -> List (Html Msg)
viewPlayers model =
    case model.players of
        Loaded players ->
            [ div [ class "feed-toggle" ] <|
                List.concat
                    [ [ viewTabs (Session.cred model.session) model.tab
                      ],
                      Player.viewPlayers model.timeZone players
                        |> List.map (Html.map GotPlayerMsg)
                    ]
            ]
        Loading ->
            []

        LoadingSlowly ->
            [ Loading.icon ]

        Failed ->
            [ Loading.error  <| "players. " ++ model.status ]

viewEvents: Model -> List (Html Msg)
viewEvents model =
    case model.events of
        Loaded events ->
            [ div [ class "feed-toggle" ] <|
                List.concat
                    [ [ viewTabs (Session.cred model.session) model.tab
                      ],

                        Event.viewEvents model.timeZone events
                        |> List.map (Html.map GotEventMsg)

                    ]
            ]
        Loading ->
            []

        LoadingSlowly ->
            [ Loading.icon ]

        Failed ->
            [ Loading.error <| "events. " ++ model.status]

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "CM Hackers"
    , content =
        div [ class "home-page" ]
            [ -- viewBanner,
              div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-12" ] <|
                    case model.tab of
                        PlayerTab -> viewPlayers model
                        EventTab -> viewEvents model
                        MatchTab -> []

                    ]
                ]
            ]
    }


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "CM Hackers" ]
            ]
        ]



-- TABS



eventTab : ( String, Msg )
eventTab =
    ( "Events", ClickedTab EventTab )


playerTab : ( String, Msg )
playerTab =
    ( "Players", ClickedTab PlayerTab )


matchTab : ( String, Msg )
matchTab  =
    ( "Matches" , ClickedTab (MatchTab) )

viewTabs : Maybe Cred -> HomeTab -> Html Msg
viewTabs maybeCred tab =
    case tab of
        PlayerTab ->
            Player.viewTabs [eventTab ] playerTab [ matchTab ]

        EventTab->
            Event.viewTabs [] eventTab  [playerTab, matchTab ]

        MatchTab ->
            Player.viewTabs [eventTab , playerTab] matchTab []

viewTab : List (Attribute msg) -> ( String, msg ) -> Html msg
viewTab attrs ( name, msg ) =
    li [ class "nav-item" ]
        [ -- Note: The RealWorld CSS requires an href to work properly.
          a (class "nav-link" :: onClick msg :: href ("#" {-++name-}) :: attrs)
            [ text name ]
        ]


-- UPDATE


type Msg
    = ClickedTab HomeTab
    | GotSession Session
    | PassedSlowLoadThreshold
    | CompletedPlayerLoad (Result Http.Error (List Player.Player))
    | GotPlayerMsg Player.Msg
    | CompletedEventLoad (Result Http.Error (List Event.Event))
    | GotEventMsg Event.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTab tab ->
            ( { model | tab = tab }
            , Cmd.none
            )
        CompletedPlayerLoad (Err err) -> ({model | status = httpErrorToString err, players = Failed}, Cmd.none)
        CompletedPlayerLoad (Ok ps) ->
            let playerModel = Player.init model.session ps
            in
              ({model | players = Loaded playerModel}, Cmd.none) -- TODO Do somthing here, or rewrite
        GotPlayerMsg subMsg ->
            case model.players of
                Loaded players ->
                    let
                        ( newPlayers, subCmd ) =
                            Player.update (Session.cred model.session) subMsg players
                    in
                    ( { model | players = Loaded players }
                    , Cmd.map GotPlayerMsg subCmd
                    )

                Loading ->
                    ( model, Log.error "Player msg Loading")

                LoadingSlowly ->
                    ( model, Log.error  "Player msg LoadingSlowly")

                Failed ->
                    ( model, Log.error  "Player msg Failed")

        CompletedEventLoad (Err err) -> ({model | status = httpErrorToString err, events = Failed}, Cmd.none)
        CompletedEventLoad (Ok es) ->
            let eventModel = Event.init model.session es
            in
              ({model | events = Loaded eventModel}, Cmd.none) -- TODO Do somthing here, or rewrite
        GotEventMsg subMsg ->
            let addlCmd = case subMsg of
                        SignupCompleted (Ok _) -> fetchEvents model.session EventTab CompletedEventLoad
                        _ -> Cmd.none
                (m, cmd)  =
                  case model.events of
                    Loaded events ->
                        let
                            ( newEvents, subCmd ) =
                                Event.update (Session.cred model.session) subMsg events
                        in
                        ( { model | events = Loaded events }
                        , Cmd.map GotEventMsg subCmd
                        )

                    Loading ->
                        ( model, Log.error "Event msg Loading")

                    LoadingSlowly ->
                        ( model, Log.error  "Event msg LoadingSlowly")

                    Failed ->
                        ( model, Log.error  "Event msg Failed")
            in (m, Cmd.batch [addlCmd, cmd])
        GotSession sess ->
            ( { model | session = sess }, Log.dbg "DBG--- session changed" )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                players =
                    case model.players of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
                events =
                    case model.events of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | players = players, events = events }, Cmd.none )




-- HTTP

fetchPlayers : Session -> HomeTab ->  (Result Http.Error (List Player) -> msg) -> Cmd msg
fetchPlayers session feedTabs resToMsg =
    let
        maybeCred =
            Session.cred session

        decoder =
            Player.decoder maybeCred 100


        req =
                    Api.get Endpoint.players maybeCred decoder
    in
      req resToMsg
    -- Cmd.map (Player.init session) (req msg)
        -- |> Task.map (Player.init session)


fetchEvents : Session -> HomeTab ->  (Result Http.Error (List Event) -> msg) -> Cmd msg
fetchEvents session feedTabs resToMsg =
    let
        maybeCred =
            Session.cred session

        decoder =
            Event.decoder maybeCred 100


        req =
                    Api.get Endpoint.events maybeCred decoder
    in
      req resToMsg


scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
