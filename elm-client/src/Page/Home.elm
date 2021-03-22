module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Loading
import Log
import Session exposing (Session)
import Task exposing (Task)
import Time
import Tennis.Player as Player exposing(..)
import Tennis.Event as Event exposing(..)
import Tennis.Match as Match exposing(..)
import Tennis.Info as Info exposing(..)
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
    , matches : Status Match.Model
    , info : Status Info.Model
    }


type HomeTab
    = InfoTab
    | EventTab
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
      , tab = InfoTab
      , players = Loading
      , events = Loading
      , matches = Loading
      , info = Loading
      }
    , Cmd.batch
        [ fetchPlayers session PlayerTab CompletedPlayerLoad
        , fetchEvents session EventTab CompletedEventLoad
        , fetchMatches session MatchTab CompletedMatchLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW

viewPlayers: Model -> List (Html Msg)
viewPlayers model =
    case model.players of
        Loaded players ->
          Player.viewPlayers model.timeZone players
            |> List.map (Html.map GotPlayerMsg)
        Loading ->
            []

        LoadingSlowly ->
            [ Loading.icon ]

        Failed ->
            [ Loading.error  <| "players. " ++ model.status ]

viewInfo: Model -> List (Html a)
viewInfo model = Info.viewInfo model.timeZone

viewEvents: Model -> List (Html Msg)
viewEvents model =
    case model.events of
        Loaded events ->
            Event.viewEvents model.timeZone events
            |> List.map (Html.map GotEventMsg)
        Loading ->
            []

        LoadingSlowly ->
            [ Loading.icon ]

        Failed ->
            [ Loading.error <| "events. " ++ model.status]

viewMatches: Model -> List (Html Msg)
viewMatches model =
    case model.matches of
        Loaded matches ->
            Match.viewMatches model.timeZone matches
            |> List.map (Html.map GotMatchMsg)
        Loading ->
            []

        LoadingSlowly ->
            [ Loading.icon ]

        Failed ->
            [ Loading.error <| "matches. " ++ model.status]

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "CM Hackers"
    , content =
        div [ class "home-page" ]
            [ -- viewBanner,
              div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-12" ] <|
                        [ div [ class "feed-toggle" ] <|
                            List.concat
                                [ [ viewTabs (Session.cred model.session) model.tab
                                  ],

                                    case model.tab of
                                        InfoTab -> viewInfo model
                                        PlayerTab -> viewPlayers model
                                        EventTab -> viewEvents model
                                        MatchTab -> viewMatches model
                                ]
                        ]
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



infoTab : ( String, Msg )
infoTab =
    ( "Info", ClickedTab InfoTab )

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
        InfoTab ->
            arrangeTabs [] infoTab [ eventTab, playerTab , matchTab ]
        PlayerTab ->
            arrangeTabs [infoTab, eventTab ] playerTab [ matchTab ]
        EventTab->
            arrangeTabs [infoTab] eventTab  [playerTab, matchTab ]
        MatchTab ->
            arrangeTabs [infoTab, eventTab , playerTab] matchTab []

arrangeTabs :
    List ( String, msg )
    -> ( String, msg )
    -> List ( String, msg )
    -> Html msg
arrangeTabs before selected after =
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
    | CompletedMatchLoad (Result Http.Error (List Match.Match))
    | GotEventMsg Event.Msg
    | GotMatchMsg Match.Msg


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
                    ( { model | players = Loaded newPlayers }
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
              ({model | events = Loaded eventModel}, Cmd.none)
        CompletedMatchLoad (Err err) -> ({model | status = httpErrorToString err, matches = Failed}, Cmd.none)
        CompletedMatchLoad (Ok ms) ->
            let matchModel = Match.init model.session ms
            in
              ({model | matches = Loaded matchModel}, Cmd.none)
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
                        ( { model | events = Loaded newEvents }
                        , Cmd.map GotEventMsg subCmd
                        )

                    Loading ->
                        ( model, Log.error "Event msg Loading")

                    LoadingSlowly ->
                        ( model, Log.error  "Event msg LoadingSlowly")

                    Failed ->
                        ( model, Log.error  "Event msg Failed")
            in (m, Cmd.batch [addlCmd, cmd])
        GotMatchMsg subMsg ->
            let (m, cmd)  =
                  case model.matches of
                    Loaded matches ->
                        let
                            ( newMatches, subCmd ) =
                                Match.update (Session.cred model.session) subMsg matches
                        in
                        ( { model | matches = Loaded newMatches }
                        , Cmd.map GotMatchMsg subCmd
                        )

                    Loading ->
                        ( model, Log.error "Match msg Loading")

                    LoadingSlowly ->
                        ( model, Log.error  "Match msg LoadingSlowly")

                    Failed ->
                        ( model, Log.error  "Match msg Failed")
            in (m, cmd)
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

fetchMatches : Session -> HomeTab ->  (Result Http.Error (List Match) -> msg) -> Cmd msg
fetchMatches session feedTabs resToMsg =
    let
        maybeCred =
            Session.cred session

        decoder =
            Match.decoder maybeCred 100


        req =
                    Api.get Endpoint.matches maybeCred decoder
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
