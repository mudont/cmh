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
import Loading
import Log
import Page
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)
import Tennis.Player as Player exposing(..)


-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , status : String
    , tab : HomeTab

    -- Loaded independently from server
    , players : Status Player.Model
    }


type HomeTab
    = EventTab (Maybe Cred)
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
                Just cred ->
                    EventTab (Just cred)
                Nothing ->
                    PlayerTab
      , players = Loading
      }
    , Cmd.batch
        [ fetchPlayers session PlayerTab CompletedPlayerLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "CM Hackers"
    , content =
        div [ class "home-page" ]
            [ -- viewBanner,
              div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
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
                                [ Loading.error "players" ]
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



eventTab : Maybe Cred -> ( String, Msg )
eventTab cred =
    ( "Events", ClickedTab (EventTab cred) )


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
            Player.viewTabs [eventTab maybeCred] playerTab [ matchTab ]

        EventTab tag->
            Player.viewTabs [] (eventTab maybeCred) [playerTab, matchTab ]

        MatchTab ->
            Player.viewTabs [eventTab maybeCred, playerTab] matchTab []

viewTabs1 :
    List ( String, msg )
    -> ( String, msg )
    -> List ( String, msg )
    -> Html msg
viewTabs1 before selected after =
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

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTab tab ->
            ( { model | tab = tab }
            , Cmd.none
            )
        CompletedPlayerLoad (Err err) -> ({model | status = errorToString err}, Cmd.none)
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

        _ ->
            ( model, Cmd.none )



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
