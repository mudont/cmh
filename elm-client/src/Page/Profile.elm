module Page.Profile exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| An Author's profile.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Avatar exposing (Avatar)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (decodeString)
import Loading
import Log
import Page
import PaginatedList exposing (PaginatedList)
import Profile exposing (Profile)
import Result exposing (withDefault)
import Route
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username, dummyUser)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , errors : List String
    , feedTab : FeedTab
    , feedPage : Int

    }


type FeedTab
    = MyArticles
    | FavoritedArticles


type Status a
    = Loading Username
    | LoadingSlowly Username
    | Loaded a
    | Failed Username


init : Session -> Username -> ( Model, Cmd Msg )
init session username =
    let
        maybeCred =
            Session.cred session
    in
    ( { session = session
      , timeZone = Time.utc
      , errors = []
      , feedTab = defaultFeedTab
      , feedPage = 1
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )


currentUsername : Model -> Username
currentUsername model = dummyUser


defaultFeedTab : FeedTab
defaultFeedTab =
    MyArticles



-- HTTP


articlesPerPage : Int
articlesPerPage =
    5



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    let
        title = "Profile title"
    in
    { title = title
    , content = text "Profile content"
    }



-- PAGE TITLE


titleForOther : Username -> String
titleForOther otherUsername =
    "Profile — " ++ Username.toString otherUsername


titleForMe : Maybe Cred -> Username -> String
titleForMe maybeCred username =
    case maybeCred of
        Just cred ->
            if username == Api.username cred then
                myProfileTitle

            else
                defaultTitle

        Nothing ->
            defaultTitle


myProfileTitle : String
myProfileTitle =
    "My Profile"


defaultTitle : String
defaultTitle =
    "Profile"



-- TABS




-- UPDATE


type Msg
    = ClickedDismissErrors
    | ClickedTab FeedTab
    | ClickedFeedPage Int
    | GotTimeZone Time.Zone
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )


        ClickedTab tab ->
            ( { model | feedTab = tab }
            , Cmd.none
            )

        ClickedFeedPage page ->
            ( { model | feedPage = page }
            , Cmd.none
            )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            ( model , Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
