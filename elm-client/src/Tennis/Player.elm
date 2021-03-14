module Tennis.Player exposing (
    Model, Msg, decoder, init, update, viewPlayers,
    viewTabs, Player)
import Api exposing (Cred)
import Avatar exposing (Avatar)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
-- import Json.Decode.Field as Field
import Page
import Profile
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Username exposing(..)

-- MODEL

type Model = Model Internals

type alias Player =
    {   username    : Username,
        firstName   : String,
        lastName    : String,
        email       : String,
        mobilePhone : String,
        homePhone   : String,
        workPhone   : String
    }
type alias Internals =
    { session: Session
    , errors: List String
    , players: List Player
    , isLoading: Bool
    }


init : Session -> List Player -> Model
init session players =
    Model
        { session = session
        , errors = []
        , players = players
        , isLoading = False
        }

-- VIEW

viewPlayers : Time.Zone -> Model -> List (Html Msg)
viewPlayers timeZone (Model { players, session, errors }) =
    let
        playersHtml =
            players
                |> List.map (viewPreview)
    in
    Page.viewErrors ClickedDismissErrors errors :: playersHtml


viewPreview : Player -> Html Msg
viewPreview player =
    let
        name = player.firstName ++ " " ++ player.lastName
        email = player.email
        mobile = player.mobilePhone
        username = player.username
    in
    tr [ class "article-preview" ]
        [ td [ class "article-meta" ]
            [ a [ Route.href (Route.Profile username) ]
                [ text <| toString username ]
            ]
        ,  td [] [text name ]
        ,  td [] [text email ]
        ,  td [] [text mobile ]
        ]


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
viewPagination toMsg page (Model player) =
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


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )



-- SERIALIZATION
type Foo = Foo Username String
fooDecoder : Decoder Foo
fooDecoder =
    Decode.succeed Foo
        |> required "username" Username.decoder
        |> required "firstName" Decode.string

playerDecoder : Decoder Player
playerDecoder =
    Decode.succeed Player
        |> required  "username" Username.decoder
        |> required  "firstName" Decode.string
        |> required  "lastName" Decode.string
        |> required  "email" Decode.string
        |> required  "mobilePhone" Decode.string
        |> required  "homePhone" Decode.string
        |> required  "workPhone" Decode.string

decoder : Maybe Cred -> Int -> Decoder (List Player)
decoder maybeCred resultsPerPage =
    Decode.list playerDecoder


