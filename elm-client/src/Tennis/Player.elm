module Tennis.Player exposing (
    Model, Msg, decoder, init, update, viewPlayers, Player)
import Api exposing (Cred)
--import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
--import Html.Styled exposing (..)
--import Html.Styled.Attributes exposing (..)
--import Html.Styled.Events exposing (..)
import Html.Events exposing (onInput)
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Page
import Route exposing (Route)
import Session exposing (Session)
import Time
import Username exposing(..)
import Bootstrap.Table as Table
import Log
import Fuzzy
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
    , search: String
    , players: List Player
    , isLoading: Bool
    }


init : Session -> List Player -> Model
init session players =
    Model
        { session = session
        , errors = []
        , search = ""
        , players = players
        , isLoading = False
        }

-- VIEW

viewPlayers : Time.Zone -> Model -> List (Html Msg)
viewPlayers timeZone (Model { players, session, search, errors }) =
    let
        playerText = \p -> p.firstName  ++ p.lastName ++ p.email ++ p.mobilePhone ++ (toString p.username)
        fuzzyScore = \p -> (Fuzzy.match [Fuzzy.addPenalty 0] [] search (playerText p)).score
        filteredPlayers =
          if search == ""
          then players
          else List.sortBy fuzzyScore players

        playersHtml =
            [ input [ placeholder "Fuzzy Search"
                    , onInput SearchChanged
                    , value search
                    ] []
            , Table.table
                { options = [ Table.striped, Table.hover, Table.small ]
                , thead =  Table.simpleThead
                    [ Table.th [] [ text "Name" ]
                    , Table.th [] [ text "Mobile" ]
                    , Table.th [] [ text "Email" ]
                    ]
                , tbody =
                    Table.tbody [] <| List.map (viewPreview) <| filteredPlayers
                }
            ]
    in
    Page.viewErrors ClickedDismissErrors errors :: playersHtml


viewPreview : Player -> Table.Row Msg
viewPreview player =
    let
        name = player.firstName ++ " " ++ player.lastName
        email = player.email
        mobile = player.mobilePhone
        username = player.username
    in
      Table.tr [  ]
        [ Table.td []
            [ a [ Route.href (Route.Profile username) ]
                [ text name ]
            ]
        ,  Table.td [] [text mobile ]
        ,  Table.td [] [text email ]
        ]

-- UPDATE


type Msg
    = ClickedDismissErrors
    | SearchChanged String


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )
        SearchChanged srch -> (Model {model | search = srch}, Log.dbg <| "SeachChanged " ++ srch)



-- SERIALIZATION

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


