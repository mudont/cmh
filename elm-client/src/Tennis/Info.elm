module Tennis.Info exposing (Model, viewInfo)
import Api exposing (Cred)
--import Css exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Text as Text
import Element exposing (link)
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, value)
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

import Bootstrap.Table as Table

-- MODEL

type Model = Model Internals

type alias Internals =
    { dummy : Int
    }


init : Session -> Model
init session =
    Model
        { dummy = 0
        }

-- VIEW

viewInfo : Time.Zone -> List (Html a)
viewInfo _ =
    [ div []
          [ Card.config [ Card.align Text.alignXsCenter ]
              |> Card.block [] teamsBlockContents
              |> Card.view
          , Card.config [ Card.align Text.alignXsCenter ]
              |> Card.block [] contactBlockContents
              |> Card.view
          --
          --, Card.config [ Card.align Text.alignXsCenter, Card.attrs [class "mt-4"] ]
          --    |> Card.headerH3 [] [ text "C H" ]
          --    |> Card.block [] []
          --    |> Card.footer [] [ text "C F" ]
          --    |> Card.view
          ]
    ]

teamsBlockContents : List (Block.Item msg)
teamsBlockContents =
    [ Block.titleH3 [] [ text "USTA Tennis Teams" ]
    , Block.text [] [ text "We are a Tennis club based in Central New Jersey." ]
    , Block.custom <|
        Table.table
        { options = [ Table.striped, Table.hover, Table.small ]
        , thead =  Table.simpleThead
            [ Table.th [] [ text "Team" ]
            , Table.th [] [ text "Captain" ]
            ]
        , tbody =
            Table.tbody []
                [ Table.tr []
                    [ Table.td [] [a [href "https://tennislink.usta.com/Leagues/Main/StatsAndStandings.aspx?t=R-3&par1=3512613436&par2=2021&par3=0&OrgURL=../../Leagues/Common/Home.aspx"]
                                     [text "USTA 4.0 18+"]]
                    , Table.td [] [text "Ashok Selvaraj/Karthik Badri"]
                    ]
                , Table.tr []
                    [ Table.td [] [a [href "https://tennislink.usta.com/Leagues/Main/StatsAndStandings.aspx?t=R-3&par1=3512613421&par2=2021&par3=0&OrgURL=../../Leagues/Common/Home.aspx"]
                                     [text "USTA 4.0 40+"]]
                    , Table.td [] [text "Ram Donthireddy/Sai Pappu"]
                    ]
                , Table.tr []
                    [ Table.td [] [a [href "https://tennislink.usta.com/Leagues/Main/StatsAndStandings.aspx?t=R-3&par1=3512613422&par2=2021&par3=0&OrgURL=../../Leagues/Common/Home.aspx"]
                                     [text "USTA 4.0 40+ Spartans"]]
                    , Table.td [] [text "Gopalakrishnan Thirukallam/Ankush Kumar"]
                    ]
                , Table.tr []
                    [ Table.td [] [a [href "https://tennislink.usta.com/Leagues/Main/StatsAndStandings.aspx?t=R-3&par1=3512613628&par2=2021&par3=0&OrgURL=../../Leagues/Common/Home.aspx"] [text "USTA 3.5 40+"]]
                    , Table.td [] [text "Vaibhav Shinde/Ravi Bhatheja"]
                    ]
                ]
        }
    ]

contactBlockContents : List (Block.Item msg)
contactBlockContents =
    [ Block.titleH3 [] [ text "Contact" ]
    , Block.link [href "mailto:bbotsch3@gmail.com,sak0007@yahoo.com"] [text "Click here"]
    , Block.text [] [ text "To email our captain" ]
            -- [ListGroup.ul [ListGroup.li [] [text "click here"]]]
    ]

sampleBlockContents : List (Block.Item msg)
sampleBlockContents =
    [ Block.titleH3 [] [ text "Contact" ]
    , Block.text [] [ text "" ]
    , Block.custom <|
        Button.button [ Button.primary ] [ text "Go somewhere" ]
    ]
-- UPDATE


type Msg
    = ClickedDismissErrors


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model model, Cmd.none )
