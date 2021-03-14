module Fun.View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Fun.Messages exposing (..)
import Fun.Model exposing (..)
import Header.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Util as Util


view : Bool -> Model -> Html Msg
view loggedIn model =
    let
        -- If user is logged in, show button and quote; if logged out, show a message instructing them to log in
        protectedQuoteView =
            if loggedIn then
                div []
                    [ p [ class "text-center" ]
                        [ button [ class "btn btn-info", onClick GetProtectedQuote ] [ text "Chuck Norris" ]
                        ]
                    ]

            else
                p [ class "text-center" ] [ text "Log in for more" ]
    in
    div []
        [ Grid.row []
            [ Grid.col [] [ text "Random Quote" ]
            , Grid.col []
                [ p [ class "text-center" ]
                    [ button [ class "btn btn-success", onClick GetQuote ] [ text "Rajnikanth" ]
                    ]
                ]
            , Grid.col []
                [ div []
                    [ protectedQuoteView
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col
                []
                [ -- Blockquote with quote
                  blockquote []
                    [ p [] [ text model.quote ]
                    ]
                ]
            ]
        ]
