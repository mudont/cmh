module Login.GoogleButton exposing (googleButton)

import Bootstrap.Button as Button
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Login.Messages exposing (..)


googleButton : Html Login.Messages.Msg
googleButton =
    Button.linkButton [ Button.light, Button.attrs [ href "/google", height 38 ] ]
        [ div
            [ class "left" ]
            [ img [ src "https://www.google.com/s2/favicons?sz=32&domain_url=google.com", height 20, width 20, style "margin" "0", alt "G" ] []
            , text "  "
            , text "Google login"
            ]
        ]



-- , alt "G", width 20
-- https://www.google.com/s2/favicons?sz=32&domain_url=google.com
-- https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Google_%22G%22_Logo.svg/512px-Google_%22G%22_Logo.svg.png
--         , [ text "Google" ]
