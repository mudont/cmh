module View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Fun.View
import Header.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Login.View
import Messages exposing (..)
import Model exposing (..)
import Url
import Util as Util


view : Model -> Browser.Document Msg
view model =
    { title = "CM Hackers"
    , body =
        [ div [ class "container" ]
            [ Html.map HeaderMsg (Header.View.view model.header)
            --, div []
                --[ text "The current URL is: "
                --      , b [] [ text (Url.toString model.url) ]
                --      , ul []
                --          [ viewLink "/home"
                --          , viewLink "/profile"
                --          , viewLink "/reviews/the-century-of-the-self"
                --          , viewLink "/reviews/public-opinion"
                --          , viewLink "/reviews/shah-of-shahs"
                --          ]
                --]

            , div [ class "jumbotron text-left" ]
                [ Html.map LoginMsg (Login.View.view model.login)
                ]
            , Html.map FunMsg (Fun.View.view (String.length model.login.token > 0) model.fun)
            ]
        ]
    }
viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]