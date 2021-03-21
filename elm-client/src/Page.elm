module Page exposing (Page(..), view, viewErrors)

import Api exposing (Cred)
import Asset exposing (src)
import Avatar
import Browser exposing (Document)
import Html exposing (Html, a, button, div, footer, h5, i, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, style, type_)
import Html.Events exposing (onClick)
import Profile
import Route exposing (Route)
import Session exposing (Session)
import Username exposing (Username)
import Viewer exposing (Viewer)
--import Bootstrap.Grid as Grid
--import Bootstrap.Navbar as Navbar

{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type Page
    = Other
    | Home
    | Login
    | Register
    | ResetPassword
    | Settings
    | Profile Username


{-| Take a page's Html and frames it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
view : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
view maybeViewer page { title, content } =
    { title = title ++ " - CM Hackers"
    , body = viewHeader page maybeViewer :: content :: [ viewFooter ]
    }

{-
//https://bootstrap-menu.com/detail-offcanvas-collapse.html
-}
viewHeader : Page -> Maybe Viewer -> Html msg
viewHeader page maybeViewer =
    -- nav [ class "navbar  navbar-expand-md navbar-light" ]
    nav [ class "navbar navbar-expand-lg " ]
        -- [ div [ class "container" ]
        [ a [ class "navbar-brand", Route.href Route.Home ]
            [ img
                  [ src Asset.hackersLogo
                  , class "d-inline-block align-top"

                  -- , style "width" "30px"
                  , style "margin-right" "10"
                  ]
                  []
            , text "CM Hackers"
            ]
        , button [class "navbar-toggler  navbar-light", type_ "button", attribute "data-trigger" "#main_nav"]
                [ span [class "navbar-toggler-icon"] []
                ]
        , div [class "navbar-collapse", id "main_nav"]
            [ div [class "offcanvas-header mt-3"]
                [ button [class "btn btn-outline-danger btn-close float-right"] [text "\u{00D7} Close"]
                , h5 [class "py-2 text-white"] [text "Main navbar"]
                ]
            -- , ul [ class "nav navbar-nav pull-sm-right" ] <|
            , ul [ class "navbar-nav" ] <|
                [ navbarLink page Route.Home [ text "Home" ]
                , li [ classList [ ( "nav-item", True ), ( "active", False ) ] ]
                          [ a [ class "nav-link", href "https://py.cmhackers.com" ] [text "Old Site"] ]

                ]
            , ul [ class "navbar-nav ml-auto" ] <| viewMenu page maybeViewer
           ]
        ]
        --]
--
--viewHeaderBs : Page -> Maybe Viewer -> Html msg
--viewHeaderBs page maybeViewer =
--  Grid.container [] -- Wrap in a container to center the navbar
--    [ Navbar.config NavbarMsg
--        |> Navbar.withAnimation
--        |> Navbar.collapseMedium            -- Collapse menu at the medium breakpoint
--        |> Navbar.info                      -- Customize coloring
--        |> Navbar.brand                     -- Add logo to your brand with a little styling to align nicely
--            [ href "#" ]
--            [ img
--                [ src "assets/images/elm-bootstrap.svg"
--                , class "d-inline-block align-top"
--                , style [ ( "width", "30px" ) ]
--                ]
--                []
--            , text " Elm Bootstrap"
--            ]
--        |> Navbar.items
--            [ Navbar.itemLink
--                [ href "#" ] [ text "Item 1" ]
--            , Navbar.dropdown              -- Adding dropdowns is pretty simple
--                { id = "mydropdown"
--                , toggle = Navbar.dropdownToggle [] [ text "My dropdown" ]
--                , items =
--                    [ Navbar.dropdownHeader [ text "Heading" ]
--                    , Navbar.dropdownItem
--                        [ href "#" ]
--                        [ text "Drop item 1" ]
--                    , Navbar.dropdownItem
--                        [ href "#" ]
--                        [ text "Drop item 2" ]
--                    , Navbar.dropdownDivider
--                    , Navbar.dropdownItem
--                        [ href "#" ]
--                        [ text "Drop item 3" ]
--                    ]
--                }
--            ]
--        |> Navbar.customItems
--            [ Navbar.formItem []
--                [ Input.text [ Input.attrs [placeholder "enter" ]]
--                , Button.button
--                    [ Button.success
--                    , Button.attrs [ Spacing.ml2Sm]
--                    ]
--                    [ text "Search"]
--                ]
--            , Navbar.textItem [ Spacing.ml2Sm, class "muted" ] [ text "Text"]
--            ]
--        |> Navbar.view state.customState
--    ]

viewMenu : Page -> Maybe Viewer -> List (Html msg)
viewMenu page maybeViewer =
    let
        linkTo =
            navbarLink page
    in
    case maybeViewer of
        Just viewer ->
            let
                username =
                    Viewer.username viewer

                avatar =
                    Viewer.avatar viewer
            in
            [ linkTo
                (Route.Settings)
                [ img [ class "user-pic", Avatar.src avatar ] []
                , Username.toHtml username
                ]
            , linkTo Route.Logout [ text "Sign out" ]
            ]

        Nothing ->
            [ linkTo Route.Login [ text "Sign in" ]
            , linkTo Route.ResetPassword [ text "Reset Password" ]
            , linkTo Route.Register [ text "Sign up" ]
            ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "CM Hackers" ]
            , span [ class "attribution" ]
                [ text "Web project "
                , a [ href "https://github.com/mudont/cmh" ] [ text "CM" ]
                , text ". Code & design licensed under BSD 3."
                ]
            ]
        ]


navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Login, Route.Login ) ->
            True

        ( Register, Route.Register ) ->
            True

        ( ResetPassword, Route.ResetPassword ) ->
            True

        ( Settings, Route.Settings ) ->
            True

        ( Profile pageUsername, Route.Profile routeUsername ) ->
            pageUsername == routeUsername

        _ ->
            False


{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]
