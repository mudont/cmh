module Header.View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Header.Messages exposing (..)
import Header.Model
import Html exposing (..)
import Html.Attributes exposing (..)
import Util


view : Header.Model.Model -> Html Header.Messages.Msg
view model =
    Grid.container []
        -- Wrap in a container to center the navbar
        [ Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.collapseMedium
            -- Collapse menu at the medium breakpoint
            |> Navbar.info
            -- Customize coloring
            |> Navbar.brand
                -- Add logo to your brand with a little styling to align nicely
                [ href "#" ]
                [ img
                    [ src "hackers-32x32.png"
                    , class "d-inline-block align-top"

                    -- , style "width" "30px"
                    -- , style "margin-top" "0"
                    ]
                    []
                , text " CM Hackers"
                ]
            |> Navbar.items
                [ Navbar.itemLink
                    [ href "#" ]
                    [ text "Item 1" ]
                , Navbar.dropdown
                    -- Adding dropdowns is pretty simple
                    { id = "mydropdown"
                    , toggle = Navbar.dropdownToggle [] [ text "My dropdown" ]
                    , items =
                        [ Navbar.dropdownHeader [ text "CM Hackers" ]
                        , Navbar.dropdownItem
                            [ href "#" ]
                            [ text "Drop item 1" ]
                        , Navbar.dropdownItem
                            [ href "#" ]
                            [ text "Drop item 2" ]
                        , Navbar.dropdownDivider
                        , Navbar.dropdownItem
                            [ href "#" ]
                            [ text "Drop item 3" ]
                        ]
                    }
                ]
            |> Navbar.customItems
                [ Navbar.formItem []
                    [ Input.text [ Input.attrs [ placeholder "enter" ] ]
                    , Button.button
                        [ Button.success
                        , Button.attrs [ Spacing.ml2Sm ]
                        ]
                        [ text "Search" ]
                    ]
                , Navbar.textItem [ Spacing.ml2Sm, class "muted" ]
                    [ text "userXyz"

                    {- Util.shortUsername  model.username -}
                    ]
                ]
            |> Navbar.view model.navbarState
        ]
