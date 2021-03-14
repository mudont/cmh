-- You need to keep track of the view state for the navbar in your model


module Header.Model exposing (..)

import Bootstrap.Navbar as Navbar


type alias Model =
    { navbarState : Navbar.State }
