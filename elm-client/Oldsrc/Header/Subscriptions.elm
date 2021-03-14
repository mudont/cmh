module Header.Subscriptions exposing (..)

import Bootstrap.Navbar as Navbar
import Header.Messages exposing (..)
import Header.Model


subscriptions : Header.Model.Model -> Sub Header.Messages.Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg
