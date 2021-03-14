module Header.Update exposing (..)

import Bootstrap.Navbar as Navbar
import Header.Messages
import Header.Model
import Http



-- You need to handle navbar messages in your update function to step the navbar state forward


update : Header.Messages.Msg -> Header.Model.Model -> ( Header.Model.Model, Cmd Header.Messages.Msg )
update msg model =
    case msg of
        Header.Messages.NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )



-- The navbar needs to know the initial window size, so the inital state for a navbar requires a command to be run by the Elm runtime


init : ( Header.Model.Model, Cmd Header.Messages.Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState Header.Messages.NavbarMsg
    in
    ( { navbarState = navbarState }, navbarCmd )



-- Define a message for the navbar
