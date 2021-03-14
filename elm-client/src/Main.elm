
module Main exposing (main)
--import Login.Model
--import Messages exposing (..)
--import Model exposing (Model)
--import Subscriptions exposing (..)
--import Update exposing (..)
--import View exposing (..)

import Api exposing (Cred)
import Avatar exposing (Avatar)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Blank as Blank
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Register as Register
import Page.ResetPassword as ResetPassword
import Page.Settings as Settings
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time
import Url exposing (Url)
import Username exposing (Username)
import Viewer exposing (Viewer)
import Log exposing (..)

type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Settings Settings.Model
    | Login Login.Model
    | Register Register.Model
    | ResetPassword ResetPassword.Model
    | Profile Username Profile.Model



-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer))



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.viewer (toSession model)

        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view viewer page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.view viewer Page.Other Blank.view

        NotFound _ ->
            Page.view viewer Page.Other NotFound.view

        Settings settings ->
            viewPage Page.Other GotSettingsMsg (Settings.view settings)

        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        Login login ->
            viewPage Page.Other GotLoginMsg (Login.view login)

        Register register ->
            viewPage Page.Other GotRegisterMsg (Register.view register)

        ResetPassword reset ->
            viewPage Page.Other GotResetPasswordMsg (ResetPassword.view reset)

        Profile username profile ->
            viewPage (Page.Profile username) GotProfileMsg (Profile.view profile)



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg
    | GotResetPasswordMsg ResetPassword.Msg
    | GotProfileMsg Profile.Msg
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Settings settings ->
            Settings.toSession settings

        Login login ->
            Login.toSession login

        Register register ->
            Register.toSession register

        ResetPassword reset ->
            ResetPassword.toSession reset

        Profile _ profile ->
            Profile.toSession profile

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.Settings ->
            Settings.init session
                |> updateWith Settings GotSettingsMsg model

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg model

        Just Route.ResetPassword ->
            ResetPassword.init session
                |> updateWith ResetPassword GotResetPasswordMsg model

        Just (Route.Profile username) ->
            Profile.init session username
                |> updateWith (Profile username) GotProfileMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.batch [
                                    Log.dbg "Internal link clicked. No fragment"
                                  , Nav.load (Url.toString url)
                                  ])

                        Just _ ->
                            ( model
                            , Cmd.batch [
                                Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                              , Log.dbg <| "Internal Link clicked: " ++ Url.toString url
                              ]
                            )

                Browser.External href ->
                    ( model
                    , Cmd.batch [ --Nav.load href
                                 Log.dbg <| "Ext Link clicked: " ++ href
                                ]
                    )

        ( ChangedUrl url, _ ) ->
            let (m, cmd) = changeRouteTo (Route.fromUrl url) model
            in (m, Cmd.batch [cmd, Log.dbg <| "Url changed to " ++ Url.toString url])
        ( GotSettingsMsg subMsg, Settings settings ) ->
            Settings.update subMsg settings
                |> updateWith Settings GotSettingsMsg model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg model

        ( GotResetPasswordMsg subMsg, ResetPassword reset ) ->
            ResetPassword.update subMsg reset
                |> updateWith ResetPassword GotResetPasswordMsg model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotProfileMsg subMsg, Profile username profile ) ->
            Profile.update subMsg profile
                |> updateWith (Profile username) GotProfileMsg model


        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Settings settings ->
            Sub.map GotSettingsMsg (Settings.subscriptions settings)

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Register register ->
            Sub.map GotRegisterMsg (Register.subscriptions register)


        ResetPassword reset ->
            Sub.map GotResetPasswordMsg (ResetPassword.subscriptions reset)

        Profile _ profile ->
            Sub.map GotProfileMsg (Profile.subscriptions profile)


-- MAIN


main : Program Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

--main : Program Login.Model.Model Model Msg
--main =
--    Browser.document
--        { init = init
--        , update = upd
--        , subscriptions = subscriptions
--        , view = view
--        --, onUrlRequest = LinkClicked
--        --, onUrlChange = UrlChanged
--        }
