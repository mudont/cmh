
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
import Tennis.EventRsvps as EventRsvps


-- MODEL

-- TODO use CurrPage or remove it
type CurrPage
    = PageRedirect
    | PageNotFound
    | PageHome
    | PageSettings
    | PageLogin
    | PageRegister
    | PageResetPassword
    | PageProfile
    | PageEventRsvps

type alias Model =
    { page : CurrPage
    , session : Session
    , home : Maybe Home.Model
    , settings : Maybe Settings.Model
    , login : Maybe Login.Model
    , register : Maybe Register.Model
    , resetPassword : Maybe ResetPassword.Model
    , username : Maybe Username
    , profile : Maybe Profile.Model
    , eventRsvps : Maybe EventRsvps.Model
    }

init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    let session = Session.fromViewer navKey maybeViewer
    in changeRouteTo (Route.fromUrl url)
        ({ page = PageRedirect
         , session = session
         , home = Nothing
         , settings = Nothing
         , login = Nothing
         , register = Nothing
         , resetPassword = Nothing
         , username = Nothing
         , profile = Nothing
         , eventRsvps = Nothing
         })



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.viewer model.session

        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view viewer page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model.page of
        PageRedirect ->
            Page.view viewer Page.Other Blank.view

        PageNotFound ->
            Page.view viewer Page.Other NotFound.view

        PageSettings ->
            case model.settings of
                Nothing -> Page.view viewer Page.Other Blank.view
                Just s -> viewPage Page.Other GotSettingsMsg (Settings.view s)

        PageHome ->
            case model.home of
                Nothing -> Page.view viewer Page.Other Blank.view
                Just h -> viewPage Page.Home GotHomeMsg (Home.view h)

        PageLogin ->
            case model.login of
                Nothing -> Page.view viewer Page.Other Blank.view
                Just l -> viewPage Page.Other GotLoginMsg (Login.view l)

        PageRegister ->
            case model.register of
                Nothing -> Page.view viewer Page.Other Blank.view
                Just r -> viewPage Page.Other GotRegisterMsg (Register.view r)

        PageEventRsvps  ->
            case model.eventRsvps of
                Nothing -> Page.view viewer Page.Other Blank.view
                Just er -> viewPage Page.Other GotEventRsvpsMsg (EventRsvps.view er)

        PageResetPassword  ->
            case model.resetPassword of
                Nothing -> Page.view viewer Page.Other Blank.view
                Just rp -> viewPage Page.Other GotResetPasswordMsg (ResetPassword.view rp)

        PageProfile ->
            case (model.profile, model.username) of
                (Just p, Just u) -> viewPage (Page.Profile u) GotProfileMsg (Profile.view p)
                _ -> Page.view viewer Page.Other Blank.view



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
    | GotEventRsvpsMsg EventRsvps.Msg


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session = model.session
    in
    case maybeRoute of
        Nothing ->
            ( updateSubModel PageNotFound model, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.Settings ->
            let (subM, cmd) = Settings.init session
            in ({model|settings = Just subM }, cmd)
                |> updateWith GotSettingsMsg PageSettings

        Just Route.Home ->
            let (subM, cmd) = Home.init session model.home
            in ({model|home = Just subM }, cmd) |> updateWith GotHomeMsg PageHome

        Just Route.Login ->
            let (subM, cmd) = Login.init session
            in ({model|login = Just subM }, cmd)
              |> updateWith GotLoginMsg PageLogin

        Just Route.Register ->
            let (subM, cmd) = Register.init session
            in ({model|register = Just subM }, cmd)
                |> updateWith GotRegisterMsg PageRegister

        Just Route.ResetPassword ->
            let (subM, cmd) = ResetPassword.init session
            in ({model|resetPassword = Just subM }, cmd)
              |> updateWith GotResetPasswordMsg PageResetPassword

        Just (Route.Profile username) ->
            let (subM, cmd) = Profile.init session username
            in ({model|profile = Just subM }, cmd)
              |> updateWith GotProfileMsg PageProfile

        Just (Route.EventRsvps eventId) ->
            let (subM, cmd) = EventRsvps.init session eventId
            in ({model|eventRsvps = Just subM }, cmd)
               |> updateWith GotEventRsvpsMsg PageEventRsvps

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ClickedLink urlRequest, _) ->
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
                                Nav.pushUrl (Session.navKey model.session) (Url.toString url)
                              , Log.dbg <| "Internal Link clicked: " ++ Url.toString url
                              ]
                            )

                Browser.External href ->
                    ( model
                    , Cmd.batch [ Nav.load href
                                , Log.dbg <| "Ext Link clicked: " ++ href
                                ]
                    )

        ( ChangedUrl url, _) ->
            let (m, cmd) = changeRouteTo (Route.fromUrl url) model
            in (m, Cmd.batch [cmd, Log.dbg <| "Url changed to " ++ Url.toString url])

        ( GotSettingsMsg subMsg, PageSettings) ->
            case model.settings of
                Nothing -> (model, Log.warn "Expected to have Settings but none")
                Just s ->
                    let (subM, cmd) = Settings.update subMsg s
                        m = {model | settings = Just subM }
                        (m2, cmd2) = updateWith GotSettingsMsg PageSettings (m,cmd)
                    in (m2, Cmd.batch [Log.dbg "DBG-- Settings msg", cmd2])

        ( GotLoginMsg subMsg, PageLogin) ->
            case model.login of
                Nothing -> (model, Log.warn "Expected to have Login but none")
                Just l ->
                    let (subM, cmd) = Login.update subMsg l
                        m = {model | login = Just subM }
                    in updateWith GotLoginMsg PageLogin (m,cmd)

        ( GotRegisterMsg subMsg, PageRegister) ->
            case model.register of
                Nothing -> (model, Log.warn "Expected to have Register but none")
                Just register ->
                    let (subM, cmd) = Register.update subMsg register
                        m = {model | register = Just subM }
                    in updateWith GotRegisterMsg PageRegister (m, cmd)

        ( GotResetPasswordMsg subMsg, PageResetPassword ) ->
            case model.resetPassword of
                Nothing -> (model, Log.warn "Expected to have ResetPasswd but none")
                Just rp ->
                    let (subM, cmd) = ResetPassword.update subMsg rp
                        m = {model | resetPassword = Just subM }
                    in updateWith GotResetPasswordMsg PageResetPassword (m,cmd)

        ( GotEventRsvpsMsg subMsg, PageEventRsvps) ->
            case model.eventRsvps of
                Nothing -> (model, Log.warn "Expected to have EventRsvps but none")
                Just er ->
                    let (subM, cmd) = EventRsvps.update subMsg er
                        m = {model | eventRsvps = Just subM }
                    in updateWith GotEventRsvpsMsg PageEventRsvps (m,cmd)

        ( GotHomeMsg subMsg, PageHome) ->
            case model.home of
                Nothing -> (model, Log.warn "Expected to have Home but none")
                Just h ->
                    let (subM, cmd) = Home.update subMsg h
                        m = {model | home = Just subM }
                    in updateWith GotHomeMsg PageHome (m, cmd)

        ( GotProfileMsg subMsg, PageProfile) ->
            case (model.profile, model.username) of
                (Just p, Just u) ->
                    let (subM, cmd) = Profile.update subMsg p
                        m = {model | profile = Just subM }
                    in updateWith GotProfileMsg PageProfile (m, cmd)
                _ -> (model, Log.warn "Expected to have Profile+Usrname but none")


        ( GotSession session, _ ) ->
            ( {model| session = session}
            , Cmd.batch
              [ Route.replaceUrl (Session.navKey session) Route.Home
              , Log.dbg "GotSession msg"
              ]
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.batch [Log.error "Err-- Unknown msg received by update() in Main.elm",Cmd.none] )


updateSubModel : CurrPage -> Model -> Model
updateSubModel page model =
    { model | page = page    }

updateWith : (subMsg -> Msg) ->
             CurrPage ->
             ( Model, Cmd subMsg ) ->
             ( Model, Cmd Msg )
updateWith toMsg page (model, subCmd ) =
    ( { model | page = page
      }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Session.changes GotSession (Session.navKey model.session),
    case model.page of
        PageNotFound ->
            Sub.none

        PageRedirect ->
            Session.changes GotSession (Session.navKey model.session)

        PageSettings ->
            case model.settings of
               Nothing -> Sub.none
               Just s -> Sub.map GotSettingsMsg (Settings.subscriptions s)

        PageHome ->
            case model.home of
               Nothing -> Sub.none
               Just h -> Sub.map GotHomeMsg (Home.subscriptions h)

        PageLogin ->
            case model.login of
               Nothing -> Sub.none
               Just l -> Sub.map GotLoginMsg (Login.subscriptions l)

        PageRegister ->
            case model.register of
               Nothing -> Sub.none
               Just r -> Sub.map GotRegisterMsg (Register.subscriptions r)


        PageResetPassword ->
            case model.resetPassword of
               Nothing -> Sub.none
               Just rp -> Sub.map GotResetPasswordMsg (ResetPassword.subscriptions rp)

        PageProfile ->
            case model.profile of
               Nothing -> Sub.none
               Just p -> Sub.map GotProfileMsg (Profile.subscriptions p)

        PageEventRsvps  ->
            case model.eventRsvps of
               Nothing -> Sub.none
               Just er -> Sub.map GotEventRsvpsMsg (EventRsvps.subscriptions er)
  ]

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

