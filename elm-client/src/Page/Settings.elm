module Page.Settings exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Avatar
import Browser.Navigation as Nav
import Email exposing (Email)
import Html exposing (Html, button, div, fieldset, h1, input, li, text, textarea, ul)
import Html.Attributes exposing (attribute, class, placeholder, type_, value, autocomplete)
import Html.Events exposing (onInput, onSubmit)
-- import Html.Styled.Attributes exposing (autocomplete)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import Loading
import Log
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task
import Username as Username exposing (Username, dummyUser)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , problems : List Problem
    , status : Status
    }


type alias Form =
    { email : String
    , firstName : String
    , lastName : String
    , username : String
    , mobilePhone : String
    , homePhone : String
    , workPhone : String
    , password : String
    , password2 : String
    }


type Status
    = Loading
    | LoadingSlowly
    | Loaded Form
    | Failed


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String
    | OK String


init : Session -> ( Model, Cmd Msg )
init session =
    let mcr = Session.cred session
        user = case mcr of
            Nothing -> dummyUser
            Just cr -> Api.username cr
    in ( { session = session
      , problems = []
      , status = Loading
      }
    , Cmd.batch

        [ Api.get (Endpoint.profile user) (Session.cred session) formDecoder CompletedFormLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
        --[ Api.get Endpoint.user (Session.cred session) (Decode.field "user" formDecoder)
        --    |> Http.send CompletedFormLoad
        --, Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        --]
    )


formDecoder : Decoder Form
formDecoder =
    Decode.succeed Form
        |> required "email" Decode.string
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "username" Decode.string
        |> required "mobilePhone" Decode.string
        |> required "homePhone" Decode.string
        |> required "workPhone" Decode.string
        |> hardcoded "" -- Password
        |> hardcoded "" -- Password2


{-| A form that has been validated. Only the `edit` function uses this. Its
purpose is to prevent us from forgetting to validate the form before passing
it to `edit`.

This doesn't create any guarantees that the form was actually validated. If
we wanted to do that, we'd need to move the form data into a separate module!

-}
type ValidForm
    = Valid Form



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Profile"
    , content =
        case Session.cred model.session of
            Just cred ->
                div [ class "settings-page" ]
                    [ div [ class "container page" ]
                        [ div [ class "row" ]
                            [ div [ class "col-md-6 offset-md-3 col-xs-12" ] <|
                                [ h1 [ class "text-xs-center" ] [ text "Your Profile" ]
                                , ul [  ]
                                    (List.map viewProblem model.problems)
                                , case model.status of
                                    Loaded form ->
                                        viewForm cred form

                                    Loading ->
                                        text ""

                                    LoadingSlowly ->
                                        Loading.icon

                                    Failed ->
                                        text "Error loading page."
                                ]
                            ]
                        ]
                    ]

            Nothing ->
                text "Sign in to view your profile."
    }


viewForm : Cred -> Form -> Html Msg
viewForm cred form =
    Html.form [ onSubmit (SubmittedForm cred form) ]
        [ fieldset []
            [ fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "Email"
                    , value form.email
                    , onInput EnteredEmail
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "First Name"
                    , value form.firstName
                    , onInput EnteredFirstName
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "Last Name"
                    , value form.lastName
                    , onInput EnteredLastName
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "Username"
                    , value form.username
                    , onInput EnteredUsername
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control "
                    , placeholder "Mobile Phone"
                    , value form.mobilePhone
                    , onInput EnteredMobilePhone
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "Home Phone"
                    , value form.homePhone
                    , onInput EnteredHomePhone
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "Work Phone"
                    , value form.workPhone
                    , onInput EnteredWorkPhone
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , type_ "password"
                    , placeholder "Password"
                    , value form.password
                    , onInput EnteredPassword
                    , autocomplete False
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , type_ "password"
                    , placeholder "Password again"
                    , value form.password2
                    , onInput EnteredPassword2
                    , autocomplete False
                    ]
                    []
                ]
            , button
                [ class "btn btn-lg btn-primary pull-xs-right" ]
                [ text "Update Profile" ]
            ]
        ]


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        (cls, errorMessage) =
            case problem of
                InvalidEntry _ message ->
                    ("error-messages", message)

                ServerError message ->
                    ("error-messages", message)
                OK message -> ("", message)
    in
    li [class cls] [ text errorMessage ]



-- UPDATE


type Msg
    = SubmittedForm Cred Form
    | EnteredEmail String
    | EnteredUsername String
    | EnteredPassword String
    | EnteredPassword2 String
    | EnteredFirstName String
    | EnteredLastName String
    | EnteredMobilePhone String
    | EnteredHomePhone String
    | EnteredWorkPhone String
    | CompletedFormLoad (Result Http.Error Form)
    | CompletedSave (Result Http.Error String)
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedFormLoad (Ok form) ->
            ( { model | status = Loaded form }
            , Cmd.none
            )

        CompletedFormLoad (Err _) ->
            ( { model | status = Failed }
            , Cmd.none
            )

        SubmittedForm cred form ->
            case validate form of
                Ok validForm ->
                    ( { model | status = Loaded form }
                    , edit cred validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        EnteredPassword2 password2 ->
            updateForm (\form -> { form | password2 = password2 }) model

        EnteredFirstName firstName ->
            updateForm (\form -> { form | firstName = firstName }) model

        EnteredLastName lastName ->
            updateForm (\form -> { form | lastName = lastName }) model

        EnteredMobilePhone mobilePhone ->
            updateForm (\form -> { form | mobilePhone = mobilePhone }) model

        EnteredHomePhone homePhone ->
            updateForm (\form -> { form | homePhone = homePhone }) model

        EnteredWorkPhone workPhone ->
            updateForm (\form -> { form | workPhone = workPhone }) model

        CompletedSave (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
            ( { model | problems = List.append model.problems serverErrors }
            , Cmd.none
            )

        CompletedSave (str) ->
            ( {model | problems = [OK "Profile Saved"]}
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            case model.status of
                Loading ->
                    ( { model | status = LoadingSlowly }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd msg )
updateForm transform model =
    case model.status of
        Loaded form ->
            ( { model | status = Loaded (transform form) }, Cmd.none )

        _ ->
            ( model, Log.error "Bad Profile model Loaded status")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!

NOTE: there are no ImageUrl or Bio variants here, because they aren't validated!

-}
type ValidatedField
    = Username
    | Email
    | Password
    | Password2


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Username
    , Email
    , Password
    , Password2
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Username ->
                if String.isEmpty form.username then
                    [ "username can't be blank." ]

                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                let
                    passwordLength =
                        String.length form.password
                in
                if passwordLength > 0 && passwordLength < Viewer.minPasswordChars then
                    [ "password must be at least " ++ String.fromInt Viewer.minPasswordChars ++ " characters long." ]

                else
                    []

            Password2 ->
                if form.password /= form.password2 then
                    [ "Both passwords must be same" ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , username = String.trim form.username
        , password = String.trim form.password
        , password2 = String.trim form.password2
        , firstName = String.trim form.firstName
        , lastName = String.trim form.lastName
        , mobilePhone = String.trim form.mobilePhone
        , homePhone = String.trim form.homePhone
        , workPhone = String.trim form.workPhone
        }



-- HTTP


{-| This takes a Valid Form as a reminder that it needs to have been validated
first.
-}
edit : Cred -> TrimmedForm -> Cmd Msg -- Http.Request Viewer
edit cred (Trimmed form) =
    let
        updates =
            [ ( "username", Encode.string form.username )
            , ( "email", Encode.string form.email )
            , ( "firstName", Encode.string form.firstName )
            , ( "lastName", Encode.string form.lastName )
            , ( "mobilePhone", Encode.string form.mobilePhone )
            , ( "homePhone", Encode.string form.homePhone )
            , ( "workPhone", Encode.string form.workPhone )
            ]

        encodedUser =
            Encode.object <|
                case form.password of
                    "" ->
                        updates

                    password ->
                        ( "password", Encode.string password ) :: updates

        body = encodedUser |> Http.jsonBody
    in
    Api.profile cred body (succeed "") CompletedSave


nothingIfEmpty : String -> Maybe String
nothingIfEmpty str =
    if String.isEmpty str then
        Nothing

    else
        Just str
