module Page.ResetPassword exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)
import Email exposing (..)


-- MODEL


type alias Model =
    { session : Session
    , results : List ResetResult
    , form : Form
    }


type alias Form =
    { email : String
    }


type ResetResult
    = InvalidEntry ValidatedField String
    | ServerError String
    | OK String


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , results = []
      , form =
            { email = ""
            }
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Reset Password"
    , content =
        div [ class "cred-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Sign up" ]
                        , p [ class "text-xs-center" ]
                            [ a [ Route.href Route.Login ]
                                [ text "Have an account?" ]
                            ]
                        , ul [ class "error-messages" ]
                            (List.map viewProblem model.results)
                        , viewForm model.form
                        ]
                    ]
                ]
            ]
    }


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , placeholder "Email"
                , onInput EnteredEmail
                , value form.email
                ]
                []
            ]
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Reset Password" ]
        ]


viewProblem : ResetResult -> Html msg
viewProblem result =
    let
        errorMessage =
            case result of
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
                OK str ->
                    str
    in
    li [] [ text errorMessage ]



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | CompletedReset (Result Http.Error String)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | results = [] }
                    ,  resetPassword validForm
                    )

                Err problems ->
                    ( { model | results = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        CompletedReset (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
            ( { model | results = List.append model.results serverErrors }
            , Cmd.none
            )

        CompletedReset (Ok str) ->
            ( { model | results = List.append model.results [OK str] }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



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
-}
type ValidatedField
    = Email


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List ResetResult) TrimmedForm
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


validateField : TrimmedForm -> ValidatedField -> List ResetResult
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        }



-- HTTP


resetPassword : TrimmedForm -> Cmd Msg -- Viewer
resetPassword (Trimmed form) =
    let
        user =
            Encode.object
                [ ( "email", Encode.string form.email )
                ]

        body =
            Http.jsonBody user
    in
    (Api.resetPassword form.email) string CompletedReset
