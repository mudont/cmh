module Util exposing (..)
import Http
import Html exposing(..)
import Html.Attributes exposing (..)
import Iso8601
import Time exposing (Posix)
import Color
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))

httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus 403 ->
            "Please Login or Register"
        Http.BadStatus code ->
            "Unknown error " ++ String.fromInt code
        Http.BadBody errorMessage ->
            errorMessage


rsvpHtml : String -> Html msg
rsvpHtml code =
    case code of
        "A" -> Filled.check_circle 32 (Color <| Color.green)
        "N" -> Filled.not_interested 32 (Color <| Color.red)
        "1" -> Filled.one_x_mobiledata 32 (Color <| Color.lightGreen)
        _ -> text code

iconMore : Html ms
iconMore = Filled.more_horiz 32 (Color <| Color.black)

dateHhMm : Posix -> String
dateHhMm ts = String.map
                -- Replace the 'T' between date and time with a space
                (\c -> if c == 'T' then ' ' else c) <|
                -- ignore the part after minutes. We don't need that msuch precision
                String.slice 0 16 <|
                Iso8601.fromTime ts