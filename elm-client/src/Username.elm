module Username exposing (Username, dummyUser, decoder, encode, toHtml, toString, urlParser)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser
import Maybe
import Regex
import String






-- TYPES


type Username
    = Username String


dummyUser : Username
dummyUser = Username "dummyUser"
-- CREATE


decoder : Decoder Username
decoder =
    Decode.map Username Decode.string



-- TRANSFORM


encode : Username -> Value
encode (Username username) =
    Encode.string username


toString : Username -> String
toString (Username username) =
    username


urlParser : Url.Parser.Parser (Username -> a) a
urlParser =
    Url.Parser.custom "USERNAME" (\str -> Just (Username str))


toHtml : Username -> Html msg
toHtml (Username username) =
    Html.text username

-- MISC
-- | Keep only the part before the '@' in an email address


shortUsername : String -> String
shortUsername =
    Regex.replace (Maybe.withDefault Regex.never (Regex.fromString "@.*")) (\_ -> "")
