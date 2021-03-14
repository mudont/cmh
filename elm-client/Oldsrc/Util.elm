module Util exposing (..)

import Maybe
import Regex
import String



-- | Keep only the part before the '@' in an email addres


shortUsername : String -> String
shortUsername =
    Regex.replace (Maybe.withDefault Regex.never (Regex.fromString "@.*")) (\_ -> "")
