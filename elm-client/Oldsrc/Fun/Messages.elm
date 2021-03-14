module Fun.Messages exposing (..)

import Http


type Msg
    = GetProtectedQuote
    | FetchProtectedQuoteCompleted (Result Http.Error String)
    | GetQuote
    | FetchRandomQuoteCompleted (Result Http.Error String)
