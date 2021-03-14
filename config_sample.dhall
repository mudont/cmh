let port
    : Integer
    = +8080

in  { port
    , dbHost = "localhost"
    , dbPort = 5432
    , database = "haskell"
    , schema = None Text
    , dbUsername = Some "postgres"
    , dbPassword = None Text
    , redirectUri = "/google/cb"
    , clientId =
        "100140516089-some_kind_of_appi07kmvl3v5i3vcd7.apps.googleusercontent.com"
    , clientPassword = "5NOT_REALxoFAKE0mlq3pod_"
    , jwkFile = "jwk.dat"
    , website = "https://mariandrive.com"
    , profileUrl = "#/settings"
    }
