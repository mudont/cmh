port module Log exposing (error, dbg, info, warn)

{-| This is a placeholder API for how we might do logging through
some service like <http://rollbar.com> (which is what we use at work).

Whenever you see Log.error used in this code base, it means
"Something unexpected happened. This is where we would log an
error to our server with some diagnostic info so we could investigate
what happened later."

(Since this is outside the scope of the RealWorld spec, and is only
a placeholder anyway, I didn't bother making this function accept actual
diagnostic info, authentication tokens, etc.)

-}
port consoleDbg : String -> Cmd msg
port consoleErr : String -> Cmd msg
port consoleInfo : String -> Cmd msg
port consoleWarn : String -> Cmd msg

dbg : String -> Cmd msg
dbg = consoleDbg
error : String -> Cmd msg
error = consoleErr
info : String -> Cmd msg
info = consoleInfo
warn : String -> Cmd msg
warn = consoleWarn