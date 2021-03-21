# CM Hackers Tennis team Website


## Installation Pre-requistes

If you don't have these already, install:
- [Stack](https://docs.haskellstack.org/en/stable/README/):\
   ```curl -sSL https://get.haskellstack.org/ | sh```
- [Elm](https://elm-lang.org/):
- [create-elm-app](https://github.com/halfzebra/create-elm-app):\
  ```npm install create-elm-app -g```
- [PosgreSQL](https://www.postgresql.org/)

## Preliminary steps
- `mv config_sample.dhall config.dhall`
- Edit `config.dhall` and make appropriate changes
- opaleye-gen -d postgresql://localhost/tennis -o src/DB/Opaleye/Database.hs  # Uses modifed opaleye-gen
- 
## Building

```
stack build
cd elm-client
elm-app build
```
## Haskell Servant + OIDC.Client Google Auth , Elm client

 1. client -> app server
 2. server redirects to Google and specifies a callback url
 3. Google handles authentication and redirects client to callback url
 4. Server gets the user details, creates a JWT and returns HTML+JS to the client
    sticking the token into localStorage. Also sets window.location to the Elm app's
    index.html
 5. Elm app has JS in index.html to initialize the Elm model from localStorage


### Guid to current code 02/17/2020
#### Elm Client
in directory `elm-client`, bootstrapped with create-elm-app. Therefore it is a PWA

`elm-app build` creates a production build in the `build` directory.

the elm code is in `src/Main.elm`. The toplevel Javascript code is in `src/index.js`

The JS code initializes the model from localStorage. it gets authenticated email address and an 
access-token


## Tech stack
- Authentication: OpenId Connect Google
- Communication : REST + Websockets
