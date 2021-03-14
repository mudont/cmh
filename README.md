# Haskell beginner's playground

This is a Playground for various functionality imlemented in Haskell, Elm (and maybe other functional languages like Purescript)

Authentication, REST servers, websockets, Web SPA/PWA clients, Postgres, Mongo (with oplog tailing)
## Pre-requistes

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



## [TODO](doc/TODO.md)


## Tech stack
- Authentication: OpenId Connect Google/Microsoft
- Communication : REST + Websockets
- FRP: streamly seems to be a new lib (Claims to be close to Rx in design)
- Client options: 
  * Purescript + Halogen + REST+Websocket + Nice default styling
  * [x?] Elm
  * [ ] GHCJS Reflex-DOM
  * [ ] GHCJS Miso (Not FRP)
  * [ ] ReScript
- Servant + Selda + Websocket server + Postgres
- [MonadMetrics for metrics](https://hackage.haskell.org/package/monad-metrics)
- ### [Streamly for FRP](https://hackage.haskell.org/package/streamly)
  Actively maintained, Fastest streaming library, general purpose
- ### Effect systems
    - [Examples of effect systems for same program](https://github.com/stepchowfun/effects)
    - [~~Polysemy~~](https://github.com/polysemy-research/polysemy) was written by Sandy McGuire,
    author of "Thinking with Types" and "Algebra Driven Design". [Polysemy poor in benchmark](https://www.youtube.com/watch?v=0jI-AlWEwYI&t=260s) that [Sandy accepted](https://reasonablypolymorphic.com/blog/mea-culpa/)
    - [Tutorial](https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1/index.html)
- ### Other effect systems
  Effects are things like IO, random numbers, exceptions, mutable global state
  - [mtl](http://hackage.haskell.org/package/mtl) Basic, Super fast, O(n^2) instances, can't have more than one State, Reader etc. 
  - [freer-simple](http://hackage.haskell.org/package/freer-simple) Slow, no boilerplate, good type error messages
  - [fused effects](https://github.com/fused-effects/fused-effects) Super fast, lot of boilerplate
  - [Eff](https://github.com/hasura/eff) By Alexis King, author of freer-simple and that [video](https://www.youtube.com/watch?v=0jI-AlWEwYI&t=260s) on Effect systems performance. This is very new, requires GHC changes to boost performance. The changes are not yet accepted
  - [Polysemy](https://github.com/polysemy-research/polysemy) Very fast, little boilerplate, good error messages


## Other useful libraries
[Artyom's Haskell toolbox](https://toolbox.brick.do//) Lists the author's favorite libraries for various tasks

## Guided tours of Source code to learn from
- [Xmonad](https://wiki.haskell.org/Xmonad/Guided_tour_of_the_xmonad_source)
- [Realworld Purescript/Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld)
- [Elm Realworld Conduit client](https://github.com/rtfeldman/elm-spa-example)
## Concepts to learn better
- Type level programming. Read Mcguire book
-  McGuire also has a book Algebra Driven Design
- Effect systems
- FRP (use streamly?)

## References
[References](doc/References.md)