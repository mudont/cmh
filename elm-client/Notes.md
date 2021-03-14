
Routing in realworld SPA

If we use Broswer.application which is the most powerful in the line
Browser.
- sandbox, -- only DOM
- element, -- + commands/ subs
- document, -- + Full Doucment 
- application -- + Url control
  
we get to specifiy
onUrlRequest: UrlRequest -> msg, -- when someone clicks a link in the app
onUrlChange: Url -> msg        -- When the url is changed
 Main: 

    import Route exposing (Route)

