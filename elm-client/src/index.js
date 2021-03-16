import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

// Some Bootstrap Navbar related stuff
// https://bootstrap-menu.com/detail-offcanvas-collapse.html
$("[data-trigger]").on("click", function(){
    var trigger_id =  $(this).attr('data-trigger');
    $(trigger_id).toggleClass("show");
    $('body').toggleClass("offcanvas-active");
});

// close button
$(".btn-close").click(function(e){
    $(".navbar-collapse").removeClass("show");
    $("body").removeClass("offcanvas-active");
});
////////////////////////////////////

console.log('DBG in JS startup')
var storageKey = "store";
var flags = localStorage.getItem(storageKey);

var socket; // For websocket

function getTokenFromLs() {
    var store = localStorage.getItem(storageKey);
    if (store) {
        var storeObj = JSON.parse(store);
        if (storeObj && storeObj.user ) {
            return storeObj.user.token;
        }
    }
    return null;
}
// -- ELM APP

var app = Elm.Main.init({flags: flags});

console.log('DBG Elm initialized')

// -- LOCAL STORAGE

// Setup subscriptions to listen to the Elm App's requests
// to set/remove localStorage
function updateStorage(state) {
    console.log('DBG updateStorage called');
    if (state === null) {
       console.log('DBG murali Removing localStorage')
       localStorage.removeItem(storageKey);
    } else {
      console.log('DBG setStorage called by Elm App. Setting...')
      localStorage.setItem(storageKey, JSON.stringify(state));
    }

    // Report that the new session was stored successfully.
    setTimeout(function() { app.ports.onStoreChange.send(state); }, 0);

}
app.ports.storeCache.subscribe(updateStorage);
// Whenever localStorage changes in another tab, report it if necessary.
window.addEventListener("storage", function(event) {
        console.log('DBG Storage event...')
        if (event.storageArea === localStorage && event.key === storageKey) {
            console.log('DBG localStorage changed telling app...')
          app.ports.onStoreChange.send(event.newValue);
          setupWebsocket();
        }
    }, false
);

// -- CONSOLE LOGGING

console.log('DBG local storage is set up now');
app.ports.consoleErr && app.ports.consoleErr.subscribe(function (msg) {
    var ts = new Date().toISOString();
    console.error(ts + ' ' + msg);
});
app.ports.consoleWarn && app.ports.consoleWarn.subscribe(function (msg) {
    var ts = new Date().toISOString();
    console.warn(ts + ' ' + msg);
});
app.ports.consoleInfo && app.ports.consoleInfo.subscribe(function (msg) {
    var ts = new Date().toISOString();
    console.info(ts + ' ' + msg);
});
app.ports.consoleDbg && app.ports.consoleDbg.subscribe(function (msg) {
    var ts = new Date().toISOString();
    console.debug(ts + ' ' + msg);
});

setupWebsocket();

// -- SERVICE WORKER

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();


// -- WEBSOCKETS
function setupWebsocket() {
    var token = getTokenFromLs();
    if (! token) {
        console.log('No token. Not connecting to Websocket');
        return;
    }

    if (socket) {
      var GOING_AWAY = 1001;
      socket.close(GOING_AWAY, "New Login")
    }
    // Create  WebSocket client.
    var wsUrl = ((window.location.protocol === 'https:' ? "wss" : "ws") + '://' +
        window.location.host + "/stream?access-token="+ token);
    console.log("DBG Connecting to websocket url " + wsUrl)
    socket = new WebSocket( wsUrl);
    console.log("DBG Wiring Websocket to Elm")
    // When a command goes to the `sendMessage` port from the Elm app,
    // we pass the message along to the WebSocket.
    app.ports.sendWsMessage && app.ports.sendWsMessage.subscribe(function (message) {
      // Pass Elm message to Server
      socket.send(message);
    });
    // When a message comes into our WebSocket, we pass the message along
    // to the `messageReceiver` port to the Elm App.
    socket.addEventListener("message", function (event) {
      // Pass server message to Elm client
      console.log('WS data ' + JSON.stringify(event.data, 4))
      app.ports.wsMessageReceiver && app.ports.wsMessageReceiver.send(event.data);
    });
}

