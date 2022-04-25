let app = {};
let auth = {};

function log(...x) {
    window.Twitch.ext.rig.log(x);
    console.log(x);
}

//function createRequest(endpoint, type, body) {
    //return fetch(`${location.protocol}//localhost:7999${endpoint}`, {
        //method: type,
        //headers: { "Authorization": `Bearer ${auth.token}`,
                   //"Content-type": "application/json"
                 //},
        //body: body
    //});
//}

function twitchRequest(endpoint, type, body) {
    return fetch(`https://api.twitch.tv/helix/${endpoint}`, {
        method: type,
        headers: { "Authorization": `Bearer ${auth.token}`,
                   "Content-type": "application/json"
                 },
        body: body
    });
}

const configureElm = (channelId) => {
    app = Elm.Main.init({
        node: document.getElementById('elm'),
        flags: channelId
    });
};

window.Twitch.ext.onAuthorized(newAuth => {
    log(newAuth);
    auth = newAuth;
    configureElm(auth.channelId);
});

window.Twitch.ext.onError((err) => {
    log('TWITCH EXT ERROR', err);
});

window.Twitch.ext.listen('broadcast', (target, contentType, message) => {
    log(`Received broadcast: ${message}`);
    app.ports.twitchBroadcastPort.send(message);
});

setTimeout(() => {
    window.Twitch.ext.send('broadcast', 'application/json', {color: 340});
}, 3000);
