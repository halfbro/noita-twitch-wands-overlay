let app = {};
let auth = {};

function log(...x) {
    window.Twitch.ext.rig.log(x);
    console.log(x);
}

function extensionBackendRequest(endpoint, type, body) {
    return fetch(`${location.protocol}//localhost:7999${endpoint}`, {
        method: type,
        headers: {
            "Authorization": `Bearer ${auth.token}`,
            "Content-type": "application/json"
        },
        body: body
    });
}

const getInitialWandsForChannel = (channelId) => extensionBackendRequest(`/wand_info/${channelId}`, "GET");


function twitchRequest(endpoint, type, body) {
    return fetch(`https://api.twitch.tv/helix/${endpoint}`, {
        method: type,
        headers: {
            "Authorization": `Bearer ${auth.token}`,
            "Content-type": "application/json"
        },
        body: body
    });
}

const configureElm = (channelId) => {
    app = Elm.Main.init({
        node: document.getElementById('elm'),
        flags: {channelId: channelId, spellData: spellData, wandSprites: wandSprites}
    });
};

window.Twitch.ext.listen('broadcast', (target, contentType, message) => {
    log(`Received broadcast: ${message}`);
    app.ports.twitchBroadcastPort.send(message);
});

window.Twitch.ext.onAuthorized(newAuth => {
    auth = newAuth;
    configureElm(auth.channelId);
    getInitialWandsForChannel(auth.channelId).then(info => info.text().then(body => {
        log('initial state', body);
        app.ports.twitchBroadcastPort.send(body);
    }));
});

window.Twitch.ext.onError((err) => {
    log('TWITCH EXT ERROR', err);
});
