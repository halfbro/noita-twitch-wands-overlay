let app = {};
let auth = {};

function log(...x) {
    window.Twitch.ext.rig.log(x);
    console.log(x);
}

function extensionBackendRequest(endpoint, type, body) {
    return fetch(`https://wand-overlay.halfbro.xyz${endpoint}`, {
        method: type,
        headers: {
            "Authorization": `Bearer ${auth.token}`,
            "Content-type": "application/json"
        },
        body: body
    });
}

const testOnlywandsConnection = (channelId) => extensionBackendRequest(`/wand_info/${channelId}`, "GET");

//function twitchRequest(endpoint, type, body) {
//    return fetch(`https://api.twitch.tv/helix${endpoint}`, {
//        method: type,
//        headers: {
//            "Authorization": `Bearer ${auth.token}`,
//            "Content-type": "application/json"
//        },
//        body: body
//    });
//}

var timeout;
const updateConfig = (config) => {
    clearTimeout(timeout);
    timeout = setTimeout(() => {
        log("Updating config: ", config);
        window.Twitch.ext.configuration.set("broadcaster", "1", JSON.stringify(config));
        window.Twitch.ext.send("broadcast", "application/json", config);
    }, 300);
};

const configureElm = (channelId, settings) => {
    app = Elm.Config.init({
        node: document.getElementById('elm'),
        flags: {
            channelId: channelId,
            streamerSettings: settings
        }
    });
    app.ports.updateConfig.subscribe(updateConfig);
};

window.Twitch.ext.onAuthorized(newAuth => {
    auth = newAuth;
    initialBroadcasterConfig = JSON.parse(window.Twitch.ext.configuration.broadcaster.content);
    configureElm(auth.channelId, initialBroadcasterConfig);
    //testOnlywandsConnection(auth.channelId).then(info => info.text().then(body => {
    //    app.ports.twitchBroadcastPort.send(body);
    //}));
});

window.Twitch.ext.onError((err) => {
    log('TWITCH EXT ERROR', err);
});
