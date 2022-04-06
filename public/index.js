var app = Elm.Main.init({
    node: document.getElementById('elm')
});

let auth = {};
function createRequest(endpoint, type, body) {
    return fetch(location.protocol + '//localhost:7999' + endpoint, {
        method: type,
        headers: { "Authorization": "Bearer " + auth.token,
                   "Content-type": "application/json"
                 },
        body: body
    });
}

//const makeGetColorRequest = () => createRequest("/color", "GET");
//const makeSetColorRequest = () => createRequest("/color", "POST", '{"color": 180}');

app.ports.sendHueBroadcast.subscribe(hue => {
    let package = {color: hue};
    window.Twitch.ext.send('broadcast', "application/json", package);
    log(`Sent broadcast with message: ${package}`);
});

window.Twitch.ext.onAuthorized(newAuth => {
    log(newAuth.token);
    auth = newAuth;
});

function log(...x) {
    window.Twitch.ext.rig.log(x);
    console.log(x);
}

window.Twitch.ext.onError((err) => {
    log('TWITCH EXT ERROR', err);
});

window.Twitch.ext.listen('broadcast', (target, contentType, message) => {
    let msg = JSON.parse(message);
    log(`Received broadcast with message.color: ${msg.color}`);
    app.ports.receiveHueBroadcast.send(msg.color);
});

setTimeout(() => {
    window.Twitch.ext.send('broadcast', 'application/json', {color: 340});
    log("asdf");
}, 3000);
