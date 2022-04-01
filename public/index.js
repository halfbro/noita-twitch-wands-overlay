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

function log(...x) {
    window.Twitch.ext.rig.log(x);
    console.log(x);
}

const makeGetColorRequest = () => createRequest("/color", "GET");
const makeSetColorRequest = () => createRequest("/color", "POST", '{"newColor": 180}');

app.ports.sendHueBroadcast.subscribe(hue => {
    window.Twitch.ext.send("broadcast", "application/json", hue);
    log("Sent broadcast with message: " + hue);
});

window.Twitch.ext.listen("broadcast", (target, contentType, message) => {
    log("Received broadcast with message: " + message);
    const i = parseInt(message);
    app.ports.receiveHueBroadcast.send(i);
});

window.Twitch.ext.onAuthorized(newAuth => {
    log(newAuth.token);
    auth = newAuth;

    const test =
          makeGetColorRequest()
          .then(response => response.text())
          .then(body => {
              log('Response: ', body);
              app.ports.receiveHueBroadcast.send(parseInt(body));
          })
          .catch(e => {
              log('Error: ', e);
          });
});
