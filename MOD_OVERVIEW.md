The extension is a simple hover overlay for Noita that gets its data from a game mod. In Noita, a streamer can build custom wands from an array of spells and effects. This overlay allows viewers to view contents of the streamer's wands at all times.

The hover location is located in the top left of the player, near where the wands are located in the game UI. Hovering over a wand will show detailed information about the wand, including which spells are on the wands and the wand's stats.

The source code for this extension can be found on Github at https://github.com/halfbro/noita-twitch-wands-overlay. The bulk of the code is written in Elm, so the generated javascript file may not be very readable. The code that isn't generated by Elm is all vanilla javascript though, and should be easily readable.

The game mod sends wand updates to a backend service via websockets every second if there are changes. Once in the backend, there are some small modifications to the data, and the changes are exposed via a separate websocket. This is done so that other clients (like a separate browser tab (onlywands.com)) are also able to hook into these updates. For the purpose of the extension, this step is just cleaning up the data a bit.

The EBS reads from the cleaned up websocket and sends the updates to Twitch via the pubsub broadcast message for the streamer. In order to have the EBS only send messages to Twitch when necessary, the overlay initiates a GET request to 'https://wand-overlay.halfbro.xyz/wand_info/{streamerName}', which starts the streaming process for the given streamer.

After sending the GET request, the overlay sets up a callback using 'Twitch.ext.listen('broadcast', ...)' to update the view.

Streamers can configure the overlay by using sliders to position the hover boxes within the stream window. This is to accomodate streamers who move or scale their in-game UI so that the hover boxes line up with their new locations.
