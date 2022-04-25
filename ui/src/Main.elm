port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Json.Decode exposing (Error, Value, decodeString)
import Types exposing (..)


port subPort : (Value -> msg) -> Sub msg


port twitchBroadcastPort : (String -> msg) -> Sub msg



---------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ twitchBroadcastPort
            (\str ->
                case decodeString decodeUpdate str of
                    Ok i ->
                        ReceivedWandUpdate i

                    Err e ->
                        BadWandUpdate e
            )
        ]


main : Program String Model Msg
main =
    Browser.element
        { init = \channelId -> ( { streamerWands = blankInfo, streamerId = channelId }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = ReceivedWandUpdate StreamerInformation
    | BadWandUpdate Error


type alias Model =
    { streamerWands : StreamerInformation
    , streamerId : String
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ReceivedWandUpdate info ->
            ( { model | streamerWands = info }, Cmd.none )

        BadWandUpdate e ->
            let
                _ =
                    Debug.log "Unexpected Wand Update error" e
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text "Hello World!"
        , button [] [ text "Change Color" ]
        , div
            [ style "height" "100px"
            ]
            [ text (Debug.toString model.streamerWands) ]
        ]
