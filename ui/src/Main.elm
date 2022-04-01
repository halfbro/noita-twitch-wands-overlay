port module Main exposing (..)

import Browser
import Color exposing (hsl, toCssString)
import Debug exposing (toString)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


port sendHueBroadcast : Int -> Cmd msg


port receiveHueBroadcast : (Int -> msg) -> Sub msg


---------------------


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { hue = 180 }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> receiveHueBroadcast UpdateColor
        }


type Msg
    = ChangeColor
    | UpdateColor Int


type alias Model =
    { hue : Int
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ChangeColor ->
            let
                newHue =
                    modBy 360 (model.hue + 20)
            in
            ( { model | hue = newHue }, sendHueBroadcast newHue )

        UpdateColor newHue ->
            ( { model | hue = newHue }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text "Hello World!"
        , button [ onClick ChangeColor ] [ text "Change Color" ]
        , div
            [ style "backgroundColor" (toCssString <| hsl (toFloat model.hue / 360.0) 0.5 0.5)
            , style "height" "100px"
            ]
            [ text "asdf" ]
        ]
