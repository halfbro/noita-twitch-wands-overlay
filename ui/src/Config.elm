port module Config exposing (..)

import Browser
import Css
import Html.Styled exposing (..)
import SingleSlider
import Types exposing (StreamerSettings, newStreamerSettings)


port updateConfig : StreamerSettings -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = toUnstyled << view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Flags =
    { channelId : String
    , streamerSettings : Maybe StreamerSettings
    }


init : Flags -> ( Model, Cmd msg )
init { streamerSettings } =
    let
        newSlider initial onChange =
            SingleSlider.init
                { min = 0.0
                , max = 100.0
                , step = 0.1
                , value = initial
                , onChange = onChange
                }

        initialSettings =
            Maybe.withDefault newStreamerSettings streamerSettings

        m : Model
        m =
            { wandsStartXSlider = newSlider initialSettings.wandBoxLeft ChangeWandBoxX
            , wandsStartYSlider = newSlider initialSettings.wandBoxTop ChangeWandBoxY
            , wandsBoxWidthSlider = newSlider initialSettings.wandBoxWidth ChangeWandBoxWidth
            }
    in
    ( m, Cmd.none )


type alias Model =
    { wandsStartXSlider : SingleSlider.SingleSlider Msg
    , wandsStartYSlider : SingleSlider.SingleSlider Msg
    , wandsBoxWidthSlider : SingleSlider.SingleSlider Msg
    }


modelToConfig : Model -> StreamerSettings
modelToConfig m =
    { wandBoxLeft = SingleSlider.fetchValue m.wandsStartXSlider
    , wandBoxTop = SingleSlider.fetchValue m.wandsStartYSlider
    , wandBoxWidth = SingleSlider.fetchValue m.wandsBoxWidthSlider
    }


type Msg
    = ChangeWandBoxX Float
    | ChangeWandBoxY Float
    | ChangeWandBoxWidth Float


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ChangeWandBoxX v ->
            let
                newModel =
                    { model | wandsStartXSlider = SingleSlider.update v model.wandsStartXSlider }
            in
            ( newModel, updateConfig <| modelToConfig newModel )

        ChangeWandBoxY v ->
            let
                newModel =
                    { model | wandsStartYSlider = SingleSlider.update v model.wandsStartYSlider }
            in
            ( newModel, updateConfig <| modelToConfig newModel )

        ChangeWandBoxWidth v ->
            let
                newModel =
                    { model | wandsBoxWidthSlider = SingleSlider.update v model.wandsBoxWidthSlider }
            in
            ( newModel, updateConfig <| modelToConfig newModel )


view : Model -> Html Msg
view model =
    div []
        [ viewSliders model
        ]


viewSliders : Model -> Html Msg
viewSliders model =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        ]
        []
        [ fromUnstyled <| SingleSlider.view model.wandsStartXSlider
        , fromUnstyled <| SingleSlider.view model.wandsStartYSlider
        , fromUnstyled <| SingleSlider.view model.wandsBoxWidthSlider
        ]
