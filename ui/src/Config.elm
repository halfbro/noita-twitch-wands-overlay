port module Config exposing (..)

import Browser
import Css
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (src)
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
        newSlider min max initial onChange =
            SingleSlider.init
                { min = min
                , max = max
                , step = 0.05
                , value = initial
                , onChange = onChange
                }

        initialSettings =
            Maybe.withDefault newStreamerSettings streamerSettings

        m : Model
        m =
            { wandsBoxStartXSlider = newSlider 0 40 initialSettings.wandBoxLeft ChangeWandBoxStartX
            , wandsBoxStartYSlider = newSlider 0 20 initialSettings.wandBoxTop ChangeWandBoxY
            , wandsBoxEndXSlider = newSlider 50 90 initialSettings.wandBoxRight ChangeWandBoxEndX
            }
    in
    ( m, Cmd.none )


type alias Model =
    { wandsBoxStartXSlider : SingleSlider.SingleSlider Msg
    , wandsBoxEndXSlider : SingleSlider.SingleSlider Msg
    , wandsBoxStartYSlider : SingleSlider.SingleSlider Msg
    }


modelToConfig : Model -> StreamerSettings
modelToConfig m =
    { wandBoxLeft = SingleSlider.fetchValue m.wandsBoxStartXSlider
    , wandBoxTop = SingleSlider.fetchValue m.wandsBoxStartYSlider
    , wandBoxRight = SingleSlider.fetchValue m.wandsBoxEndXSlider
    }


type Msg
    = ChangeWandBoxStartX Float
    | ChangeWandBoxEndX Float
    | ChangeWandBoxY Float


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ChangeWandBoxStartX v ->
            let
                newModel =
                    { model | wandsBoxStartXSlider = SingleSlider.update v model.wandsBoxStartXSlider }
            in
            ( newModel, updateConfig <| modelToConfig newModel )

        ChangeWandBoxEndX v ->
            let
                newModel =
                    { model | wandsBoxEndXSlider = SingleSlider.update v model.wandsBoxEndXSlider }
            in
            ( newModel, updateConfig <| modelToConfig newModel )

        ChangeWandBoxY v ->
            let
                newModel =
                    { model | wandsBoxStartYSlider = SingleSlider.update v model.wandsBoxStartYSlider }
            in
            ( newModel, updateConfig <| modelToConfig newModel )


view : Model -> Html Msg
view model =
    styled div
        [ Css.padding (Css.px 12)
        , Css.margin (Css.px 5)
        , Css.border3 (Css.px 2) Css.solid (Css.rgb 255 255 255)
        , Css.borderRadius (Css.px 4)
        , Css.backgroundColor (Css.rgba 17 13 12 0.8)
        ]
        []
        [ viewSliders model
        ]


viewSliders : Model -> Html Msg
viewSliders model =
    let
        viewSliderWithHelp slider imgPath =
            styled div
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.alignItems Css.center
                , Css.property "gap" "20px"
                ]
                []
                [ styled div [Css.flex2 (Css.num 1) (Css.num 0)] [] [fromUnstyled <| SingleSlider.view slider]
                , img [ src imgPath ] []
                ]
    in
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.property "gap" "20px"
        ]
        []
        [ Css.Global.global
            [ Css.Global.class "input-range"
                [ Css.width (Css.pct 100)]
            , Css.Global.class "input-range-label"
                [ Css.margin (Css.px 20)]
            , Css.Global.class "input-range-labels-container"
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.justifyContent Css.spaceBetween
                ]
            ]
        , viewSliderWithHelp model.wandsBoxStartXSlider "./config/Horizontal-Adjustment-Help.png"
        , viewSliderWithHelp model.wandsBoxStartYSlider "./config/Vertical-Adjustment-Help.png"
        , viewSliderWithHelp model.wandsBoxEndXSlider "./config/Width-Adjustment-Help.png"
        ]
