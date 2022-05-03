port module Main exposing (..)

import Browser
import Dict exposing (Dict, empty)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (Error, Value, decodeString, decodeValue, dict, string)
import List exposing (length)
import Result exposing (withDefault)
import Round as Round
import String exposing (fromFloat, fromInt)
import Types exposing (..)


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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { channelId : String
    , spellData : Value
    , wandSprites : Value
    }


init : Flags -> ( Model, Cmd msg )
init flags =
    let
        m =
            Model
                blankInfo
                flags.channelId
                (withDefault empty <| decodeValue (dict decodeSpell) flags.spellData)
                (withDefault empty <| decodeValue (dict string) flags.wandSprites)
    in
    ( m, Cmd.none )


type Msg
    = ReceivedWandUpdate StreamerInformation
    | BadWandUpdate Error


type alias Model =
    { streamerInfo : StreamerInformation
    , streamerId : String
    , spellData : SpellData
    , wandSprites : WandSprites
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ReceivedWandUpdate info ->
            ( { model | streamerInfo = info }, Cmd.none )

        BadWandUpdate e ->
            let
                _ =
                    Debug.log "Unexpected Wand Update error" e
            in
            ( model, Cmd.none )


view : Model -> Html msg
view model =
    div []
        [ div [ class "wands-container" ] (List.map (viewWand model.spellData model.wandSprites) model.streamerInfo.wands)
        ]


viewWandStats : Wand -> Html msg
viewWandStats wand =
    let
        stats =
            wand.stats

        sanitizeClassName =
            String.toLower
                << String.map
                    (\c ->
                        case c of
                            '/' ->
                                '-'

                            ' ' ->
                                '-'

                            x ->
                                x
                    )

        trStat label stat =
            tr [] [ td [ class <| sanitizeClassName label, class "stat" ] [ text label ], td [] [ text stat ] ]

        showTimeInteger : Int -> String
        showTimeInteger i =
            let
                hundreths =
                    round (toFloat i * 5 / 3)

                seconds =
                    hundreths // 100

                remainder =
                    remainderBy 100 hundreths
            in
            fromInt seconds ++ "." ++ fromInt remainder
    in
    div [ class "wand-stats" ]
        [ table []
            [ trStat "Shuffle" <|
                if stats.shuffleDeckWhenEmpty then
                    "Yes"

                else
                    "No"
            , trStat "Spells/Cast" <| fromInt stats.actionsPerRound
            , trStat "Cast Delay" <| showTimeInteger stats.fireRateWait
            , trStat "Recharge Time" <| showTimeInteger stats.reloadTime
            , trStat "Mana Max" <| fromFloat stats.manaMax
            , trStat "Mana chg spd" <| fromFloat stats.manaChargeSpeed
            , trStat "Capacity" <| fromInt <| stats.deckCapacity - length wand.alwaysCast
            , trStat "Spread" <| Round.round 1 stats.spreadDegrees ++ " DEG"
            ]
        ]


viewSpellSlot : Maybe Spell -> Html msg
viewSpellSlot spell =
    div [ class "spell-slot" ] <|
        case spell of
            Nothing ->
                []

            Just actualSpell ->
                [ img [ class "spell-sprite", src <| "data:image/png;base64, " ++ actualSpell.sprite ] []
                ]


viewSpellDeck : SpellData -> List SpellName -> Html msg
viewSpellDeck spellData spellNames =
    let
        spells =
            spellNames
                |> List.map (\name -> Dict.get name spellData)
    in
    div [ class "spell-deck" ] <| List.map viewSpellSlot spells


viewWandAlwaysCast : SpellData -> Wand -> Html msg
viewWandAlwaysCast spellData wand =
    case wand.alwaysCast of
        [] ->
            div [] []

        spells ->
            div [ class "always-cast" ]
                [ p [] [ text "Always Cast:" ]
                , viewSpellDeck spellData spells
                ]


viewWand : SpellData -> WandSprites -> Wand -> Html msg
viewWand spellData wandSprites wand =
    let
        imageBase64 =
            "data:image/png;base64, " ++ (Dict.get wand.stats.sprite wandSprites |> Maybe.withDefault "")

        sprite =
            img [ src imageBase64, class "wand-image" ] []

        statsSection =
            viewWandStats wand

        alwaysCastSection =
            viewWandAlwaysCast spellData wand

        deckPadding =
            List.repeat (wand.stats.deckCapacity - length wand.alwaysCast - length wand.deck) "0"

        deckSection =
            div [ class "deck" ]
                [ p [] [ text "Spells:" ]
                , viewSpellDeck spellData (wand.deck ++ deckPadding)
                ]
    in
    div [ class "wand" ]
        [ div [ class "wand-upper-section" ] [ statsSection, sprite ], alwaysCastSection, deckSection ]
