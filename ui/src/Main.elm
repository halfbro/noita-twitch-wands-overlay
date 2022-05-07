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
    div
        [ class "magic-hover-box"
        ]
        [ div [ class "wands-container" ]
            (List.map (viewWandBrief model.spellData model.wandSprites) model.streamerInfo.wands)
        ]


viewWandBrief : SpellData -> WandSprites -> Wand -> Html msg
viewWandBrief spellData wandSprites wand =
    let
        deckPadding =
            List.repeat (wand.stats.deckCapacity - length wand.alwaysCast - length wand.deck) "0"
    in
    div [ class "wand-brief" ]
        [ div [ class "wand-brief-upper" ]
            [ viewWandSprite wandSprites wand False
            , table []
                [ tr []
                    [ td [ class "stat" ] [ text "Shuffle" ]
                    , td []
                        [ text <|
                            if wand.stats.shuffleDeckWhenEmpty then
                                "Yes"

                            else
                                "No"
                        ]
                    ]
                , tr []
                    [ td [ class "stat" ] [ text "Spells/Cast" ]
                    , td []
                        [ text <| fromInt wand.stats.actionsPerRound ]
                    ]
                ]
            ]
        , viewSpellDeck spellData (wand.deck ++ deckPadding)
        , div [class "wand-detail" ] [viewWandDetails spellData wandSprites wand]
        ]


viewWandSprite : WandSprites -> Wand -> Bool -> Html msg
viewWandSprite wandSprites wand isRotated =
    let
        imageBase64 =
            "data:image/png;base64, " ++ (Dict.get wand.stats.sprite wandSprites |> Maybe.withDefault "")
    in
    img
        ([ src imageBase64, class "wand-image" ]
            ++ (if isRotated then
                    [ class "rotated" ]

                else
                    []
               )
        )
        []


viewWandDetails : SpellData -> WandSprites -> Wand -> Html msg
viewWandDetails spellData wandSprites wand =
    let
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
        [ div [ class "wand-upper-section" ] [ statsSection, viewWandSprite wandSprites wand True ], alwaysCastSection, deckSection ]


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
            Round.round 2 <| (toFloat i * 5 / 300)
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
            , trStat "Mana Max" <| Round.round 0 stats.manaMax
            , trStat "Mana chg spd" <| Round.round 0 stats.manaChargeSpeed
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
            text ""

        spells ->
            div [ class "always-cast" ]
                [ p [] [ text "Always Cast:" ]
                , viewSpellDeck spellData spells
                ]
