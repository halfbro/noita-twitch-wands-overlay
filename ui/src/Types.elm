module Types exposing (..)

import Json.Decode as JD exposing (Decoder, bool, float, index, int, list, map2, maybe, string)
import Json.Decode.Pipeline exposing (required)


type alias WandInformation =
    { wands : List String
    , inventory : List String
    }


type alias SpellName =
    String


type alias Inventory =
    List SpellName


type alias WandStats =
    { sprite : String
    , reloadTime : Int
    , manaChargeSpeed : Float
    , spreadDegrees : Float
    , shuffleDeckWhenEmpty : Bool
    , uiName : Maybe Int
    , manaMax : Float
    , actionsPerRound : Int
    , speedMultiplier : Float
    , fireRateWait : Int
    , deckCapacity : Int
    }


decodeWandStats : Decoder WandStats
decodeWandStats =
    JD.succeed WandStats
        |> required "sprite" string
        |> required "reload_time" int
        |> required "mana_charge_speed" float
        |> required "spread_degrees" float
        |> required "shuffle_deck_when_empty" bool
        |> required "ui_name" (maybe int)
        |> required "mana_max" float
        |> required "actions_per_round" int
        |> required "speed_multiplier" float
        |> required "fire_rate_wait" int
        |> required "deck_capacity" int


type alias Wand =
    { stats : WandStats
    , alwaysCast : List SpellName
    , deck : List SpellName
    }


decodeWand : Decoder Wand
decodeWand =
    JD.succeed Wand
        |> required "stats" decodeWandStats
        |> required "always_cast" (list string)
        |> required "deck" (list string)


type alias StreamerInformation =
    ( List Wand, Inventory )


blankInfo : StreamerInformation
blankInfo =
    ( [], [] )


decodeUpdate : Decoder StreamerInformation
decodeUpdate =
    map2 Tuple.pair (index 0 (list decodeWand)) (index 1 (list string))
