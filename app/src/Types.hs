module Types
  ( Inventory,
    SpellName,
    StreamerInformation,
    blankStreamerInformation,
    Wand,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

type SpellName = String

type Inventory = [SpellName]

data WandStats = WandStats
  { sprite :: String,
    reload_time :: Integer,
    mana_charge_speed :: Double,
    spread_degrees :: Double,
    shuffle_deck_when_empty :: Bool,
    ui_name :: Maybe Integer, -- May actually be a string for special wands
    mana_max :: Double,
    actions_per_round :: Integer,
    speed_multiplier :: Double,
    fire_rate_wait :: Integer,
    deck_capacity :: Integer
  }
  deriving (Generic, Show, Eq)

instance ToJSON WandStats

instance FromJSON WandStats

data Wand = Wand
  { stats :: WandStats,
    always_cast :: [SpellName],
    deck :: [SpellName]
  }
  deriving (Generic, Show, Eq)

instance ToJSON Wand

instance FromJSON Wand

type StreamerInformation = ([Wand], Inventory)

blankStreamerInformation :: StreamerInformation
blankStreamerInformation = ([], [])
