module Archetype where

data BattingArch = Utility | Streak | Handyman | Cannon | Magician | BMOC | Freak
    deriving (Eq, Ord, Show)

data PitchingArch = FlameSP | ControlSP | JunkSP | KnuckleSP | FlameRP | ControlRP | JunkRP
    deriving (Eq, Ord, Show)
