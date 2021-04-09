module Attribute where


data BattingAttr = BabipL | BabipR | AvoidKL | AvoidKR | GapL | GapR | PowerL | PowerR | EyeL | EyeR | Speed | Steal | Bunt | Range | Error | Arm | DP | Catcher
    deriving (Eq, Ord, Show)

data PitchingAttr = MovementL | MovementR | ControlL | ControlR | Stamina | Hold | Pitch1 | Pitch2 | Pitch3 | Pitch4 | Pitch5 | Velocity
    deriving (Eq, Ord, Show)
