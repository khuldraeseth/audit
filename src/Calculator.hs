module Calculator where

import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Archetype
import Attribute
import Player


battingBounds :: M.Map BattingArch (M.Map BattingAttr (Int, Int))
battingBounds = M.fromList
    [ (Utility, M.fromList [(BabipL, (30, 85)), (BabipR, (30, 85)), (AvoidKL, (30, 75)), (AvoidKR, (30, 75)), (GapL, (25, 75)), (GapR, (25, 75)), (PowerL, (25, 70)), (PowerR, (25, 70)), (EyeL, (30, 80)), (EyeR, (30, 80)), (Speed, (25, 60)), (Steal, (25, 60)), (Bunt, (5, 40)), (Range, (45, 85)), (Error, (35, 75)), (Arm, (35, 70)), (DP, (30, 70)), (Catcher, (25, 75))])
    , (Streak, M.fromList [(BabipL, (35, 90)), (BabipR, (35, 90)), (AvoidKL, (35, 75)), (AvoidKR, (35, 75)), (GapL, (35, 70)), (GapR, (35, 70)), (PowerL, (30, 60)), (PowerR, (30, 60)), (EyeL, (35, 75)), (EyeR, (35, 75)), (Speed, (50, 85)), (Steal, (50, 85)), (Bunt, (35, 70)), (Range, (40, 90)), (Error, (30, 70)), (Arm, (25, 60)), (DP, (35, 80)), (Catcher, (5, 10))])
    , (Handyman, M.fromList [(BabipL, (30, 80)), (BabipR, (30, 80)), (AvoidKL, (30, 70)), (AvoidKR, (30, 70)), (GapL, (25, 70)), (GapR, (25, 70)), (PowerL, (25, 75)), (PowerR, (25, 75)), (EyeL, (30, 85)), (EyeR, (30, 85)), (Speed, (25, 50)), (Steal, (10, 25)), (Bunt, (40, 75)), (Range, (20, 70)), (Error, (45, 90)), (Arm, (30, 70)), (DP, (20, 60)), (Catcher, (45, 90))])
    , (Cannon, M.fromList [(BabipL, (25, 60)), (BabipR, (25, 60)), (AvoidKL, (25, 65)), (AvoidKR, (25, 65)), (GapL, (30, 80)), (GapR, (30, 80)), (PowerL, (40, 95)), (PowerR, (40, 95)), (EyeL, (40, 80)), (EyeR, (40, 80)), (Speed, (15, 40)), (Steal, (5, 30)), (Bunt, (5, 25)), (Range, (25, 75)), (Error, (30, 80)), (Arm, (45, 100)), (DP, (25, 70)), (Catcher, (20, 80))])
    , (Magician, M.fromList [(BabipL, (30, 80)), (BabipR, (30, 80)), (AvoidKL, (40, 85)), (AvoidKR, (40, 85)), (GapL, (30, 65)), (GapR, (30, 65)), (PowerL, (20, 50)), (PowerR, (20, 50)), (EyeL, (35, 75)), (EyeR, (35, 75)), (Speed, (40, 70)), (Steal, (40, 70)), (Bunt, (40, 80)), (Range, (40, 95)), (Error, (40, 90)), (Arm, (35, 80)), (DP, (40, 90)), (Catcher, (5, 10))])
    , (BMOC, M.fromList [(BabipL, (25, 70)), (BabipR, (25, 70)), (AvoidKL, (25, 70)), (AvoidKR, (25, 70)), (GapL, (35, 95)), (GapR, (35, 95)), (PowerL, (40, 100)), (PowerR, (40, 100)), (EyeL, (30, 90)), (EyeR, (30, 90)), (Speed, (5, 25)), (Steal, (5, 10)), (Bunt, (5, 10)), (Range, (20, 50)), (Error, (20, 40)), (Arm, (10, 40)), (DP, (10, 40)), (Catcher, (25, 65))])
    , (Freak, M.fromList [(BabipL, (25, 65)), (BabipR, (25, 65)), (AvoidKL, (20, 60)), (AvoidKR, (20, 60)), (GapL, (30, 80)), (GapR, (30, 80)), (PowerL, (45, 100)), (PowerR, (45, 100)), (EyeL, (30, 80)), (EyeR, (30, 80)), (Speed, (30, 65)), (Steal, (30, 65)), (Bunt, (5, 10)), (Range, (40, 90)), (Error, (30, 60)), (Arm, (40, 70)), (DP, (25, 60)), (Catcher, (5, 10))])
    ]

pitchingBounds :: M.Map PitchingArch (M.Map PitchingAttr (Int, Int))
pitchingBounds = M.fromList
    [ (FlameSP, M.fromList [(Velocity, (94, 98)), (MovementL, (30, 95)), (MovementR, (30, 95)), (ControlL, (30, 75)), (ControlR, (30, 75)), (Stamina, (50, 85)), (Hold, (40, 85)), (Pitch1, (50, 95)), (Pitch2, (30, 85)), (Pitch3, (30, 80)), (Pitch4, (0, 70)), (Pitch5, (0, 60))])
    , (ControlSP, M.fromList [(Velocity, (90, 95)), (MovementL, (40, 85)), (MovementR, (40, 85)), (ControlL, (40, 95)), (ControlR, (40, 95)), (Stamina, (50, 75)), (Hold, (40, 85)), (Pitch1, (45, 95)), (Pitch2, (40, 85)), (Pitch3, (40, 75)), (Pitch4, (0, 70)), (Pitch5, (0, 60))])
    , (JunkSP, M.fromList [(Velocity, (88, 92)), (MovementL, (40, 90)), (MovementR, (40, 90)), (ControlL, (40, 85)), (ControlR, (40, 85)), (Stamina, (50, 75)), (Hold, (40, 85)), (Pitch1, (50, 95)), (Pitch2, (45, 90)), (Pitch3, (45, 90)), (Pitch4, (0, 80)), (Pitch5, (0, 70))])
    , (KnuckleSP, M.fromList [(Velocity, (75, 91)), (MovementL, (30, 90)), (MovementR, (30, 90)), (ControlL, (35, 85)), (ControlR, (35, 85)), (Stamina, (20, 70)), (Hold, (30, 85)), (Pitch1, (40, 95)), (Pitch2, (30, 90)), (Pitch3, (30, 90)), (Pitch4, (0, 80)), (Pitch5, (0, 0))])
    , (FlameRP, M.fromList [(Velocity, (93, 100)), (MovementL, (40, 95)), (MovementR, (40, 95)), (ControlL, (40, 65)), (ControlR, (40, 65)), (Stamina, (10, 40)), (Hold, (40, 90)), (Pitch1, (50, 95)), (Pitch2, (50, 90)), (Pitch3, (40, 80)), (Pitch4, (0, 70)), (Pitch5, (0, 60))])
    , (ControlRP, M.fromList [(Velocity, (91, 96)), (MovementL, (50, 85)), (MovementR, (50, 85)), (ControlL, (50, 95)), (ControlR, (50, 95)), (Stamina, (10, 30)), (Hold, (40, 90)), (Pitch1, (55, 100)), (Pitch2, (50, 85)), (Pitch3, (40, 80)), (Pitch4, (0, 70)), (Pitch5, (0, 60))])
    , (JunkRP, M.fromList [(Velocity, (90, 93)), (MovementL, (50, 95)), (MovementR, (50, 95)), (ControlL, (45, 80)), (ControlR, (45, 80)), (Stamina, (10, 30)), (Hold, (45, 90)), (Pitch1, (60, 100)), (Pitch2, (50, 95)), (Pitch3, (50, 90)), (Pitch4, (0, 80)), (Pitch5, (0, 70))])
    ]


between :: (Ord a) => a -> (a, a) -> Bool
x `between` (a, b) = a <= x && x <= b

legal :: Player -> Bool
legal (Batter arch attrs) = flip all (M.toList attrs) $ \(attr, val) -> val `between` (bounds ! attr)
    where bounds = battingBounds ! arch
legal (Pitcher arch attrs) = flip all (M.toList attrs) $ \(attr, val) -> val `between` (bounds ! attr)
    where bounds = pitchingBounds ! arch

badAttrs :: Player -> Bool
badAttrs (Batter _ attrs) = M.size attrs /= 18
badAttrs (Pitcher _ attrs) = M.size attrs /= 12


scale :: Int -> Int
scale x
    | x <= 0   = 0
    | x == 1   = 11
    | x <= 40  = scale 1 + (x - 1)
    | x <= 50  = scale 40 + 2 * (x-40)
    | x <= 60  = scale 50 + 3 * (x-50)
    | x <= 70  = scale 60 + 4 * (x-60)
    | x <= 80  = scale 70 + 6 * (x-70)
    | x <= 90  = scale 80 + 7 * (x-80)
    | x <= 115 = scale 90 + 8 * (x-90)
    | otherwise = error "Update scale exceeded"

scaleDiff :: Int -> Int -> Int
scaleDiff = (-) `on` scale

count f = length . filter f

scaleVelo :: Int -> Int
scaleVelo x = 30 * count (<x) ([75, 80] ++ [83..100])

scaleDiffVelo :: Int -> Int -> Int
scaleDiffVelo = (-) `on` scaleVelo


tpeSpentOnB :: BattingArch -> (BattingAttr, Int) -> Int
tpeSpentOnB arch (attr, val) = val `scaleDiff` (fst $ battingBounds ! arch ! attr)

tpeSpentOnP :: PitchingArch -> (PitchingAttr, Int) -> Int
tpeSpentOnP arch (Velocity, val) = val `scaleDiffVelo` (fst $ pitchingBounds ! arch ! Velocity)
tpeSpentOnP arch (attr, val) = val `scaleDiff` (fst $ pitchingBounds ! arch ! attr)

tpeSpent :: Player -> Int
tpeSpent (Batter arch attrs) = sum $ tpeSpentOnB arch <$> M.toList attrs
tpeSpent (Pitcher arch attrs) = sum $ tpeSpentOnP arch <$> M.toList attrs

tooSlow :: Player
tooSlow = Batter Streak $ M.fromList [(Speed, 45)]

justRight :: Player
justRight = Batter Streak $ M.fromList [(Speed, 65)]

tooFast :: Player
tooFast = Batter Streak $ M.fromList [(Speed, 95)]
