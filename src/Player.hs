module Player where

import qualified Data.Map.Strict as M

import Archetype
import Attribute


data Player
    = Batter BattingArch BattingAttrs
    | Pitcher PitchingArch PitchingAttrs
    deriving (Show)

type Attrs a = M.Map a Int

type BattingAttrs = Attrs BattingAttr
type PitchingAttrs = Attrs PitchingAttr
