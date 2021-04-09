module Parser where

import qualified Data.Map.Strict as M
import Text.Parsec hiding (Error)
import Text.Parsec.Char
import Text.Parsec.Combinator

import Archetype
import Attribute
import Player


type Parser = Parsec String Int

player :: Parser Player
player = do
    manyTill anyChar (try $ string "Position:")
    spaces
    pos <- many1 alphaNum
    case pos of
        "SP" -> pitcher
        "RP" -> pitcher
        "CL" -> pitcher
        _    -> batter

batter :: Parser Player
batter = do
    manyTill anyChar (try $ (optional (string "Hitting ") *> string "Archetype:"))
    spaces
    arch <- battingArch
    manyTill anyChar endOfLine
    attrChanges <- many attrChangeB
    let attrs = foldr ($) M.empty attrChanges
    pure $ Batter arch attrs

pitcher :: Parser Player
pitcher = do
    manyTill anyChar (try (string "Player Archetype:") <|> try (string "Pitching Archetype:"))
    spaces
    arch <- pitchingArch
    manyTill anyChar endOfLine
    attrChanges <- many attrChangeP
    let attrs = foldr ($) M.empty attrChanges
    pure $ Pitcher arch attrs

battingArch :: Parser BattingArch
battingArch = choice . map try $
    [ Utility <$ string "Mr. Utility"
    , Streak <$ string "The Streak"
    , Handyman <$ string "Handy Man"
    , Cannon <$ string "The Cannon"
    , Magician <$ string "Magician"
    , BMOC <$ (string "Big Man On Campus" <|> string "Big Woman On Campus")
    , Freak <$ string "The Freak"
    ]

pitchingArch :: Parser PitchingArch
pitchingArch = choice . map try $
    [ FlameSP <$ (string "Flamethrower" *> (try (string " SP" *> pure ()) <|> notFollowedBy (string " RP")))
    , ControlSP <$ (string "Control Freak" *> (try (string " SP" *> pure ()) <|> notFollowedBy (string " RP")))
    , JunkSP <$ (string "Junkballer" *> (try (string " SP" *> pure ()) <|> notFollowedBy (string " RP")))
    , KnuckleSP <$ string "Knuckler SP"
    , FlameRP <$ string "Flamethrower RP"
    , ControlRP <$ string "Control Freak RP"
    , JunkRP <$ string "Junkballer RP"
    ]

attrChangeB :: Parser (BattingAttrs -> BattingAttrs)
attrChangeB = try $ do
    parens
    spaces
    parens
    spaces
    attr <- attrB
    char ':'
    spaces
    val <- integer
    manyTill anyChar endOfLine
    spaces
    pure $ M.insert attr val

attrB :: Parser BattingAttr
attrB = choice . map try $
    [ BabipL <$ string "BABIP" <* vsLHP
    , BabipR <$ string "BABIP" <* vsRHP
    , AvoidKL <$ string "Avoid K" <* vsLHP
    , AvoidKR <$ string "Avoid K" <* vsRHP
    , GapL <$ string "Gap" <* vsLHP
    , GapR <$ string "Gap" <* vsRHP
    , PowerL <$ string "Power" <* vsLHP
    , PowerR <$ string "Power" <* vsRHP
    , EyeL <$ string "Eye" <* vsLHP
    , EyeR <$ string "Eye" <* vsRHP
    , Speed <$ string "Speed" <* extra
    , Steal <$ string "Steal" <* extra
    , Bunt <$ string "Bunting" <* extra
    , Range <$ string "Fielding Range" <* extra
    , Error <$ string "Fielding Error" <* extra
    , Arm <$ string "Fielding/Catching Arm" <* extra
    , DP <$ string "Turn Double Play" <* extra
    , Catcher <$ string "Catcher Ability" <* extra
    ]
    where vsLHP = manyTill (noneOf ":") (try $ vs *> lhp)
          vsRHP = manyTill (noneOf ":") (try $ vs *> rhp)
          vs = spaces *> string "vs" *> optional (char '.')
          lhp = spaces *> string "LHP"
          rhp = spaces *> string "RHP"
          extra = many $ noneOf ":"

attrChangeP :: Parser (PitchingAttrs -> PitchingAttrs)
attrChangeP = try $ do
    parens
    spaces
    parens
    spaces
    attr <- attrP
    try (notYetUnlocked attr) <|> do
        char ':'
        spaces
        val <- integer
        manyTill anyChar endOfLine
        spaces
        pure $ M.insert attr val

notYetUnlocked :: PitchingAttr -> Parser (PitchingAttrs -> PitchingAttrs)
notYetUnlocked attr = do
    char '('
    manyTill anyChar endOfLine
    spaces
    pure $ M.insert attr 0

attrP :: Parser PitchingAttr
attrP = choice . map try $
    [ MovementL <$ string "Movement" <* vsLHB
    , MovementR <$ string "Movement" <* vsRHB
    , ControlL <$ string "Control" <* vsLHB
    , ControlR <$ string "Control" <* vsRHB
    , Stamina <$ string "Stamina" <* spaces
    , Hold <$ string "Holding Runners" <* spaces
    , pitch <* spaces
    , Velocity <$ string "Velocity" <* spaces
    ]
    where vsLHB = manyTill (noneOf ":") (try $ vs *> lhb)
          vsRHB = manyTill (noneOf ":") (try $ vs *> rhb)
          vs = spaces *> string "vs" *> optional (char '.')
          lhb = spaces *> string "LHB"
          rhb = spaces *> string "RHB"
          --extra = many $ noneOf ":"

pitch :: Parser PitchingAttr
pitch = do
    choice $ map (try . string) ["Fastball", "Sinker", "Cutter", "Curveball", "Slider", "Changeup", "Splitter", "Forkball", "Circle Change", "Screwball", "Knuckle Curve", "Knuckleball", "Pitch 1", "Pitch 2", "Pitch 3", "Pitch 4", "Pitch 5"]
    modifyState succ
    n <- getState
    pure $ case n of
        1 -> Pitch1
        2 -> Pitch2
        3 -> Pitch3
        4 -> Pitch4
        5 -> Pitch5
        _ -> error "too many pitches... or too few? owo"

parens :: Parser String
parens = between (char '(') (char ')') (many $ noneOf ")")

integer :: Parser Int
integer = read <$> many1 digit
