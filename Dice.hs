module Dice where

import System.Random

import Typeclasses

newtype Die = D Int deriving (Eq, Show)

faces :: Die -> Int
faces (D i) = i

data DiceExpr = Die Die | DTimes Int DiceExpr | DPlus DiceExpr DiceExpr | Const Int deriving (Show)

data DiceRoll = RDie Die Int | RDTimes Int DiceExpr [DiceRoll] | RDPlus DiceExpr DiceExpr DiceRoll DiceRoll | RConst Int deriving (Show)

roll :: (RandomGen g) => g -> DiceExpr -> (DiceRoll, g)
roll g (Die d) = (RDie d v, g')
	where (v, g') = randomR (1, faces d) g
roll g (DTimes n ex) = (RDTimes n ex v, g')
	where
		(v, g') = cumulate g n ex
		cumulate g 0 ex = ([], g)
		cumulate g n ex = let
			(value, g') = roll g ex
			(rest, gf) = cumulate g' (n - 1) ex
			in (value : rest, gf)
roll g (DPlus xa xb) = let
	(va, g') = roll g xa
	(vb, g'') = roll g' xb
	in (RDPlus xa xb va vb, g'')
roll g (Const i) = (RConst i, g)

rollValue :: DiceRoll -> Int
rollValue (RDie _ i) = i
rollValue (RDTimes _ _ dr) = sum $ map rollValue dr
rollValue (RDPlus _ _ va vb) = (rollValue va) + (rollValue vb)
rollValue (RConst i) = i

rollExpr :: DiceRoll -> DiceExpr
rollExpr (RDie d _) = Die d
rollExpr (RDTimes i x _) = DTimes i x
rollExpr (RDPlus xa xb _ _) = DPlus xa xb
rollExpr (RConst i) = Const i

describe :: DiceExpr -> String
describe (Die d) = 'd':(show $ faces d)
describe (DTimes n ex) = (show n) ++ "(" ++ (describe ex) ++ ")"
describe (DPlus xa xb) = (describe xa) ++ " + " ++ (describe xb)
describe (Const i) = (show i)

instance ExpectedValue DiceExpr where
	expected (Die d) = (1.0 + (fromIntegral $ faces d)) / 2.0
	expected (DTimes n ex) = (fromIntegral n) * (expected ex)
	expected (DPlus xa xb) = (expected xa) + (expected xb)
	expected (Const i) = fromIntegral i

-- Note that this is the statiscal notion of X <= i--that the die rolls _at or under_ the given value.
-- Since most 5e rolls are "at or over", you'll want to invert this--see below.
-- FIXME: populate the rest of the patterns of this function
cumProb :: DiceExpr -> Int -> Float
-- A single die is a uniform distribution
cumProb (Die (D d)) i
	| i <= 0 = 0.0
	| i > d = 1.0
	| otherwise = (fromIntegral $ i) / (fromIntegral d)
-- A constant is a discretized Dirac delta distribution
cumProb (Const c) i
	| i < c = 0.0
	| otherwise = 1.0

-- The specific 5e variant
probCheckPasses :: DiceExpr -> Int -> Float
probCheckPasses d i = 1.0 - (cumProb d $ i - 1)
