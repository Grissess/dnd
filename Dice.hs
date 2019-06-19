module Dice where

import System.Random

import Typeclasses

newtype Die = D Int deriving (Eq, Show)

faces :: Die -> Int
faces (D i) = i

data DiceExpr = Die Die | DTimes Int DiceExpr | DPlus DiceExpr DiceExpr | Const Int deriving (Show)

roll :: (RandomGen g) => g -> DiceExpr -> (Int, g)
roll g (Die d) = randomR (1, faces d) g
roll g (DTimes n ex) = cumulate 0 g n ex
	where
		cumulate a g 0 ex = (a, g)
		cumulate a g n ex = let (value, g') = roll g ex in cumulate (a + value) g' (n - 1) ex
roll g (DPlus xa xb) = let
	(va, g') = roll g xa
	(vb, g'') = roll g' xb
	in (va + vb, g'')
roll g (Const i) = (i, g)

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
	| i <= c = 1.0
	| otherwise = 0.0

-- The specific 5e variant
probCheckPasses :: DiceExpr -> Int -> Float
probCheckPasses d i = 1.0 - (cumProb d $ i - 1)
