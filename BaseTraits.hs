module BaseTraits where

data Abilities = Abilities {
	str :: Int,
	dex :: Int,
	con :: Int,
	int :: Int,
	wis :: Int,
	cha :: Int
} deriving (Eq, Show)

newtype AScores = AScores Abilities deriving (Eq, Show)
newtype AMods = AMods Abilities deriving (Eq, Show)

mods :: AScores -> AMods
mods (AScores ab) = AMods Abilities {
	str = f $ str ab,
	dex = f $ dex ab,
	con = f $ con ab,
	int = f $ int ab,
	wis = f $ wis ab,
	cha = f $ cha ab
} where f n = floor $ (fromIntegral $ n - 10) / 2.0

data Size = Fine | Diminutive | Tiny | Small | Medium | Large | Huge | Gargantuan | Colossal | ColossalPlus deriving (Eq, Ord, Show)

newtype Level = Level Int deriving (Eq, Show)

level :: Level -> Int
level (Level l) = l

data CR =
	  CR0
	| CROneEighth
	| CROneQuarter
	| CROneHalf
	| CR1
	| CR2
	| CR3
	| CR4
	| CR5
	| CR6
	| CR7
	| CR8
	| CR9
	| CR10
	| CR11
	| CR12
	| CR13
	| CR14
	| CR15
	| CR16
	| CR17
	| CR18
	| CR19
	| CR20
	| CR21
	| CR22
	| CR23
	| CR24
	| CR25
	| CR26
	| CR27
	| CR28
	| CR29
	| CR30

toFloat :: CR -> Float
toFloat CR0 = 0.0
toFloat CROneEighth = 0.125
toFloat CROneQuarter = 0.25
toFloat CROneHalf = 0.5
toFloat CR1 = 1.0
toFloat CR2 = 2.0
toFloat CR3 = 3.0
toFloat CR4 = 4.0
toFloat CR5 = 5.0
toFloat CR6 = 6.0
toFloat CR7 = 7.0
toFloat CR8 = 8.0
toFloat CR9 = 9.0
toFloat CR10 = 10.0
toFloat CR11 = 11.0
toFloat CR12 = 12.0
toFloat CR13 = 13.0
toFloat CR14 = 14.0
toFloat CR15 = 15.0
toFloat CR16 = 16.0
toFloat CR17 = 17.0
toFloat CR18 = 18.0
toFloat CR19 = 19.0
toFloat CR20 = 20.0
toFloat CR21 = 21.0
toFloat CR22 = 22.0
toFloat CR23 = 23.0
toFloat CR24 = 24.0
toFloat CR25 = 25.0
toFloat CR26 = 26.0
toFloat CR27 = 27.0
toFloat CR28 = 28.0
toFloat CR29 = 29.0
toFloat CR30 = 30.0

fromFloat :: Float -> CR
fromFloat f
	| f <= 0.0 = CR0
	| f <= 0.125 = CROneEighth
	| f <= 0.25 = CROneQuarter
	| f <= 0.5 = CROneHalf
	| f <= 1.0 = CR1
	| f <= 2.0 = CR2
	| f <= 3.0 = CR3
	| f <= 4.0 = CR4
	| f <= 5.0 = CR5
	| f <= 6.0 = CR6
	| f <= 7.0 = CR7
	| f <= 8.0 = CR8
	| f <= 9.0 = CR9
	| f <= 10.0 = CR10
	| f <= 11.0 = CR11
	| f <= 12.0 = CR12
	| f <= 13.0 = CR13
	| f <= 14.0 = CR14
	| f <= 15.0 = CR15
	| f <= 16.0 = CR16
	| f <= 17.0 = CR17
	| f <= 18.0 = CR18
	| f <= 19.0 = CR19
	| f <= 20.0 = CR20
	| f <= 21.0 = CR21
	| f <= 22.0 = CR22
	| f <= 23.0 = CR23
	| f <= 24.0 = CR24
	| f <= 25.0 = CR25
	| f <= 26.0 = CR26
	| f <= 27.0 = CR27
	| f <= 28.0 = CR28
	| f <= 29.0 = CR29
	| otherwise = CR30
