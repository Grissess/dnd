module BaseTraits where

import qualified Data.Set as Set

import Dice
import Damage
import Typeclasses

data Abilities = Abilities {
	str :: Int,
	dex :: Int,
	con :: Int,
	int :: Int,
	wis :: Int,
	cha :: Int
} deriving (Eq, Show)

instance Default Abilities where
	def = Abilities { str = 10, dex = 10, con = 10, int = 10, wis = 10, cha = 10 }

data Ability = Str | Dex | Con | Int | Wis | Cha deriving (Eq, Ord, Show)

class AbilityLike a where
	abilityOf :: Ability -> a -> Int

instance AbilityLike Abilities where
	abilityOf Str = str
	abilityOf Dex = dex
	abilityOf Con = con
	abilityOf Int = int
	abilityOf Wis = wis
	abilityOf Cha = cha

newtype AScores = AScores Abilities deriving (Eq, Show)
newtype AMods = AMods Abilities deriving (Eq, Show)

instance Default AScores where
	def = AScores (def :: Abilities)

instance Default AMods where
	def = mods (def :: AScores)

instance AbilityLike AScores where
	abilityOf a (AScores ab) = abilityOf a ab

instance AbilityLike AMods where
	abilityOf a (AMods ab) = abilityOf a ab

class HasMods a where
	mods :: a -> AMods

instance HasMods AScores where
	mods (AScores ab) = AMods Abilities {
		str = f $ str ab,
		dex = f $ dex ab,
		con = f $ con ab,
		int = f $ int ab,
		wis = f $ wis ab,
		cha = f $ cha ab
	} where f n = floor $ (fromIntegral $ n - 10) / 2.0

class HasAbilities a where
	abilities :: a -> Abilities

instance HasAbilities AScores where
	abilities (AScores ab) = ab

instance HasAbilities AMods where
	abilities (AMods ab) = ab

data Size = Fine | Diminutive | Tiny | Small | Medium | Large | Huge | Gargantuan | Colossal | ColossalPlus deriving (Eq, Ord, Show)

class HasHitDie a where
	hitDie :: a -> Die

-- 5e DMG, p. 276
instance HasHitDie Size where
	hitDie sz
		| sz <= Tiny = D 4
		| sz == Small = D 6
		| sz == Medium = D 8
		| sz == Large = D 10
		| sz == Huge = D 12
		| otherwise = D 20

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

class HasProfBonus a where
	profBonus :: a -> Int

-- 5e DMG, p. 274
instance HasProfBonus CR where
	profBonus cr
		| f < 5 = 2
		| f < 9 = 3
		| f < 13 = 4
		| f < 17 = 5
		| f < 21 = 6
		| f < 25 = 7
		| f < 29 = 8
		| otherwise = 9
		where f = floor $ toFloat cr

data ACKind = Normal | UnarmoredDefense | Armor Int | ArmorDex Int | Natural Int

data BaseCreature = BaseCreature {
	ascores :: AScores,
	acKind :: ACKind,
	cr :: CR,
	size :: Size,
	hitDice :: Int,
	hitDieOverride :: Maybe Die,  -- XXX Only a kludge until this is calculated from classes...
	savingProfs :: Set.Set Ability,
	immunities :: Set.Set DmgKind,
	resistances :: Set.Set DmgKind,
	vulnerabilities :: Set.Set DmgKind

}

instance Default BaseCreature where
	def = BaseCreature {
		ascores = def,
		acKind = Normal,
		cr = CR0,  -- Just your typical commoner...
		size = Medium,
		hitDice = 1,
		hitDieOverride = Nothing,
		savingProfs = Set.empty,
		immunities = Set.empty,
		resistances = Set.empty,
		vulnerabilities = Set.empty
	}

instance HasMods BaseCreature where
	mods = mods . ascores

instance HasProfBonus BaseCreature where
	profBonus = profBonus . cr

instance HasHitDie BaseCreature where
	hitDie BaseCreature { hitDieOverride = Just d } = d
	hitDie bc = hitDie $ size bc

expectedHitPoints :: BaseCreature -> Int
expectedHitPoints bc = let
	c = con $ abilities $ mods bc
	dEx = (hitDice bc) `DTimes` ((Die $ hitDie bc) `DPlus` (Const c))
	in floor $ expected dEx

armorClass :: BaseCreature -> Int
armorClass bc @ BaseCreature { acKind = Normal } = 10 + (dex $ abilities $ mods bc)
armorClass bc @ BaseCreature { acKind = UnarmoredDefense } = let
	m = abilities $ mods bc
	in 10 + (dex m) + (con m)
armorClass BaseCreature { acKind = Armor i } = i
armorClass bc @ BaseCreature { acKind = ArmorDex i } = i + (dex $ abilities $ mods bc)
armorClass BaseCreature { acKind = Natural i } = i

dmgFactor :: BaseCreature -> DmgKind -> Float
dmgFactor bc k = let
	f = 1.0
	f' = if k `Set.member` (immunities bc) then 0.0 else f
	f'' = if k `Set.member` (resistances bc) then 0.5 * f' else f'
	f''' = if k `Set.member` (vulnerabilities bc) then 2.0 * f'' else f''
	in f'''

savingMod :: BaseCreature -> Ability -> Int
savingMod bc ab =
	(ab `abilityOf` (mods bc)) + if ab `Set.member` (savingProfs bc) then (profBonus bc) else 0
