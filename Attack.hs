module Attack where

import Dice
import BaseTraits
import Space
import Damage
import Typeclasses

class HasAtkModBonus a where
	atkModBonus :: a -> AMods -> Int

data AttackKind = Melee | Ranged | Special deriving (Eq, Show)

instance HasAtkModBonus AttackKind where
	atkModBonus Melee (AMods ab) = str ab
	atkModBonus Ranged (AMods ab) = dex ab
	atkModBonus Special _ = 0

data Target = One | Area Area deriving (Show)

data DmgRoll = DmgRoll DiceExpr DmgKind deriving (Show)

dmgDEx :: DmgRoll -> DiceExpr
dmgDEx (DmgRoll dex _) = dex

instance HasDmgKind DmgRoll where
	dmgKind (DmgRoll _ k) = k

instance ExpectedValue DmgRoll where
	expected = expected . dmgDEx

data EffectDensity =
	  Exactly Int
	| AreaDensity Float  -- e.g., 0.04 is one per typical 5-foot-square
	deriving (Show)

data Save =
	  NoSave
	| SaveReduces { onPass :: Float, granting :: Ability, saving :: Ability }
	deriving (Show)

data Uses = Indefinite | PerDay Int | Recharge Int Die deriving (Show)

data Attack = Attack {
	baseDmgRolls :: [DmgRoll],  -- The first element of this should be the kind of damage to get the ability mod bonus
	atkKind :: AttackKind,
	toHitBonus :: Int,  -- The kind of thing that "magic +1" gives you
	generalAtkBonus :: Int,  -- idem
	save :: Save,
	uses :: Uses,
	target :: Target,  -- Overrides any EffectDensity
	effectDensity :: EffectDensity,
	range :: Int,
	finesse :: Bool
} deriving (Show)

instance Default Attack where
	def = Attack {
		baseDmgRolls = [],  -- This is strictly illegal and must be later initialized
		atkKind = Melee,    -- This shouldn't be depended upon as being the default--initialize explicitly
		toHitBonus = 0,
		generalAtkBonus = 0,
		save = NoSave,
		uses = Indefinite,
		target = One,
		effectDensity = Exactly 2,  -- 5e DMG, p. 278, somewhat implicit
		range = 5,
		finesse = False
	}

instance HasAtkModBonus Attack where
	atkModBonus Attack { finesse = False, atkKind = k } am = atkModBonus k am
	atkModBonus Attack { finesse = True } am = max (atkModBonus Melee am) (atkModBonus Ranged am)

dmgRolls :: Attack -> AMods -> [DmgRoll]
dmgRolls atk @ Attack { baseDmgRolls = fdr:rdr } am = let
	bfdex = (dmgDEx fdr) `DPlus` (Const $ atkModBonus atk am)
	bfdr = DmgRoll bfdex $ dmgKind fdr
	in bfdr:rdr

expectedTargets :: Attack -> Int
expectedTargets Attack { target = One } = 1
expectedTargets Attack { effectDensity = Exactly i } = i
expectedTargets Attack { target = Area a, effectDensity = AreaDensity d } = 
	ceiling $ d * effectFloorArea a

-- Order is Attack, attacking creature, target creature
expectedDamage :: Attack -> BaseCreature -> BaseCreature -> Float
expectedDamage atk ac dc =
	(fromIntegral $ expectedTargets atk) * (expectedDamageVsTarget atk ac dc)

expectedDmgRollSumVsTarget :: BaseCreature -> [DmgRoll] -> Float
expectedDmgRollSumVsTarget _ [] = 0
expectedDmgRollSumVsTarget bc (dr:rst) =
	(expected dr) * (dmgFactor bc $ dmgKind dr) + (expectedDmgRollSumVsTarget bc rst)

-- See above
expectedDamageVsTarget :: Attack -> BaseCreature -> BaseCreature -> Float
expectedDamageVsTarget atk @ Attack { save = NoSave } ac dc =
	((fromIntegral $ generalAtkBonus atk) + (expectedDmgRollSumVsTarget dc $ dmgRolls atk $ mods ac))
expectedDamageVsTarget atk @ Attack { save = SaveReduces { onPass = op, granting = g, saving = s } } ac dc = let
	dsa = savingMod dc s
	am = mods ac
	apb = profBonus ac
	asa = g `abilityOf` am
	fullDmg = expectedDamageVsTarget atk { save = NoSave } ac dc
	reducedDmg = fromIntegral $ floor $ op * fullDmg
	-- cumProb is a bit weak at the moment, so shove every calculation possible into the DC instead of the DExpr
	-- Assume our defender is weak--having no saving proficiency here
	pPass = probCheckPasses (Die $ D 20) $ (8 + apb + asa) - dsa
	in reducedDmg * pPass + fullDmg * (1.0 - pPass)

expectedHitAC :: Attack -> AMods -> Float
expectedHitAC atk am =
	(expected $ Die $ D 20) + (fromIntegral $ toHitBonus atk) + (fromIntegral $ atkModBonus atk am)
