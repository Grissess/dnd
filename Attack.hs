module Attack where

import Dice
import BaseTraits
import Space
import Damage

class HasAtkModBonus a where
	atkModBonus :: a -> AMods -> Int

data AttackKind = Melee | Ranged

instance HasAtkModBonus AttackKind where
	atkModBonus Melee (AMods ab) = str ab
	atkModBonus Ranged (AMods ab) = dex ab

data Target = One | Area Area

data DmgRoll = DmgRoll DiceExpr DmgKind

dmgDEx :: DmgRoll -> DiceExpr
dmgDEx (DmgRoll dex _) = dex

instance HasDmgKind DmgRoll where
	dmgKind (DmgRoll _ k) = k

data Attack = Attack {
	baseDmgRolls :: [DmgRoll],  -- the first element of this should be the kind of damage to get the ability mod bonus
	atkKind :: AttackKind,
	target :: Target,
	range :: Int,
	finesse :: Bool
}

instance HasAtkModBonus Attack where
	atkModBonus Attack { finesse = False, atkKind = k } am = atkModBonus k am
	atkModBonus Attack { finesse = True } am = max (atkModBonus Melee am) (atkModBonus Ranged am)

dmgRolls :: Attack -> AMods -> [DmgRoll]
dmgRolls atk @ Attack { baseDmgRolls = fdr:rdr } am = let
	bfdex = (dmgDEx fdr) `DPlus` (Const $ atkModBonus atk am)
	bfdr = DmgRoll bfdex $ dmgKind fdr
	in bfdr:rdr


