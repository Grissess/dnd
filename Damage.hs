module Damage where

class HasDmgKind a where
	dmgKind :: a -> DmgKind

-- 5e PHB, p. 196
data DmgKind =
	  Acid
	| Bludgeoning
	| Cold
	| Fire
	| Force
	| Lightning
	| Necrotic
	| Piercing
	| Poison
	| Psychic
	| Radiant
	| Slashing
	| Thunder

isPhysical :: DmgKind -> Bool
isPhysical Bludgeoning = True
isPhysical Piercing = True
isPhysical Slashing = True
isPhysical _ = False

data Damage = Damage Int DmgKind

dmgAmount :: Damage -> Int
dmgAmount (Damage i _) = i

instance HasDmgKind Damage where
	dmgKind (Damage _ k) = k
