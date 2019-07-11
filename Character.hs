module Character where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Dice
import BaseTraits
import Typeclasses

data Character = Character {
	c_nm :: String,
	modifiers :: [Modifier]
}

data CharState = CharState {
	bc :: BaseCreature,
	hitDiceRoll :: DiceRoll,
	warnings :: [String]
}

hitPoints :: CharState -> HP
hitPoints cs = HP $ rollValue $ hitDiceRoll cs

data ASI = ASI Ability Ability

addAbility :: Ability -> Int -> AScores -> AScores
addAbility a i as = setAbilityOf a (i + (a `abilityOf` as)) as

applyASI :: ASI -> AScores -> AScores
applyASI (ASI ab1 ab2) as =
	addAbility ab2 1 $ addAbility ab1 1 as

capAScore :: Ability -> AScores -> (AScores, Bool)
capAScore a as =
	if (a `abilityOf` as) > 20
	then (setAbilityOf a 20 as, True)
	else (as, False)

capAScores :: AScores -> (AScores, Bool)
capAScores as = foldr merge (as, False) [Str .. Cha]
	where
		merge a (as, t) = let
			(as', b) = capAScore a as
			in (as', b || t)

data Modifier =
	-- First few are base modifiers that everything else lowers to
	  SetAbilityScoresNoCap AScores
	| SetSize Size
	| SetHitDice Int
	| AddHitDice Int
	| SetHitDieOverride (Maybe Die)
	| SetAC ACKind
	| SetHitDiceRoll DiceRoll
	| AddHitDiceRoll DiceRoll
	| AddAction { aa_act :: Action, aa_attack :: Bool, aa_proficient :: Bool, aa_legendaryCost :: Maybe Int }
	| SetLegendaryQuota Int
	| AddWarning String
	-- These are non-base, but are still not usually directly used
	| SetAbilityScores AScores
	| ApplyASINoCap ASI
	| ApplyASI ASI

applyMods :: CharState -> [Modifier] -> CharState
applyMods = foldr $ flip applyMod

isBase :: Modifier -> Bool
isBase (SetAbilityScoresNoCap _) = True
isBase (SetSize _) = True
isBase (SetHitDice _) = True
isBase (AddHitDice _) = True
isBase (SetHitDieOverride _) = True
isBase (SetAC _) = True
isBase (SetHitDiceRoll _) = True
isBase (AddHitDiceRoll _) = True
isBase (AddAction {}) = True
isBase (AddWarning _) = True
isBase _ = False

applyMod :: CharState -> Modifier -> CharState
applyMod cs (SetAbilityScoresNoCap as) =
	cs { bc = (bc cs) { ascores = as } }
applyMod cs (SetSize sz) =
	cs { bc = (bc cs) { size = sz } }
applyMod cs (SetHitDice hd) =
	cs { bc = (bc cs) { hitDice = hd } }
applyMod cs (AddHitDice hd) =
	cs { bc = (bc cs) { hitDice = hd + (hitDice $ bc cs) } }
applyMod cs (SetHitDieOverride hdo) =
	cs { bc = (bc cs) { hitDieOverride = hdo } }
applyMod cs (SetAC ack) =
	cs { bc = (bc cs) { acKind = ack } }
applyMod cs (SetHitDiceRoll dr) =
	cs { hitDiceRoll = dr }
applyMod cs (AddHitDiceRoll dr) = let
	odr = hitDiceRoll cs
	in cs { hitDiceRoll = RDPlus (rollExpr odr) (rollExpr dr) odr dr }
applyMod cs (aa @ AddAction {}) = let
	act = aa_act aa
	nm = name act
	crtr = bc cs
	crtr' = crtr { actions = ActionMap $ Map.insert nm act $ actionMap $ actions crtr }
	crtr'' = if (aa_attack aa) then crtr' { attackActions = nm : (attackActions crtr') } else crtr'
	crtr''' = if (aa_proficient aa) then crtr'' { proficientAttacks = Set.insert nm $ proficientAttacks crtr'' } else crtr''
	in cs { bc = applyLegendaryCost (aa_legendaryCost aa) nm crtr''' }
	where
		applyLegendaryCost (Just i) nm bc =
			bc { legendaryAttacks = LegendaryAttacks (legendaryQuota $ legendaryAttacks bc) $ Map.insert nm i $ legendaryCosts $ legendaryAttacks bc }
		applyLegendaryCost Nothing _ bc = bc
applyMod cs (SetLegendaryQuota q) =
	cs { bc = (bc cs) { legendaryAttacks = LegendaryAttacks q $ legendaryCosts $ legendaryAttacks $ bc cs } }
applyMod cs (AddWarning warn) =
	cs { warnings = (warnings cs) ++ [warn] }

applyMod cs (SetAbilityScores as) = let
	(as', capped) = capAScores as
	in if capped
		then applyMods cs [
			SetAbilityScoresNoCap as',
			AddWarning $ "capped scores in SetAbilityScores: " ++ (show as)
		]
		else applyMod cs $ SetAbilityScoresNoCap as'
applyMod cs (ApplyASINoCap asi) = let
	as' = applyASI asi $ ascores $ bc cs
	in applyMod cs $ SetAbilityScoresNoCap as'
applyMod cs (ApplyASI asi) = let
	as' = applyASI asi $ ascores $ bc cs
	in applyMod cs $ SetAbilityScores as'
