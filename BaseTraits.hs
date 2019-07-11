module BaseTraits where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Function
import Debug.Trace
import Control.Monad.State.Lazy

import Dice
import Damage
import Space
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

data Ability = Str | Dex | Con | Int | Wis | Cha deriving (Eq, Ord, Show, Bounded, Enum)

class AbilityLike a where
	abilityOf :: Ability -> a -> Int
	setAbilityOf :: Ability -> Int -> a -> a

instance AbilityLike Abilities where
	abilityOf Str = str
	abilityOf Dex = dex
	abilityOf Con = con
	abilityOf Int = int
	abilityOf Wis = wis
	abilityOf Cha = cha

	setAbilityOf Str i ab = ab { str = i }
	setAbilityOf Dex i ab = ab { dex = i }
	setAbilityOf Con i ab = ab { con = i }
	setAbilityOf Int i ab = ab { int = i }
	setAbilityOf Wis i ab = ab { wis = i }
	setAbilityOf Cha i ab = ab { cha = i }

newtype AScores = AScores Abilities deriving (Eq, Show)
newtype AMods = AMods Abilities deriving (Eq, Show)

instance Default AScores where
	def = AScores (def :: Abilities)

instance Default AMods where
	def = mods (def :: AScores)

instance AbilityLike AScores where
	abilityOf a (AScores ab) = abilityOf a ab
	setAbilityOf a i (AScores ab) = AScores $ setAbilityOf a i ab

instance AbilityLike AMods where
	abilityOf a (AMods ab) = abilityOf a ab
	setAbilityOf a i (AMods ab) = AMods $ setAbilityOf a i ab

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
	deriving (Eq, Ord, Show)

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

newtype AC = AC Int deriving (Eq, Ord, Show)

acValue :: AC -> Int
acValue (AC i) = i

-- 5e DMG, p. 274 
acForCR :: CR -> AC
acForCR cr
	| f < 4 = AC 13
	| f < 5 = AC 14
	| f < 8 = AC 15
	| f < 10 = AC 16
	| f < 13 = AC 17
	| f < 17 = AC 18
	| otherwise = AC 19
	where f = floor $ toFloat cr

newtype HP = HP Int deriving (Eq, Ord, Show)

hpValue :: HP -> Int
hpValue (HP i) = i

-- 5e DMG, p. 274 
crForHP :: HP -> CR
crForHP hp
	| h <= 6 = CR0
	| h <= 35 = CROneEighth
	| h <= 49 = CROneQuarter
	| h <= 70 = CROneHalf
	| h <= 85 = CR1
	| h <= 100 = CR2
	| h <= 115 = CR3
	| h <= 130 = CR4
	| h <= 145 = CR5
	| h <= 160 = CR6
	| h <= 175 = CR7
	| h <= 190 = CR8
	| h <= 205 = CR9
	| h <= 220 = CR10
	| h <= 235 = CR11
	| h <= 250 = CR12
	| h <= 265 = CR13
	| h <= 280 = CR14
	| h <= 295 = CR15
	| h <= 310 = CR16
	| h <= 325 = CR17
	| h <= 340 = CR18
	| h <= 355 = CR19
	| h <= 400 = CR20
	| h <= 445 = CR21
	| h <= 490 = CR22
	| h <= 535 = CR23
	| h <= 580 = CR24
	| h <= 625 = CR25
	| h <= 670 = CR26
	| h <= 715 = CR27
	| h <= 760 = CR28
	| h <= 805 = CR29
	| otherwise = CR30  -- Technically capping at 850, but...
	where h = hpValue hp

-- 5e DMG, p. 274 
expectedDamageCR :: Int -> CR
expectedDamageCR d
	| d <= 1 = CR0
	| d <= 3 = CROneEighth
	| d <= 5 = CROneQuarter
	| d <= 8 = CROneHalf
	| d <= 14 = CR1
	| d <= 20 = CR2
	| d <= 26 = CR3
	| d <= 32 = CR4
	| d <= 38 = CR5
	| d <= 44 = CR6
	| d <= 50 = CR7
	| d <= 56 = CR8
	| d <= 62 = CR9
	| d <= 68 = CR10
	| d <= 74 = CR11
	| d <= 80 = CR12
	| d <= 86 = CR13
	| d <= 92 = CR14
	| d <= 98 = CR15
	| d <= 104 = CR16
	| d <= 110 = CR17
	| d <= 116 = CR18
	| d <= 122 = CR19
	| d <= 140 = CR20
	| d <= 158 = CR21
	| d <= 176 = CR22
	| d <= 194 = CR23
	| d <= 212 = CR24
	| d <= 230 = CR25
	| d <= 248 = CR26
	| d <= 266 = CR27
	| d <= 284 = CR28
	| d <= 302 = CR29
	| otherwise = CR30  -- Technically capping at 320

-- 5e DMG, p. 274 
toHitBonusForCR :: CR -> Int
toHitBonusForCR cr
	| f < 3 = 3
	| f < 4 = 4
	| f < 5 = 5
	| f < 8 = 6
	| f < 11 = 7
	| f < 16 = 8
	| f < 17 = 9
	| f < 21 = 10
	| f < 24 = 11
	| f < 27 = 12
	| f < 30 = 13
	| otherwise = 14
	where f = floor $ toFloat cr

-- 5e DMG, p. 274 
dcForCR :: CR -> Int
dcForCR cr
	| f < 4 = 13
	| f < 5 = 14
	| f < 8 = 15
	| f < 11 = 16
	| f < 13 = 17
	| f < 17 = 18
	| f < 21 = 19
	| f < 24 = 20
	| f < 27 = 21
	| f < 30 = 22
	| otherwise = 23
	where f = floor $ toFloat cr

data ACKind = Normal | UnarmoredDefense | Armor Int | ArmorDex Int | Natural Int

data LegendaryAttacks = LegendaryAttacks Int (Map.Map String Int)  -- total number, map from attack to cost

legendaryQuota :: LegendaryAttacks -> Int
legendaryQuota (LegendaryAttacks q _) = q

legendaryCosts :: LegendaryAttacks -> Map.Map String Int
legendaryCosts (LegendaryAttacks _ c) = c

data Action =
	  AttackAction Attack
	deriving (Show)

instance Named Action where
	name (AttackAction a) = name a

newtype ActionMap = ActionMap (Map.Map String Action) deriving (Show)

actionMap :: ActionMap -> Map.Map String Action
actionMap (ActionMap map) = map

fromActionList :: [Action] -> ActionMap
fromActionList acts = ActionMap $ Map.fromList $ zip (map name acts) acts

data BaseCreature = BaseCreature {
	ascores :: AScores,
	acKind :: ACKind,
	actions :: ActionMap,
	attackActions :: [String],
	size :: Size,
	hitDice :: Int,
	hitDieOverride :: Maybe Die,  -- XXX Only a kludge until this is calculated from classes...
	maxAttacksPerRound :: Int,  -- XXX Eventually dependent; should depend on class feats (Extra Attack), Legendary Actions, etc.
	expectedDamageRounds :: Int,
	savingProfs :: Set.Set Ability,
	immunities :: Set.Set DmgKind,
	resistances :: Set.Set DmgKind,
	vulnerabilities :: Set.Set DmgKind,
	proficientAttacks :: Set.Set String,
	legendaryAttacks :: LegendaryAttacks
}

instance Default BaseCreature where
	def = BaseCreature {
		ascores = def,
		acKind = Normal,
		actions = fromActionList $ map AttackAction defaultAttacks,
		attackActions = map name defaultAttacks,
		size = Medium,
		hitDice = 1,
		hitDieOverride = Nothing,
		maxAttacksPerRound = 1,
		expectedDamageRounds = 3,
		savingProfs = Set.empty,
		immunities = Set.empty,
		resistances = Set.empty,
		vulnerabilities = Set.empty,
		proficientAttacks = Set.empty,
		legendaryAttacks = LegendaryAttacks 0 Map.empty
	}

instance HasMods BaseCreature where
	mods = mods . ascores

instance HasProfBonus BaseCreature where
	profBonus = profBonus . cr

instance HasHitDie BaseCreature where
	hitDie BaseCreature { hitDieOverride = Just d } = d
	hitDie bc = hitDie $ size bc

expectedHitPoints :: BaseCreature -> HP
expectedHitPoints bc = let
	c = con $ abilities $ mods bc
	dEx = (hitDice bc) `DTimes` ((Die $ hitDie bc) `DPlus` (Const c))
	in HP $ floor $ expected dEx

armorClass :: BaseCreature -> AC
armorClass bc @ BaseCreature { acKind = Normal } = AC $ 10 + (dex $ abilities $ mods bc)
armorClass bc @ BaseCreature { acKind = UnarmoredDefense } = let
	m = abilities $ mods bc
	in AC $ 10 + (dex m) + (con m)
armorClass BaseCreature { acKind = Armor i } = AC i
armorClass bc @ BaseCreature { acKind = ArmorDex i } = AC $ i + (dex $ abilities $ mods bc)
armorClass BaseCreature { acKind = Natural i } = AC i

dmgFactor :: BaseCreature -> DmgKind -> Float
dmgFactor bc k = let
	f = 1.0
	f' = if k `isIn` (immunities bc) then 0.0 else f
	f'' = if k `isIn` (resistances bc) then 0.5 * f' else f'
	f''' = if k `isIn` (vulnerabilities bc) then 2.0 * f'' else f''
	in f'''
	where isIn = Set.member

savingMod :: BaseCreature -> Ability -> Int
savingMod bc ab =
	(ab `abilityOf` (mods bc)) + if ab `Set.member` (savingProfs bc) then (profBonus bc) else 0

isProficientAttack :: BaseCreature -> Attack -> Bool
isProficientAttack bc atk = (name atk) `Set.member` (proficientAttacks bc)

multiattacks :: BaseCreature -> [Attack]
multiattacks bc = catMaybes $ map match $ Map.elems $ actionMap $ actions bc
	where
		match (AttackAction (atk @ Multiattack {})) = Just atk
		match _ = Nothing

-- XXX Referring to multiattack names in this list works, but is awkward. See
-- the instance of Named for Attack below.
attacks :: BaseCreature -> [Attack]
attacks bc = lookupAttacks bc $ attackActions bc

-- 5e DMG p. 278: calculate as the average over three rounds, but note that
-- some creatures can still designate multiple attacks per round (Multiattack)
-- and/or take multiple Attack actions (Extra Attack, Fury of Blows, ...)
-- See offensiveCR for why the defensiveCR is used to calculate a prof bonus.
bestAttackGivenHistory :: BaseCreature -> BaseCreature -> [Attack] -> (Attack -> Float) -> [Attack] -> Attack
bestAttackGivenHistory ac dc atks cost hist = let
	prb = profBonus $ defensiveCR ac
	available = filter (\atk -> usableOnRound atk hist) atks
	atksDmgs = zip available $ map (\atk -> (expectedDamageUsingProf atk ac dc prb) / (cost atk)) available
	in fst $ maximumBy (compare `on` snd) atksDmgs

nextAttackHistory :: BaseCreature -> BaseCreature -> [Attack] -> (Attack -> Float) -> [Attack] -> [Attack]
-- Common optimization
nextAttackHistory ac @ BaseCreature { maxAttacksPerRound = 1 } dc atks cost hist =
	hist ++ [bestAttackGivenHistory ac dc atks cost hist]
-- XXX This use of Multiattack is probably not recommendable; the "history"
-- should be refactored to be more expressive than just [Attack]
nextAttackHistory ac @ BaseCreature { maxAttacksPerRound = n } dc atks cost hist = let
	nextAtk = bestAttackGivenHistory ac dc atks cost hist 
	nextHist = nextAttackHistory ac { maxAttacksPerRound = (n - 1) } dc atks cost $ hist ++ [nextAtk]
	in hist ++ [mergeAtks nextAtk $ last nextHist]
	where
		mergeAtks Attack { a_nm = a } Attack { a_nm = b } = Multiattack { names = [a, b] }
		mergeAtks Attack { a_nm = a } Multiattack { names = bs } = Multiattack { names = a:bs }
		mergeAtks Multiattack { names = as } Attack { a_nm = b } = Multiattack { names = as ++ [b] }
		mergeAtks Multiattack { names = as } Multiattack { names = bs } = Multiattack { names = as ++ bs }

historyOfSize :: BaseCreature -> BaseCreature -> Int -> [Attack] -> (Attack -> Float) -> [Attack]
historyOfSize _ _ 0 _ _ = []
historyOfSize ac dc n atks cost = nextAttackHistory ac dc atks cost (historyOfSize ac dc (n - 1) atks cost)

legendaryAtks :: BaseCreature -> [(Attack,Int)]
legendaryAtks bc @ BaseCreature { legendaryAttacks = LegendaryAttacks _ costmap } =
	mapMaybe mapf $ Map.toList costmap
	where
		mapf (anm, cost) = mapinner (lookupAttack bc anm) cost
		mapinner (Just atk) cost = Just (atk, cost)
		mapinner Nothing _ = Nothing

legendaryAtkHistory :: BaseCreature -> BaseCreature -> [Attack]
-- Another common optimization
legendaryAtkHistory ac @ BaseCreature { legendaryAttacks = LegendaryAttacks q _ } dc =
	fst $ runState nextLegendary ([],q)
	where
		nextLegendary :: State ([Attack], Int) [Attack]
		nextLegendary = do
			(hist, quot) <- get
			let acts = filter (\(a,c) -> c <= quot) $ legendaryAtks ac
			if (null acts)
				then return hist
				else do
					let next = bestAttackGivenHistory ac dc (map fst acts) (\act -> fromIntegral $ fromJust $ lookup act acts) hist
					let hist' = hist ++ [next]
					let quot' = quot - (fromJust $ lookup next acts)
					put (hist', quot')
					nextLegendary

expectedDamageOutput :: BaseCreature -> BaseCreature -> Float
expectedDamageOutput ac dc = let
	prb = profBonus $ defensiveCR ac
	hist = historyOfSize ac dc (expectedDamageRounds ac) (attacks ac) (const 1.0)
	lhist = legendaryAtkHistory ac dc
	dmgs = map (\atk -> expectedDamageUsingProf atk ac dc prb) hist
	total = sum dmgs
	ldmgs = map (\atk -> expectedDamageUsingProf atk ac dc prb) lhist
	ltotal = sum ldmgs
	-- total' = trace ("edo total: " ++ (show total) ++ " parts: " ++ (show dmgs) ++ " prb: " ++ (show prb)) total
	in ltotal + total / (fromIntegral $ length hist)

-- All CR calculation from 5e DMG pp. 274-5

defensiveCR :: BaseCreature -> CR
defensiveCR bc = let
	crh = crForHP $ expectedHitPoints bc
	rac = acValue $ armorClass bc
	cac = acValue $ acForCR crh
	adj = truncate $ (fromIntegral $ rac - cac) / 2.0
	in fromFloat $ (toFloat crh) + (fromIntegral adj)

-- XXX There's a dependency cycle where calculating overall attack bonus
-- requires computing the proficiency bonus, which requires computing the CR,
-- which calls back into this. A similar effect happens with save DCs--thus all
-- the xxxUsingProf functions.
offensiveCRVsTargetUsingProf :: BaseCreature -> BaseCreature -> Int -> CR
offensiveCRVsTargetUsingProf ac dc prb = let
	crd = expectedDamageCR $ floor $ expectedDamageOutput ac dc
	atb = maximum [overallAtkBonusUsingProf atk ac prb | atk <- attacks ac]
	cab = toHitBonusForCR crd
	ast = filter hasSavingThrow $ attacks ac
	cdc = dcForCR crd
	sdc = if (null ast) then cdc else maximum [saveDCUsingProf (save atk) ac prb | atk <- ast]
	adjatk = truncate $ (fromIntegral $ atb - cab) / 2.0
	adjdc = truncate $ (fromIntegral $ sdc - cdc) / 2.0
	adj = max adjatk adjdc
	in fromFloat $ (toFloat crd) + (fromIntegral adj)

-- Attempt to find the fixed point of a CR/Prof cycle
-- (credit to @Geometer1729)
offensiveCRVsTarget :: BaseCreature -> BaseCreature -> CR
offensiveCRVsTarget ac dc = let
	crs = iterate (\(x,y) -> (y,step y)) (CR0, step CR0)
	in fst $ head $ filter (uncurry (==)) crs
	where
		step cr = offensiveCRVsTargetUsingProf ac dc $ profBonus cr

offensiveCR :: BaseCreature -> CR
offensiveCR bc = offensiveCRVsTarget bc def

cr :: BaseCreature -> CR
cr bc = let
	crf = ((toFloat $ defensiveCR bc) + (toFloat $ offensiveCR bc)) / 2.0
	in if crf >= 1.0
		then fromFloat $ fromIntegral $ floor crf
		else fromFloat crf

-- Former Attack module:

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

saveDCUsingProf :: Save -> BaseCreature -> Int -> Int
-- Intentionally left unimplemented for NoSave--don't call that
saveDCUsingProf SaveReduces { granting = gab } bc prb = 8 + prb + (gab `abilityOf` (mods bc))

saveDC :: Save -> BaseCreature -> Int
saveDC s bc = saveDCUsingProf s bc (profBonus bc)

data Uses = Indefinite | PerDay Int | Recharge Int Die deriving (Show)

data RechargeSimulation =
	  Never
	| AfterPassProbability Float
	deriving (Show)

data Attack = Attack {
		a_nm :: String,  -- Should be unique per creature--used to determine identity
		baseDmgRolls :: [DmgRoll],  -- The first element of this should be the kind of damage to get the ability mod bonus
		atkKind :: AttackKind,
		toHitBonus :: Int,  -- The kind of thing that "magic +1" gives you
		generalAtkBonus :: Int,  -- idem
		save :: Save,
		uses :: Uses,
		rechargeSimulation :: RechargeSimulation,
		target :: Target,  -- Overrides any EffectDensity
		effectDensity :: EffectDensity,
		range :: Int,
		finesse :: Bool
	}
	| Multiattack {
		names :: [String]
	} deriving (Show)

instance Named Attack where
	name Attack {a_nm = nm} = nm
	name Multiattack {names = nms} = "Multiattack of " ++ (intercalate "," nms)

instance Eq Attack where
	a == b = (name a) == (name b)

instance Default Attack where
	def = Attack {
		a_nm = "UNSET",  -- This is likely to not be unique; it should be initialized
		baseDmgRolls = [],  -- This is strictly illegal and must be later initialized
		atkKind = Melee,    -- This shouldn't be depended upon as being the default--initialize explicitly
		toHitBonus = 0,
		generalAtkBonus = 0,
		save = NoSave,
		uses = Indefinite,
		rechargeSimulation = Never,
		target = One,
		effectDensity = Exactly 2,  -- 5e DMG, p. 278, somewhat implicit
		range = 5,
		finesse = False
	}

defaultAttacks :: [Attack]
defaultAttacks = [def {
	a_nm = "Punch",
	baseDmgRolls = [DmgRoll (Const 1) Bludgeoning]
}]

instance HasAtkModBonus Attack where
	atkModBonus Attack { finesse = False, atkKind = k } am = atkModBonus k am
	atkModBonus Attack { finesse = True } am = max (atkModBonus Melee am) (atkModBonus Ranged am)

isBaseAttack :: Attack -> Bool
isBaseAttack Attack {} = True
isBaseAttack _ = False

isMultiattack :: Attack -> Bool
isMultiattack Multiattack {} = True
isMultiattack _ = False

hasSavingThrow :: Attack -> Bool
hasSavingThrow Attack { save = SaveReduces {}} = True
hasSavingThrow _ = False

lookupAttack :: BaseCreature -> String -> Maybe Attack
lookupAttack bc nm = let
	act = Map.lookup nm $ actionMap $ actions bc
	in match act
	where
		match Nothing = Nothing
		match (Just (AttackAction a)) = (Just a)
		match (Just _) = Nothing

lookupAttacks :: BaseCreature -> [String] -> [Attack]
lookupAttacks bc nms = catMaybes $ map (lookupAttack bc) nms

dmgRolls :: Attack -> BaseCreature -> [DmgRoll]
dmgRolls atk @ Attack { baseDmgRolls = fdr:rdr } ac = let
	bfdex = (dmgDEx fdr) `DPlus` (Const $ atkModBonus atk $ mods ac)
	bfdr = DmgRoll bfdex $ dmgKind fdr
	in bfdr:rdr
dmgRolls Multiattack { names = nms } ac =
	concat $ [dmgRolls at ac | at <- lookupAttacks ac nms]

expectedTargets :: Attack -> Maybe BaseCreature -> Int
expectedTargets Attack { target = One } _ = 1
expectedTargets Attack { effectDensity = Exactly i } _ = i
expectedTargets Attack { target = Area a, effectDensity = AreaDensity d } _ = 
	ceiling $ d * effectFloorArea a
-- This tends to be somewhat problematic with expectedDamage and might be removed
expectedTargets Multiattack { names = nms } (Just bc) = let
	ets = [expectedTargets at (Just bc) | at <- lookupAttacks bc nms]
	in if (null ets) then trace ("no ets entries for matk with nms:" ++ (show nms)) 1 else maximum ets
-- Hardly an advisable default
expectedTargets Multiattack {} Nothing = 1

-- Order is Attack, attacking creature, target creature
expectedDamageUsingProf :: Attack -> BaseCreature -> BaseCreature -> Int -> Float
expectedDamageUsingProf atk ac dc prb =
	(fromIntegral $ expectedTargets atk $ Just ac) * (expectedDamageVsTargetUsingProf atk ac dc prb)

expectedDamage :: Attack -> BaseCreature -> BaseCreature -> Float
expectedDamage atk ac dc = expectedDamageUsingProf atk ac dc (profBonus ac)

expectedDmgRollSumVsTarget :: BaseCreature -> [DmgRoll] -> Float
expectedDmgRollSumVsTarget _ [] = 0
expectedDmgRollSumVsTarget bc (dr:rst) =
	(expected dr) * (dmgFactor bc $ dmgKind dr) + (expectedDmgRollSumVsTarget bc rst)

-- See above
expectedDamageVsTargetUsingProf :: Attack -> BaseCreature -> BaseCreature -> Int -> Float
expectedDamageVsTargetUsingProf atk @ Attack { save = NoSave } ac dc _ =
	((fromIntegral $ generalAtkBonus atk) + (expectedDmgRollSumVsTarget dc $ dmgRolls atk ac))
expectedDamageVsTargetUsingProf atk @ Attack { save = SaveReduces { onPass = op, granting = g, saving = s } } ac dc prb = let
	dsa = savingMod dc s
	am = mods ac
	asa = g `abilityOf` am
	fullDmg = expectedDamageVsTarget atk { save = NoSave } ac dc
	reducedDmg = fromIntegral $ floor $ op * fullDmg
	-- cumProb is a bit weak at the moment, so shove every calculation possible into the DC instead of the DExpr
	pPass = probCheckPasses (Die $ D 20) $ (8 + prb + asa) - dsa
	in reducedDmg * pPass + fullDmg * (1.0 - pPass)
expectedDamageVsTargetUsingProf Multiattack { names = nms } ac dc prb =
	sum [expectedDamageVsTargetUsingProf at ac dc prb | at <- lookupAttacks ac nms]

expectedDamageVsTarget :: Attack -> BaseCreature -> BaseCreature -> Float
expectedDamageVsTarget atk ac dc = expectedDamageVsTargetUsingProf atk ac dc (profBonus ac)

overallAtkBonusUsingProf :: Attack -> BaseCreature -> Int -> Int
overallAtkBonusUsingProf atk @ Attack { toHitBonus = thb } ac prb =
	thb + (atkModBonus atk $ mods ac) + (if (isProficientAttack ac atk) then prb else 0)
overallAtkBonusUsingProf Multiattack { names = nms } ac prb =
	maximum [overallAtkBonusUsingProf atk ac prb | atk <- lookupAttacks ac nms]

overallAtkBonus :: Attack -> BaseCreature -> Int
overallAtkBonus atk ac = overallAtkBonusUsingProf atk ac (profBonus ac)

expectedHitAC :: Attack -> BaseCreature -> Float
expectedHitAC atk @ Attack {} ac =
	(expected $ Die $ D 20) + (fromIntegral $ overallAtkBonus atk ac)
expectedHitAC Multiattack { names = nms } ac =
	(sum [expectedHitAC at ac | at <- lookupAttacks ac nms]) / (fromIntegral $ length nms)

-- Can the attack be used after the previous attacks?
-- Mostly for the offensive CR calculation, where previous rounds are in lengths [0, 2].
usableOnRound :: Attack -> [Attack] -> Bool
--usableOnRound atk _ | trace ("usableOnRound: " ++ (show atk)) False = undefined
usableOnRound Attack { uses = Indefinite } _ = True
usableOnRound Attack { uses = PerDay i, a_nm = nm } atks = let
	diruses = length $ filter (==nm) $ map name $ filter isBaseAttack atks
	indiruses = length $ filter (elem nm) $ map names $ filter isMultiattack atks
	in diruses + indiruses < i
usableOnRound Attack { uses = Recharge _ _, rechargeSimulation = Never } atks = null atks
-- Recharge dice vs. threshold follows a binomial distribution.
usableOnRound Attack { uses = Recharge t d, rechargeSimulation = AfterPassProbability passp, a_nm = nm } atks = let
	useidcs = findIndices namedAttackWasUsed atks
	in if (null useidcs) then True else let
		rounds = (length atks) - (last useidcs)
		passProb = probCheckPasses (Die d) t
		binomp = 1.0 - (1 - passProb)**(fromIntegral rounds)
		in binomp >= passp
	where
		namedAttackWasUsed Attack { a_nm = n } = n == nm
		namedAttackWasUsed Multiattack { names = nms } = nm `elem` nms
usableOnRound Multiattack {} _ = True
