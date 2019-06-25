import System.Random
import qualified Data.Set as Set
import qualified Data.Map as Map

import Dice
import BaseTraits
import Space
import Damage
import Typeclasses

main :: IO ()
main = do
	let dex = (DPlus (DTimes 2 $ Die $ D 8) $ Const 2)
	let sward = (def :: Attack) {
		name = "sward",
		baseDmgRolls = [DmgRoll dex Slashing, DmgRoll (6 `DTimes` (Die $ D 6)) Fire],
		atkKind = Melee
	}
	let radiantBreadth = (def :: Attack) {  -- [sic]
		name = "radiantBreadth",
		baseDmgRolls = [DmgRoll (26 `DTimes` (Die $ D 6)) Fire],
		atkKind = Special,
		target = Area $ Cone 60,
		-- effectDensity = AreaDensity 0.004,  -- one in every 10 squares
		save = SaveReduces { onPass = 0.5, granting = Cha, saving = Dex }
	}
	let skrub = (def :: BaseCreature)
	let killa = (def :: BaseCreature) {
		ascores = AScores (def :: Abilities) { str = 20, con = 14, dex = 14 },
		hitDice = 10
	}
	let getbackere = (def :: BaseCreature) {
		ascores = AScores (def :: Abilities) { dex = 20, con = 20 },
		hitDice = 3,
		size = Tiny,
		hitDieOverride = Just (D 8),
		savingProfs = Set.singleton Dex
	}
	let scaly = (def :: BaseCreature) {
		ascores = AScores Abilities { str = 30, dex = 14, con = 29, int = 18, wis = 17, cha = 28 },
		hitDice = 28,
		size = Gargantuan,
		acKind = Natural 22,
		attacks = [def {
			name = "Bite",
			baseDmgRolls = [DmgRoll (2 `DTimes` (Die $ D 10)) Piercing],
			range = 15
		}, def {
			name = "Claw",
			baseDmgRolls = [DmgRoll (2 `DTimes` (Die $ D 6)) Slashing],
			range = 10
		}, def {
			name = "Tail",
			baseDmgRolls = [DmgRoll (2 `DTimes` (Die $ D 8)) Bludgeoning],
			range = 10
		}, def {
			name = "Wing",
			baseDmgRolls = [DmgRoll (2 `DTimes` (Die $ D 6)) Bludgeoning],
			range = 15,
			save = SaveReduces { onPass = 0.5, granting = Str, saving = Dex},
			target = Area $ Sphere 15
		}, Multiattack { names = ["Bite", "Claw", "Claw"] }, def {
			name = "Fire Breath",
			baseDmgRolls = [DmgRoll (13 `DTimes` (Die $ D 10)) Fire],
			atkKind = Special,
			target = Area $ Cone 90,
			save = SaveReduces { onPass = 0.5, granting = Con, saving = Dex},
			uses = Recharge 5 (D 6)
		}],
		savingProfs = Set.fromList [Dex, Con, Wis, Cha],
		immunities = Set.singleton Fire,
		proficientAttacks = Set.fromList ["Bite", "Claw", "Tail"],
		legendaryAttacks = LegendaryAttacks 3 $ Map.fromList [("Tail", 1), ("Wing", 2)]
	}
	putStrLn $ show dex
	putStrLn $ describe dex
	putStrLn $ show $ expected dex
	gen <- getStdGen
	let (value, gen') = roll gen dex
	putStrLn $ show value
	showHp "skrub" skrub
	showHp "killa" killa
	showHp "getbackere" getbackere
	showHp "scaly" scaly
	putStrLn "-- Sward (killa): "
	putStrLn $ "expected target: " ++ (show $ expectedTargets sward $ Just killa)
	putStrLn $ "expected damage vs skrub: " ++ (show $ expectedDamageVsTarget sward killa skrub)
	putStrLn $ "expected damage vs getbackere: " ++ (show $ expectedDamageVsTarget sward killa getbackere)
	putStrLn $ "expected damage vs scaly: " ++ (show $ expectedDamageVsTarget sward killa scaly)
	putStrLn $ "total damage vs skrub: " ++ (show $ expectedDamage sward killa skrub)
	putStrLn $ "total damage vs getbackere: " ++ (show $ expectedDamage sward killa getbackere)
	putStrLn $ "total damage vs scaly: " ++ (show $ expectedDamage sward killa scaly)
	putStrLn "-- RadiantBreadth (scaly): "
	putStrLn $ "expected target: " ++ (show $ expectedTargets radiantBreadth $ Just scaly)
	putStrLn $ "expected damage vs skrub: " ++ (show $ expectedDamageVsTarget radiantBreadth scaly skrub)
	putStrLn $ "expected damage vs getbackere: " ++ (show $ expectedDamageVsTarget radiantBreadth scaly getbackere)
	putStrLn $ "expected damage vs scaly: " ++ (show $ expectedDamageVsTarget radiantBreadth scaly scaly)
	putStrLn $ "total damage vs skrub: " ++ (show $ expectedDamage radiantBreadth scaly skrub)
	putStrLn $ "total damage vs getbackere: " ++ (show $ expectedDamage radiantBreadth scaly getbackere)
	putStrLn $ "total damage vs scaly: " ++ (show $ expectedDamage radiantBreadth scaly scaly)


showHp :: String -> BaseCreature -> IO ()
showHp nm bc = do
	putStrLn $ "-----" ++ nm ++ "-----"
	putStrLn $ "expected HP of: " ++ (show $ expectedHitPoints bc)
	putStrLn $ "armorClass: " ++ (show $ armorClass bc)
	putStrLn $ "aScores: " ++ (show $ ascores bc)
	putStrLn $ "amods: " ++ (show $ mods bc)
	putStrLn $ "defensiveCR: " ++ (show $ defensiveCR bc)
	putStrLn $ "offensiveCR: " ++ (show $ offensiveCR bc)
	putStrLn $ "CR: " ++ (show $ cr bc)
	putStrLn $ "BON: " ++ (show $ profBonus bc)
	putStrLn $ "Expected SAV DC for this CR:" ++ (show $ dcForCR $ cr bc)
	putStrLn $ "Expected attack bonus for this CR: " ++ (show $ toHitBonusForCR $ cr bc)
	putStrLn $ "Expected damage output: " ++ (show $ expectedDamageOutput bc def)
	putStrLn $ "Damage history over 3 rounds:"
	let hist = historyOfSize bc def 3 (attacks bc) (const 1.0)
	sequence_ [putStrLn $ "  " ++ (atid atk) ++ ": " ++ (show $ expectedDamage atk bc def) ++ " dmg" | atk <- hist]
	putStrLn $ "Legendary attack history (per round):"
	let lhist = legendaryAtkHistory bc def
	sequence_ [putStrLn $ "  " ++ (atid atk) ++ ": " ++ (show $ expectedDamage atk bc def) ++ " dmg" | atk <- lhist]
	let acts = [showAttack nm bc atk | atk <- attacks bc]
	sequence_ acts
	where
		atid Attack { name = nm } = nm
		atid Multiattack { names = nms } = "Multiattack of " ++ (show nms)

showAttack :: String -> BaseCreature -> Attack -> IO ()
showAttack nm bc atk @ Attack {} = do
	putStrLn $ nm ++ "'s " ++ (name atk) ++ " attack:"
	putStrLn $ "  Overall to hit bonus: " ++ (show $ overallAtkBonus atk bc)
	if (hasSavingThrow atk)
		then putStrLn $ "  Save DC:" ++ (show $ saveDC (save atk) bc)
		else pure ()
	showAttackCommon nm bc atk
showAttack nm bc atk @ Multiattack { names = nms } = do
	putStrLn $ nm ++ "'s multiattack with attacks " ++ (show nms) ++ ":"
	showAttackCommon nm bc atk

showAttackCommon :: String -> BaseCreature -> Attack -> IO ()
showAttackCommon nm bc atk = do
	putStrLn $ "  Expected targets: " ++ (show $ expectedTargets atk $ Just bc)
	putStrLn $ "  Expected damage vs 1 target: " ++ (show $ expectedDamageVsTarget atk bc def)
	putStrLn $ "  Expected overall damage: " ++ (show $ expectedDamage atk bc def)
	putStrLn $ "  Expected hit AC: " ++ (show $ expectedHitAC atk bc)
	putStrLn ""
