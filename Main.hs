import System.Random
import qualified Data.Set as Set

import Dice
import BaseTraits
import Attack
import Space
import Damage
import Typeclasses

main :: IO ()
main = do
	let dex = (DPlus (DTimes 2 $ Die $ D 8) $ Const 2)
	let sward = (def :: Attack) {
		baseDmgRolls = [DmgRoll dex Slashing, DmgRoll (6 `DTimes` (Die $ D 6)) Fire],
		atkKind = Melee
	}
	let radiantBreadth = (def :: Attack) {  -- [sic]
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
		ascores = AScores (def :: Abilities) { dex = 20 },
		hitDice = 3,
		size = Tiny,
		cr = CR20,
		hitDieOverride = Just (D 8),
		savingProfs = Set.singleton Dex
	}
	let scaly = (def :: BaseCreature) {
		ascores = AScores Abilities { str = 30, dex = 14, con = 29, int = 18, wis = 17, cha = 28 },
		hitDice = 28,
		size = Gargantuan,
		acKind = Natural 22,
		cr = CR24,
		savingProfs = Set.fromList [Dex, Con, Wis, Cha],
		immunities = Set.singleton Fire
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
	putStrLn $ "expected target: " ++ (show $ expectedTargets sward)
	putStrLn $ "expected damage vs skrub: " ++ (show $ expectedDamageVsTarget sward killa skrub)
	putStrLn $ "expected damage vs getbackere: " ++ (show $ expectedDamageVsTarget sward killa getbackere)
	putStrLn $ "expected damage vs scaly: " ++ (show $ expectedDamageVsTarget sward killa scaly)
	putStrLn $ "total damage vs skrub: " ++ (show $ expectedDamage sward killa skrub)
	putStrLn $ "total damage vs getbackere: " ++ (show $ expectedDamage sward killa getbackere)
	putStrLn $ "total damage vs scaly: " ++ (show $ expectedDamage sward killa scaly)
	putStrLn "-- RadiantBreadth (scaly): "
	putStrLn $ "expected target: " ++ (show $ expectedTargets radiantBreadth)
	putStrLn $ "expected damage vs skrub: " ++ (show $ expectedDamageVsTarget radiantBreadth scaly skrub)
	putStrLn $ "expected damage vs getbackere: " ++ (show $ expectedDamageVsTarget radiantBreadth scaly getbackere)
	putStrLn $ "expected damage vs scaly: " ++ (show $ expectedDamageVsTarget radiantBreadth scaly scaly)
	putStrLn $ "total damage vs skrub: " ++ (show $ expectedDamage radiantBreadth scaly skrub)
	putStrLn $ "total damage vs getbackere: " ++ (show $ expectedDamage radiantBreadth scaly getbackere)
	putStrLn $ "total damage vs scaly: " ++ (show $ expectedDamage radiantBreadth scaly scaly)


showHp :: String -> BaseCreature -> IO ()
showHp nm bc = do
	putStrLn $ "expected HP of " ++ nm ++ ": " ++ (show $ expectedHitPoints bc)
	putStrLn $ "armorClass: " ++ (show $ armorClass bc)
	putStrLn $ "aScores: " ++ (show $ ascores bc)
	putStrLn $ "amods: " ++ (show $ mods bc)
