import System.Random

import Dice
import BaseTraits
import Attack
import Space
import Damage

main :: IO ()
main = do
	let dex = (DPlus (DTimes 2 $ Die $ D 8) $ Const 2)
	putStrLn $ show dex
	putStrLn $ describe dex
	putStrLn $ show $ expected dex
	gen <- getStdGen
	let (value, gen') = roll gen dex
	putStrLn $ show value
