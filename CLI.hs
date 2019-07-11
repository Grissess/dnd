module CLI where

import Data.List
import Data.Char

import Text.ParserCombinators.ReadP

import Dice
import BaseTraits
import Space
import Damage
import Typeclasses

readBaseCreature :: IO BaseCreature
readBaseCreature = do
    putStrLn "========================="
    putStrLn "== MONSTER CREATOR CLI =="
    putStrLn "========================="
    putStrLn "What is the name of the monster?"
    name <- getLine

    undefined

getYesNo :: String -> IO Bool
getYesNo prompt = do
    putStrLn prompt
    inp <- (map toLower) <$> getLine
    if "yes" `isInfixOf` inp
        then return True
        else if "no" `isInfixOf` inp
            then return False
            else do
                putStrLn "Please answer yes or no"
                getYesNo prompt

getStats :: IO AScores
getStats = do
    putStrLn "--- Ability Scores ---"
    putStr "STR: "
    str <- getLine
    putStr "DEX: "
    dex <- getLine
    putStr "CON: "
    con <- getLine
    putStr "INT: "
    int <- getLine
    putStr "WIS: "
    wis <- getLine
    putStr "CHA: "
    cha <- getLine
    putStrLn $ "STR: " ++ str ++ "| DEX: " ++ dex ++ " | CON: " ++ con ++ " | INT: " ++ int ++ " | WIS: " ++ wis ++ " | CHA: " ++ cha
    resp <- getYesNo "Is this correct?"
    if resp
        then return $ AScores Abilities { str= read str, dex= read dex, con= read con, int= read int, wis= read wis, cha= read wis }
        else getStats

--data Size = Fine | Diminutive | Tiny | Small | Medium | Large | Huge | Gargantuan | Colossal | ColossalPlus deriving (Eq, Ord, Show)
getSize :: IO Size
getSize = do
    putStrLn "--- Size ---"
    putStrLn "What size is this creature?"
    sz <- getLine
    case (toLower <$> sz) of
        "fine" -> return Fine      
        "diminutive" -> return Diminutive
        "tiny" -> return Tiny
        "small" -> return Small
        "medium" -> return Medium
        "large" -> return Large
        "huge" -> return Huge
        "gargantuan" -> return Gargantuan
        "colossal" -> return Colossal
        "colossalplus" -> return ColossalPlus
        _ -> do
            putStrLn "Please enter a valid size"
            getSize

getAC :: IO ACKind
getAC = do
    putStrLn "--- AC ---"
    putStrLn "What kind of AC does this creature use, and what is the value? (Ex. \"Natural 22\")"

    undefined
            

--newtype Die = D Int
--data DiceExpr = Die Die | DTimes Int DiceExpr | DPlus DiceExpr DiceExpr | Const Int
--read strings of them form "n1dk1+n2dk2+...+c"
readDiceExpr :: String -> DiceExpr
readDiceExpr s = let parses = (readP_to_S parseDieSum) [c | c <- s, c /= ' ']
                     finished = filter ((=="") . snd) parses
                 in if length finished == 1 then clean $ fst . head $ finished else error $ "cannot parse " ++ s

clean :: DiceExpr -> DiceExpr
clean (DPlus d (Const 0)) = d
clean (DPlus d1 d2) = DPlus (clean d1) (clean d2)
clean (DTimes n d) = DTimes n (clean d)
clean d = d

parseInt :: ReadP Int
parseInt = do
    digits <- many1 $ satisfy isDigit 
    return $ read digits

parseDie :: ReadP DiceExpr
parseDie = do
    n <- parseInt
    char 'd'
    k <- parseInt
    return $ DTimes n (Die $ D k)

parseDieConst :: ReadP DiceExpr
parseDieConst = Const <$> parseInt

parseDieSum :: ReadP DiceExpr
parseDieSum = do
    de1 <- parseDie +++ parseDieConst
    de2 <- parseDieSum'
    return $ DPlus de1 de2

parseDieSum' :: ReadP DiceExpr
parseDieSum' = (do
    char '+'
    de1 <- parseDie +++ parseDieConst
    de2 <- parseDieSum'
    return $ DPlus de1 de2) <++ (return $ Const 0)

