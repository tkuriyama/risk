
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Random
import Control.Applicative
import Data.List (sort, group)
import qualified Data.Map as M (fromList, Map, lookup)
import Data.Ratio

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int
type Losses = (Army, Army)
  
data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show, Eq, Ord)

applyPair :: (a -> b) -> (a, a) -> (b, b)
applyPair f (a, b) = (f a, f b)

-- Ex 1, 2

updateField :: Battlefield -> Losses -> Battlefield
updateField (Battlefield as ds) (asLoss, dsLoss) =
  Battlefield (as - asLoss) (ds - dsLoss)

losses :: ([DieValue], [DieValue]) -> Losses
losses pair = 
  let (aDice', dDice') = applyPair (reverse . sort) pair
      ls = zipWith (\a d -> if d >= a then 1 else 0) aDice' dDice'
  in (sum ls, length ls - sum ls)
                
maxTroops :: Battlefield -> (Army, Army)
maxTroops (Battlefield as ds) = (if as >= 4 then 3 else as - 1,
                                 if ds >= 2 then 2 else ds)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  let (as', ds') = maxTroops b
  rolls <- dice (as' + ds')
  return $ updateField b $ losses . splitAt as' $ rolls

-- Ex 3

invade :: Battlefield -> Rand StdGen Battlefield
invade b
  | attackers b < 2 || defenders b <= 0 = return b
  | otherwise = battle b >>= invade

-- Ex 4 -- estimated probability that Attacker wins

trials = 5000
length' = fromIntegral . length

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  bs <- replicateM trials (invade b)
  let wins = filter ((==) 0 . defenders) bs
  return $ fromRational$ (length' wins) / (length' bs)

-- Ex 5 -- exact probability that Attacker wins

type Probability = Rational
type Scenario = (Probability, Losses)
die' = map DV [1..6] 

throw :: Int -> [[DieValue]]
throw 1 = [[d] | d <- die']
throw 2 = [[d1, d2] | d1 <- die', d2 <- die']
throw 3 = [[d1, d2, d3] | d1 <- die', d2 <- die', d3 <- die']
throw _ = []
     
pLosses :: [[DieValue]] -> [[DieValue]] -> [Scenario]
pLosses ass dss =
  let pairs = liftA2 (curry losses) ass dss
      groups = group $ sort pairs
      total = length' pairs
      genPair g = ((length' g) / total, head g)
  in map genPair groups

pMap :: M.Map (Army, Army) [Scenario]
pMap = M.fromList $ liftA2 f [1..3] [1..2]
  where f a b = ((a, b), pLosses (throw a) (throw b))

aLoses :: Battlefield -> Scenario -> Bool
aLoses (Battlefield as ds) (_, (asLoss, dsLoss))
  | ds - dsLoss <= 0 = False
  | as - asLoss <= 1 = True
  | otherwise = False

agg :: [(Probability,  Battlefield)] -> Probability
agg pairs = foldr f 0 pairs
  where f (p, b) accP = accP + (p * successExact b)

successExact :: Battlefield -> Probability
successExact (Battlefield _ 0) = 1
successExact (Battlefield 1 _) = 0
successExact b =
  case M.lookup (maxTroops b) pMap of
    Nothing -> 0
    Just ps -> agg $ map update $ filter (not . aLoses b) ps
  where update (p, losses) = (p, updateField b losses)

-- COmparison Output

round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2

width :: Int -> String -> String
width n s | length s >= n = s
          | otherwise = s ++ (replicate (n - length s) ' ')

showP :: Double -> String
showP n = if n' < 0.1 then "< 0.1%" else (show $ round2dp n') ++ "%"
  where n' = n * 100
  
compareSuccess :: Battlefield -> IO String
compareSuccess b@(Battlefield as ds) = do
  p <- evalRandIO (successProb b)
  let p2 = successExact b
      p2d = fromRational p2
      diff = p - p2d
  return $ "A: " ++ show as ++
           " B: " ++ show ds ++
           width 15 (" | Sim: " ++ showP p) ++ 
           width 17 (" | Exact: " ++ showP p2d) ++ 
           width 16 (" | Diff: " ++ showP diff) ++
           " | Exact: " ++ show p2

genComps :: IO ()
genComps = do 
  s <- sequence $ map compareSuccess $ liftA2 Battlefield [1..10] [1..10]
  putStrLn $ unlines ("Win probabiloity for A" : s)

main = genComps
