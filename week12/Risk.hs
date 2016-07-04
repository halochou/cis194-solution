{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (replicateM)

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

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

-- Ex2

dice n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
        attackerDice <- dice (min 3 $ attackers bf - 1)
        defenderDice <- dice (min 2 $ defenders bf)
        let zipDice = zip attackerDice defenderDice
        let newAttackers = attackers bf - length (filter (uncurry (<=)) zipDice)
        let newDefenders = defenders bf - length (filter (uncurry (>)) zipDice)
        return (Battlefield newAttackers newDefenders)

-- Ex3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
            newBF <- battle bf
            if defenders newBF == 0 || attackers newBF < 2 then
              return newBF
            else
              invade newBF

-- Ex4

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
                 bfs <- replicateM 1000 $ invade bf
                 let succNum = length $ filter (\b -> defenders b == 0) bfs
                 return $ fromIntegral succNum / 1000
