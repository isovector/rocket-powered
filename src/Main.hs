{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Monad.RWS.Lazy
import Data.Ord (comparing, Ordering (EQ))

data Attribute
    = None
    | Big
    | Small
    | Holy
    | Unholy
    deriving (Show, Read, Eq)

data Person = Person
    { perHP :: Int
    -- , perDmgHandler :: Effect -> Battle ()
    }

instance Show Person where
    show = show . perHP

instance Eq Person where
    -- TODO: super big hack
    a == b = perHP a == perHP b

data CombatEnv = CombatEnv
    { cenvTeams :: [[Person]]
    , cenvTarget :: Person
    , cenvTargetMay :: Maybe Person
    , cenvAttacker :: Person
    , cenvAttackerMay :: Maybe Person
    }

instance Show CombatEnv where
    show = const "combat"

data Status
    = Burning
    | Bleeding
    deriving (Show, Read, Eq)

data RawEffect
    = Damage Person Attribute Int
    | Heal Person Int
    | AddStatus Person Status
    | RemoveStatus Person Status
    deriving Show

getTarget :: RawEffect -> Maybe Person
getTarget (Damage p _ _)     = Just p
getTarget (Heal p _)         = Just p
getTarget (AddStatus p _)    = Just p
getTarget (RemoveStatus p _) = Just p

type Effect = (RawEffect, CombatEnv)

newtype Battle a = Battle
    { runBattle' :: RWS CombatEnv [Effect] () a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader CombatEnv
             , MonadWriter [Effect]
             )

runBattle :: CombatEnv -> Battle a -> (a, [Effect])
runBattle env b = evalRWS (runBattle' b) env ()

bAttacker :: Battle Person
bAttacker = asks cenvAttacker

bTarget :: Battle Person
bTarget = asks cenvTarget

bWithTeam :: (Person -> [Person] -> Bool) -> Battle [Person]
bWithTeam p = do
    me <- bAttacker
    teams <- asks cenvTeams
    return . concat $ filter (p me) teams

bAllies :: Battle [Person]
bAllies = bWithTeam elem

bEnemies :: Battle [Person]
bEnemies = bWithTeam $ \me -> not . elem me

change :: RawEffect -> Battle ()
change e = do
    env <- ask
    let effect = (e, env)
    tell [effect]
    -- case cenvTargetMay env of
    --   Just p  -> perDmgHandler p $ effect
    --   Nothing -> return ()

teamHeal :: Int -> Battle ()
teamHeal dmg = do
    allies <- bAllies
    forM_ allies $ \who -> change $ Heal who dmg

attack :: Person -> Int -> Battle ()
attack target dmg = change $ Damage target None dmg

chainAttack :: Int -> Battle ()
chainAttack dmg = do
    enemies <- bEnemies
    forM_ enemies $ flip attack dmg

-- handleDamage :: [Effect] -> [Effect]
-- handleDamage = map snd . filter fst . map handle
--     where
--         handle e =
--             let target = getTarget e
--              in runBattle env $ do
--                  return ()




battle :: Battle ()
battle = do
    chainAttack 10
    teamHeal 20

env = CombatEnv
    { cenvTeams = [ [ me ], [ bg1, bg2 ] ]
    , cenvTarget = bg1
    , cenvTargetMay = Just bg1
    , cenvAttacker = me
    , cenvAttackerMay = Just me
    }
    where me = Person { perHP = 100 }
          bg1 = Person { perHP = 201 }
          bg2 = Person { perHP = 202 }

main = do
    let results = snd $ runBattle env battle
    mapM_ (putStrLn . show) $ results

