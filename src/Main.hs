{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

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
    , perDmgHandler :: Effect -> Battle ()
    }

instance Show Person where
    show = show . perHP

instance Eq Person where
    -- TODO: super big hack
    a == b = perHP a == perHP b

data CombatEnv = CombatEnv
    { cenvTeams :: [[Person]]
    , cenvTarget :: Maybe Person
    , cenvAttacker :: Maybe Person
    , cenvActor :: Maybe Person
    , cenvDepth :: Int
    }

instance Show CombatEnv where
    show = const "combat"

data Status
    = Burning
    | Bleeding
    deriving (Show, Read, Eq)

data Effect
    = Damage Person Attribute Int
    | Heal Person Int
    | AddStatus Person Status
    | RemoveStatus Person Status
    deriving Show

getTarget :: Effect -> Maybe Person
getTarget (Damage p _ _)     = Just p
getTarget (Heal p _)         = Just p
getTarget (AddStatus p _)    = Just p
getTarget (RemoveStatus p _) = Just p

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

bActor :: Battle (Maybe Person)
bActor = asks cenvActor

bAttacker :: Battle (Maybe Person)
bAttacker = asks cenvAttacker

bTarget :: Battle (Maybe Person)
bTarget = asks cenvTarget

bWithTeam :: Person -> (Person -> [Person] -> Bool) -> Battle [Person]
bWithTeam me p = do
    teams <- asks cenvTeams
    return . concat $ filter (p me) teams

bAllies :: Battle [Person]
bAllies = bActor >>= \case
            Just a  -> bWithTeam a elem
            Nothing -> return []

bEnemies :: Battle [Person]
bEnemies = bActor >>= \case
             Just a  -> bWithTeam a $ \me -> not . elem me
             Nothing -> asks $ concat . cenvTeams


suggest :: Effect -> Battle ()
suggest e
    | Just p <- getTarget e =
        flip local (perDmgHandler p e) $
            \r -> r { cenvActor = Just p
                    , cenvDepth = 1 + cenvDepth r
                    }
    | otherwise = return ()

accept :: Effect -> Battle ()
accept = tell . return

teamHeal :: Int -> Battle ()
teamHeal dmg = do
    allies <- bAllies
    forM_ allies $ \who -> suggest $ Heal who dmg

attack :: Person -> Int -> Battle ()
attack target dmg = suggest $ Damage target None dmg

chainAttack :: Int -> Battle ()
chainAttack dmg = do
    enemies <- bEnemies
    forM_ enemies $ flip attack dmg

battle :: Battle ()
battle = do
    chainAttack 10
    teamHeal 20

thorns :: Effect -> Battle ()
thorns e@(Damage p _ dmg) = do
    accept e
    bAttacker >>= \case
        Just aggro -> suggest . Damage aggro None $ dmg `div` 10
        Nothing    -> return ()


env = CombatEnv
    { cenvTeams = [ [ me ], [ bg1, bg2 ] ]
    , cenvTarget = Just bg1
    , cenvAttacker = Just me
    , cenvActor = Just me
    , cenvDepth = 0
    }
    where me = Person { perHP = 100, perDmgHandler = accept }
          bg1 = Person { perHP = 201, perDmgHandler = thorns }
          bg2 = Person { perHP = 202, perDmgHandler = accept }

main = do
    let results = snd $ runBattle env battle
    mapM_ (putStrLn . show) $ results

