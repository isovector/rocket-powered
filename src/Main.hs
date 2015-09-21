{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad.RWS.Lazy

data Attributes
    = Big
    | Small
    deriving (Show, Read, Eq)

data Person = Person
    { perHP :: Int
    -- , perAttribs :: Attributes
    } deriving (Show, Read, Eq)

data CombatEnv = CombatEnv
    { cenvTeams :: [[Person]]
    , cenvTarget :: Person
    , cenvTargetMay :: Maybe Person
    , cenvAttacker :: Person
    , cenvAttackerMay :: Maybe Person
    }

data Status
    = Burning
    | Bleeding
    deriving (Show, Read, Eq)

data Effect
    = Damage Person Int Int
    | Heal Person Int Int
    | AddStatus Person Status
    | RemoveStatus Person Status
    deriving (Show, Read, Eq)

newtype Battle a = Battle
    { runBattle' :: RWS CombatEnv [Effect] () a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader CombatEnv
             , MonadWriter [Effect]
             )

runBattle :: Battle a -> CombatEnv -> (a, [Effect])
runBattle b env = evalRWS (runBattle' b) env ()

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

teamHeal :: Int -> Battle ()
teamHeal dmg = do
    allies <- bAllies
    forM_ allies $ \who -> tell [Heal who dmg dmg]

attack :: Person -> Int -> Battle ()
attack target dmg = tell [Damage target dmg dmg]

chainAttack :: Int -> Battle ()
chainAttack dmg = do
    enemies <- bEnemies
    forM_ enemies $ flip attack dmg

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

main = mapM_ (putStrLn . show) . snd $ runBattle battle env

