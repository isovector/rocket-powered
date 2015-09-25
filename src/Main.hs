{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

module Main where

import Control.Applicative
import Control.Monad.RWS.Lazy
import Data.Ord (comparing, Ordering (EQ))
import System.Random


data Attribute
    = None
    | Big
    | Small
    | Holy
    | Unholy
    | Magic
    deriving (Show, Read, Eq)

data Person = Person
    { perHP :: Int
    , perDmgHandler :: DmgHandler
    }

type Combat = Battle ()
type DmgHandler = Effect -> Combat

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
    { runBattle' :: RWST CombatEnv [Effect] () IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader CombatEnv
             , MonadWriter [Effect]
             )

runBattle :: CombatEnv -> Battle a -> IO (a, [Effect])
runBattle env b = evalRWST (runBattle' b) env ()

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

suggest :: Effect -> Combat
suggest e
    | Just p <- getTarget e =
        flip local (perDmgHandler p e) $
            \r -> r { cenvActor = Just p
                    , cenvDepth = 1 + cenvDepth r
                    }
    | otherwise = return ()

accept :: Effect -> Combat
accept = tell . return

teamHeal :: Int -> Combat
teamHeal dmg = do
    allies <- bAllies
    forM_ allies $ \who -> suggest $ Heal who dmg

attack :: Person -> Int -> Combat
attack target dmg = suggest $ Damage target None dmg

chainAttack :: Int -> Combat
chainAttack dmg = do
    enemies <- bEnemies
    forM_ enemies $ flip attack dmg

exchange :: Combat -> Combat
exchange = local $ \r -> r { cenvTarget   = cenvAttacker r
                           , cenvAttacker = cenvTarget r
                           }

depthGuard :: Int -> Combat -> Combat
depthGuard n b = do
    depth <- asks cenvDepth
    if depth <= n
       then b
       else return ()

thorns :: Effect -> Combat
thorns e@(Damage _ Magic _) = accept e
thorns e@(Damage p _ dmg)   = depthGuard 2 $ do
    accept e
    bAttacker >>= \case
        Just aggro -> exchange . suggest . Damage aggro None $ dmg `div` 2
        Nothing    -> return ()
thorns e = accept e

rewrite :: (Effect -> Battle (Maybe Effect)) -> Combat -> Combat
rewrite f c = do
    env <- ask
    (_, es) <- liftIO $ runBattle env c
    forM_ es $ \e ->
        f e >>= \case
          Just r  -> accept r
          Nothing -> return ()

noise :: Int -> Combat -> Combat
noise max = rewrite noise'
  where
    noise' (Damage t a dmg) = do
        delta <- rand (-max) max
        return . Just . Damage t a $ dmg + delta
    noise' e = return $ Just e


magicMissile :: Combat
magicMissile = do
    num <- rand 3 5
    enemies <- bEnemies
    let len = length enemies - 1
    forM_ [1..num] $ \_ -> do
        badGuy <- (!!) <$> bEnemies <*> rand 0 len
        noise 3 . suggest $ Damage badGuy Magic 8

rand :: Int -> Int -> Battle Int
rand a b = liftIO . getStdRandom $ randomR (a, b)

battle :: Combat
battle = do
    chainAttack 10
    teamHeal 20
    magicMissile


env = CombatEnv
    { cenvTeams = [ [ me ], [ bg1, bg2 ] ]
    , cenvTarget = Just bg1
    , cenvAttacker = Just me
    , cenvActor = Just me
    , cenvDepth = 0
    }
    where me =  Person { perHP = 100, perDmgHandler = accept }
          bg1 = Person { perHP = 201, perDmgHandler = accept }
          bg2 = Person { perHP = 202, perDmgHandler = accept }

main = do
    results <- snd <$> runBattle env battle
    mapM_ (putStrLn . show) $ results

