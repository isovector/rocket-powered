{-# LANGUAGE LambdaCase, Rank2Types, DeriveFunctor, MultiParamTypeClasses #-}

module Main where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Monad.Free
import Control.Monad (ap)
import Data.Traversable
import System.Random

class (Functor f, Functor g) => Pairing f g where
    pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing ((->) a) ((,) a) where
    pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
    pair p f g = pair (flip p) g f

instance Pairing f g => Pairing (Cofree f) (Free g) where
    pair p (a :< _ ) (Pure x)  = p a x
    pair p (_ :< fs) (Free gs) = pair (pair p) fs gs

instance Pairing CoActionF Action where
    pair f (CoActionF c _ _) (Damage x k)    = f (c x) k
    pair f (CoActionF _ c _) (AddStatus x k) = f (c x) k
    pair f (CoActionF _ _ c) (RemStatus x k) = f (c x) k

data Status = None
            | Bleeding
            deriving (Show, Eq, Enum, Read)

data Action n = Damage Int n
              | AddStatus Status n
              | RemStatus Status n
              | Rand Int Int (Int -> n)
              deriving Functor

data CoActionF n = CoActionF
    { damageH   :: Int -> n
    , addStatusH :: Status -> n
    , remStatusH :: Status -> n
    -- , randH :: Int -> Int -> (Int, n)
    }
    deriving Functor

type Combat a = Free Action a

damage d       = liftF $ Damage d ()
addStatus s    = liftF $ AddStatus s ()
remStatus s    = liftF $ RemStatus s ()
rand i j       = liftF $ Rand i j id

type CoAction a = Cofree CoActionF a
mkCoAction :: CoAction (IO ())
mkCoAction = coiter next $ return ()
  where
    next = return CoActionF `ap` coDamage
                            `ap` coAddStatus
                            `ap` coRemStatus
                         -- `ap` coRand

coDamage :: IO () -> Int -> IO ()
coDamage w i = do
    w
    putStrLn $ "damage:\t\t" ++ show i

coAddStatus :: IO () -> Status -> IO ()
coAddStatus w s = do
    w
    putStrLn $ "+status:\t" ++ show s

coRemStatus :: IO () -> Status -> IO ()
coRemStatus w s = do
    w
    putStrLn $ "-status:\t" ++ show s

-- coRand :: IO () -> Int -> Int -> (Int, IO ())
-- coRand w i j = do
--     w
--     result <- getStdRandom $ randomR (i, j)
--     printEffect $ k result


-- make this a cofree comonad when you get the chance
printEffect :: Combat a -> IO a
printEffect (Pure a) = return a
printEffect (Free x)
    | (Damage i n) <- x = do
        putStrLn $ "damage:\t\t" ++ show i
        printEffect n
    | (AddStatus s n) <- x = do
        putStrLn $ "+status:\t" ++ show s
        printEffect n
    | (RemStatus s n) <- x = do
        putStrLn $ "-status:\t" ++ show s
        printEffect n
    | (Rand i j k) <- x = do
        result <- getStdRandom $ randomR (i, j)
        printEffect $ k result

battle :: Combat ()
battle = do
    -- dmg <- rand 8 12
    damage 6
    addStatus Bleeding
    damage 2

runBattle w = pair (\_ b -> b) w battle


-- rand :: Int -> Int -> IO Int
-- rand a b = getStdRandom $ randomR (a, b)

main = printEffect battle
