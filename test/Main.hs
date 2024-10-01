{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadTimer
import Control.Monad.IOSim
import Data.Bifunctor
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

data AtomicCounter s = AtomicCounter (StrictTVar (IOSim s) Int)

newCounter :: IOSim s (AtomicCounter s)
newCounter = do
    ref <- newTVarIO 0
    atomically $ labelTVar ref "TheCounter"
    return (AtomicCounter ref)

incr :: Maybe (Int, Int) -> AtomicCounter s -> IOSim s ()
incr mDelays (AtomicCounter ref) = do
    -- Introduce delays to test with IOSim
    maybe (pure ()) (threadDelay . fst) mDelays
    i <- readTVarIO ref
    -- Introduce delays to test with IOSim
    maybe (pure ()) (threadDelay . snd) mDelays
    atomically $ writeTVar ref (i + 1)

get :: AtomicCounter s -> IOSim s Int
get (AtomicCounter ref) = readTVarIO ref

main :: IO ()
main =
    defaultMain $
        testGroup
            "Broken counter"
            [ testProperty "IOSimPOR finds race" $
                expectFailure $
                    exploreSimTrace
                        (\opts -> opts{explorationDebugLevel = 1})
                        (testCase Nothing Nothing)
                        ( \_ tr ->
                            case traceResult False tr of
                                Left err -> counterexample (show err) $ property False
                                Right tw -> tw === 2
                        )
            , testProperty "IOSim finds race" $ \x ->
                2 === runSimOrThrow (uncurry testCase . bimap Just Just $ x)
            , testProperty "IOSim also finds ok case" $ \x ->
                2 =/= runSimOrThrow (uncurry testCase . bimap Just Just $ x)
            ]
  where
    testCase :: forall s. Maybe (Int, Int) -> Maybe (Int, Int) -> IOSim s Int
    testCase delays1 delays2 = do
        exploreRaces
        counter <- newCounter
        _ <- concurrently (incr delays1 counter) (incr delays2 counter)
        get counter
