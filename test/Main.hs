{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadTimer
import Control.Monad.IOSim
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------
-- Implementation, polymorphic on @m@

newtype AtomicCounter m = AtomicCounter (StrictTVar m Int)

newCounter :: (MonadLabelledSTM m) => m (AtomicCounter m)
newCounter = do
    ref <- newTVarIO 0
    atomically $ labelTVar ref "TheCounter"
    return (AtomicCounter ref)

incr :: (MonadSTM m) => AtomicCounter m -> m ()
incr (AtomicCounter ref) = do
    i <- readTVarIO ref
    atomically $ writeTVar ref (i + 1)

get :: (MonadSTM m) => AtomicCounter m -> m Int
get (AtomicCounter ref) = readTVarIO ref

-- Helper for generating random schedules in IOSim
incrWithDelays :: (MonadSTM m, MonadDelay m) => (Int, Int) -> AtomicCounter m -> m ()
incrWithDelays (t1, t2) (AtomicCounter ref) = do
    threadDelay t1
    i <- readTVarIO ref
    threadDelay t2
    atomically $ writeTVar ref (i + 1)

--------------------------------------------------------------------------------
-- Tests

main :: IO ()
main =
    defaultMain $
        testGroup
            "Broken counter"
            [ testProperty "IOSimPOR finds race" $
                exploreSimTrace
                    (\opts -> opts{explorationDebugLevel = 1})
                    testCase'
                    ( \_ tr ->
                        case traceResult False tr of
                            Left err -> counterexample (show err) $ property False
                            Right tw -> tw === 2
                    )
            , testProperty "IOSim eventually finds race" $ \x ->
                2 === runSimOrThrow (testCase x)
            , testProperty "IOSim eventually finds ok case" $ \x ->
                2 =/= runSimOrThrow (testCase x)
            , testProperty "IO eventually finds race" $ monadicIO $ do
                res <- run $ testCase' @IO
                assertWith (res == 2) (show res <> " =/= 2")
            , testProperty "IO eventually finds ok case" $ monadicIO $ do
                res <- run $ testCase' @IO
                assertWith (res /= 2) (show res <> " === 2")
            ]
  where
    testCase ::
        ( MonadAsync m
        , MonadLabelledSTM m
        , MonadDelay m
        ) =>
        ((Int, Int), (Int, Int)) -> m Int
    testCase delays = do
        counter <- newCounter
        _ <-
            concurrently
                (incrWithDelays (fst delays) counter)
                (incrWithDelays (snd delays) counter)
        get counter

    testCase' ::
        ( MonadAsync m
        , MonadTest m
        , MonadLabelledSTM m
        ) =>
        m Int
    testCase' = do
        exploreRaces
        counter <- newCounter
        _ <- concurrently (incr counter) (incr counter)
        get counter
