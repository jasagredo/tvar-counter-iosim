module Main (main) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTest
import Control.Monad.IOSim
import Test.QuickCheck

data AtomicCounter s = AtomicCounter (StrictTVar (IOSim s) Int)

newCounter :: IOSim s (AtomicCounter s)
newCounter = do
    ref <- newTVarIO 0
    atomically $ labelTVar ref "TheCounter"
    return (AtomicCounter ref)

incr :: AtomicCounter s -> IOSim s ()
incr (AtomicCounter ref) = do
    i <- readTVarIO ref
    atomically $ writeTVar ref (i + 1)

get :: AtomicCounter s -> IOSim s Int
get (AtomicCounter ref) = readTVarIO ref

main :: IO ()
main =
    quickCheck $
        exploreSimTrace
            (\opts -> opts{explorationDebugLevel = 1})
            ( do
                exploreRaces
                counter <- newCounter
                _ <- concurrently (incr counter) (incr counter)
                get counter
            )
            ( \_ tr ->
                case traceResult False tr of
                    Left err -> counterexample (show err) $ property False
                    Right tw -> property $ tw === 2
            )
